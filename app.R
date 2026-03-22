library("DT")
library("shiny")
library("bslib")
library("mspms")
library("shinybusy")
library("imputeLCMD") # Keep getting a weird error
# Warning: Error in impute_matrix: requireNamespace("imputeLCMD") is not TRUE
options(expressions = 500000)

# Tabs to hide until data is loaded
data_tabs <- c("Processed Data", "QC", "Stats", "DataViz", "Report")

ui <- page_navbar(
  title = "mspms",
  id = "main_tabs",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 260,
    numericInput(inputId = "log2fc_thresh", "Log2 Fold Change Threshold", 3),
    numericInput(
      inputId = "padj_thresh", "Adjusted P value Threshold",
      0.05, min = 0, max = 1, step = 0.05
    ),
    numericInput(
      inputId = "il_p_thresh", "iceLogo P value Threshold", 0.05,
      min = 0, max = 1, step = 0.05
    ),
    numericInput("width", "Plot Download Width (in):", 7),
    numericInput("height", "Plot Download Height (in):", 5),
    selectInput("format", "Plot Download format:", choices = c("png", "pdf"))
  ),

  ### About page ###
  nav_panel(
    "About",
    icon = icon("magnifying-glass-chart"),
    tags$style(HTML("
      .main-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 75vh;
        flex-direction: column;
        text-align: center;
      }
      .logo { width: 300px; height: auto; }
      .instructions { font-size: 18px; margin-top: 20px; line-height: 1.6; }
    ")),
    div(
      class = "main-container",
      img(src = "mspms_logo.png", class = "logo"),
      h2("Welcome to Shiny Interface to the mspms R Package!"),
      div(
        class = "instructions",
        p("Follow the instructions below to use the application:"),
        p("Upload your data in the 'File Upload' tab."),
        p("Once data is uploaded, you will be able to access additional tabs
          where you can look at QC plots, statistics, data visualizations,
          and even generate a standardized mspms .html report."),
        p("For support, please create an issue at:"),
        tags$a(
          "https://github.com/baynec2/mspms-shiny",
          href = "https://github.com/baynec2/mspms-shiny"
        )
      )
    )
  ),

  ### File Upload ###
  nav_panel(
    "File Upload",
    icon = icon("file-upload"),
    card(
      card_header("Upload Files"),
      fileInput("peptide_library", "Upload Peptide Library (uses standard
                228 library if none selected)", accept = ".csv"),
      fileInput("colData", "Upload colData", accept = ".csv"),
      fileInput("peaks", "Upload PEAKS file", accept = ".csv"),
      fileInput("fragpipe", "Upload Fragpipe file", accept = "tsv"),
      fileInput("proteome_discoverer", "Upload Proteome Discoverer file",
        accept = "tsv"
      ),
      fileInput("sage", "Upload Sage lfq file", accept = "tsv"),
      fileInput("diann", "Upload DIA-NN file", accept = "tsv"),
      sliderInput("nresidues", "N residues to show after cleavage",
        value = 4, min = 1, max = 14
      ),
      actionButton("example_data", "Use built in example data",
        class = "btn-primary"
      )
    )
  ),

  ### QC ###
  nav_panel(
    "QC",
    icon = icon("check-square"),
    add_busy_spinner(spin = "fading-circle"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("QC Plot 1"),
        shinycssloaders::withSpinner(plotOutput("qc_plot_1")),
        downloadButton("download_qc_plot_1", label = "Download QC Plot 1")
      ),
      card(
        card_header("QC Plot 2"),
        shinycssloaders::withSpinner(plotOutput("qc_plot_2")),
        downloadButton("download_qc_plot_2", label = "Download QC Plot 2")
      )
    )
  ),

  ### Processed Data ###
  nav_panel(
    "Processed Data",
    icon = icon("database"),
    card(
      card_header("Processed Data"),
      shinycssloaders::withSpinner(DT::DTOutput("processed_data")),
      downloadButton("downloadpd", label = "Download")
    )
  ),

  ### Stats ###
  nav_panel(
    "Stats",
    icon = icon("star-of-life"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Statistics Results"),
        selectInput(
          "statistics_type",
          label = "Select statistics to perform",
          choices = c("limma", "t-tests")
        ),
        shinycssloaders::withSpinner(DT::DTOutput("stats_results")),
        downloadButton("stats_download", label = "Download statistic results")
      ),
      card(
        card_header("Time Course Plot"),
        selectInput(
          "data_type",
          label = "Select data type to plot",
          choices = c("normalized and imputed", "raw data")
        ),
        selectInput(
          "axis_type",
          label = "Select axis type",
          choices = c("free_y", "fixed"),
          selected = "free_y"
        ),
        shinycssloaders::withSpinner(plotOutput("time_course_plot")),
        downloadButton("download_time_course_plot", label = "Download Plot")
      )
    )
  ),

  ### DataViz ###
  nav_panel(
    "DataViz",
    icon = icon("chart-simple"),
    card(
      card_header("Plot Options"),
      layout_columns(
        col_widths = c(8, 4),
        selectInput(
          "plot_type",
          label = "Select Type of Plot to Display",
          choices = c(
            "PCA", "Heatmap", "Volcano Plot", "iceLogo", "Cleavage Count"
          ),
          multiple = FALSE
        ),
        div(
          style = "padding-top: 25px;",
          downloadButton("downloadPlot", "Download Plot")
        )
      )
    ),
    shinycssloaders::withSpinner(
      uiOutput(outputId = "dynamic_plot_output")
    )
  ),

  ### Report ###
  nav_panel(
    "Report",
    icon = icon("file-lines"),
    card(
      card_header("Generate Report"),
      p("Download a standardized mspms HTML report."),
      downloadButton("download_report", label = "Download Report")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Rendering the about page
  output$about <- renderUI({
    includeHTML("./about2.html")
  })

  ##############################################################################
  # Setting nice ggplot2 defaults
  ##############################################################################
  ggplot2::theme_set(
    ggplot2::theme_bw()
  )
  ##############################################################################
  # Making reproducible (imputation relies on random numbers)
  ##############################################################################
  set.seed(2)
  ##############################################################################
  # Hide data tabs initially, show them once data is loaded
  ##############################################################################
  observe({
    for (tab in data_tabs) {
      nav_hide("main_tabs", tab)
    }
  })

  # Show tabs when data files are uploaded
  observe({
    if ((!is.null(input$peaks) && !is.null(input$colData)) |
      (!is.null(input$fragpipe) && !is.null(input$colData)) |
      (!is.null(input$proteome_discoverer) && !is.null(input$colData)) |
      (!is.null(input$diann) && !is.null(input$colData)) |
      (!is.null(input$sage) && !is.null(input$colData))) {
      for (tab in data_tabs) {
        nav_show("main_tabs", tab)
      }
    }
  })

  ##############################################################################
  # Example Data
  ##############################################################################
  use_example_data <- reactiveVal(FALSE)

  observeEvent(input$example_data, {
    for (tab in data_tabs) {
      nav_show("main_tabs", tab)
    }
    use_example_data(TRUE)
  })

  ##############################################################################
  # Loading the Peptide Library and making nmer calculations based on user input
  ##############################################################################
  peptide_library <- reactive({
    if (is.null(input$peptide_library$datapath)) {
      mspms::peptide_library
    } else {
      readr::read_csv(input$peptide_library$datapath, col_select = 1:3)
    }
  })

  all_possible_nmers <- reactive({
    req(input$nresidues)
    req(peptide_library())
    mspms::calculate_all_cleavages(
      peptide_library_seqs =
        peptide_library()$library_real_sequence,
      n_AA_after_cleavage = input$nresidues
    )
  })

  ##############################################################################
  # pre-processing the data
  ##############################################################################
  prepared_data <- reactive({
    if (!is.null(input$fragpipe$datapath)) {
      mspms::prepare_fragpipe(
        input$fragpipe$datapath,
        input$colData$datapath,
        n_residues = input$nresidues,
        peptide_library = peptide_library()
      )
    } else if (!is.null(input$peaks)) {
      mspms::prepare_peaks(
        input$peaks$datapath,
        input$colData$datapath,
        n_residues = input$nresidues,
        peptide_library = peptide_library()
      )
    } else if (!is.null(input$proteome_discoverer)) {
      mspms::prepare_pd(
        input$proteome_discoverer$datapath,
        input$colData$datapath,
        n_residues = input$nresidues,
        peptide_library = peptide_library()
      )
    } else if (!is.null(input$diann)) {
      mspms::prepare_diann(input$diann$datapath,
        input$colData$datapath,
        n_residues = input$nresidues,
        peptide_library = peptide_library()
      )
    } else if (!is.null(input$sage)) {
      mspms::prepare_sage(input$sage$datapath,
        input$colData$datapath,
        n_residues = input$nresidues,
        peptide_library = peptide_library()
      )
    } else if (use_example_data()) {
      lfq_filepath <- system.file("extdata/peaks_protein-peptides-lfq.csv",
        package = "mspms"
      )

      colData_filepath <- system.file("extdata/colData.csv",
        package = "mspms"
      )

      example_data <- mspms::prepare_peaks(lfq_filepath,
        colData_filepath,
        0.3,
        n_residues = input$nresidues
      )
    }
  })

  ##############################################################################
  # Processing the data
  ##############################################################################
  processed_qf <- reactive({
    req(prepared_data())
    mspms::process_qf(prepared_data())
  })

  mspms_tidy_data <- reactive({
    req(processed_qf())
    mspms::mspms_tidy(processed_qf())
  })

  output$downloadpd <- downloadHandler(
    filename = function() {
      paste("mspms_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(mspms_tidy_data(), file)
    }
  )

  output$processed_data <- DT::renderDT(
    mspms_tidy_data(),
    options = list(scrollX = TRUE)
  )

  ##############################################################################
  # QC Plots
  ##############################################################################
  output$qc_plot_1 <- renderPlot({
    mspms::plot_qc_check(processed_qf()) +
      ggplot2::theme(legend.position = "bottom")
  })

  output$qc_plot_2 <- renderPlot({
    req(processed_qf())
    mspms::plot_nd_peptides(processed_qf())
  })

  output$download_qc_plot_1 <- downloadHandler(
    filename = function() {
      paste("qc_plot1_", Sys.Date(), ".", input$format, sep = "")
    },
    content = function(file) {
      p <- mspms::plot_qc_check(processed_qf()) +
        ggplot2::theme(legend.position = "bottom")
      ggplot2::ggsave(file, p,
        device = input$format,
        width = input$width,
        height = input$height
      )
    }
  )

  output$download_qc_plot_2 <- downloadHandler(
    filename = function() {
      paste("qc_plot2_", Sys.Date(), ".", input$format, sep = "")
    },
    content = function(file) {
      p <- mspms::plot_nd_peptides(processed_qf()) +
        ggplot2::theme(legend.position = "bottom")
      ggplot2::ggsave(file, p,
        device = input$format,
        width = input$width,
        height = input$height
      )
    }
  )

  ##############################################################################
  # Statistics
  ##############################################################################
  stats_data <- reactive({
    req(processed_qf())

    if (input$statistics_type == "limma") {
      mspms::limma_stats(processed_qf())
    } else if (input$statistics_type == "t-tests") {
      processed_qf() %>%
        mspms::log2fc_t_test()
    } else {
      return(NULL)
    }
  })

  sig <- reactive({
    req(stats_data())
    req(input$log2fc_thresh)
    req(input$padj_thresh)

    if (input$log2fc_thresh == 0) {
      stop("log2fc_thresh cannot be 0. Please provide a non-zero value.")
    }

    stats_data() %>%
      dplyr::filter(
        p.adj <= input$padj_thresh,
        if (input$log2fc_thresh > 0) {
          log2fc >= input$log2fc_thresh
        } else {
          log2fc <= input$log2fc_thresh
        }
      )
  })

  output$stats_download <- downloadHandler(
    filename = function() {
      paste(input$statistics_type, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(stats_data(), file)
    }
  )

  output$stats_results <- DT::renderDT(
    stats_data(),
    options = list(scrollX = TRUE)
  )

  ##############################################################################
  # Time course plotting
  ##############################################################################
  data_type <- reactive({
    req(input$data_type)
    if (input$data_type == "normalized and imputed") {
      "peptides_norm"
    } else {
      "peptides"
    }
  })

  filtered_data <- reactive({
    selected_rows <- input$stats_results_rows_selected

    if (length(selected_rows) == 0) {
      return(NULL)
    }

    selected_peptides <- stats_data()$peptide[selected_rows]

    out <- mspms_tidy(processed_qf(), se_name = data_type()) %>%
      dplyr::filter(peptide %in% selected_peptides)

    if (data_type() == "peptides") {
      out <- out %>%
        dplyr::rename(peptides_norm = peptides)
    }

    out
  })

  output$time_course_plot <- renderPlot({
    if (!is.null(filtered_data())) {
      p1 <- mspms::plot_time_course(filtered_data()) +
        ggplot2::facet_wrap(~peptide, scales = input$axis_type) +
        ggplot2::ylab(data_type())
      p1
    } else {
      ggplot2::ggplot() +
        ggplot2::geom_text(ggplot2::aes(
          x = 1, y = 1,
          label = "No data to display. Please select a peptide by clicking the
          rows of interest on the table to the left."
        )) +
        ggplot2::theme_void()
    }
  })

  output$download_time_course_plot <- downloadHandler(
    filename = function() {
      paste("time_course_plot_", Sys.Date(), ".", input$format, sep = "")
    },
    content = function(file) {
      p <- filtered_data() %>%
        mspms::plot_time_course() +
        ggplot2::facet_wrap(~peptide, scales = input$axis_type)

      ggplot2::ggsave(file, p,
        device = input$format,
        width = input$width,
        height = input$height
      )
    }
  )

  ##############################################################################
  # Data Visualization Plot
  ##############################################################################
  output$dynamic_plot_output <- renderUI({
    if (input$plot_type == "Heatmap") {
      tagList(
        card(plotly::plotlyOutput("plotly_plot_output", height = "700px")),
        card(plotOutput("clicked_heatmap_plot", height = "400px"))
      )
    } else {
      plotOutput("plot_output", height = "700px")
    }
  })

  output$plot_output <- renderPlot({
    plot_type <- input$plot_type
    if (plot_type == "PCA") {
      req(mspms_tidy_data())
      mspms_tidy_data() %>%
        mspms::plot_pca()
    } else if (plot_type == "Volcano Plot") {
      req(input$log2fc_thresh)
      req(input$padj_thresh)
      req(stats_data())
      stats_data() %>%
        mspms::plot_volcano(
          log2fc_threshold = input$log2fc_thresh,
          padj_threshold = input$padj_thresh
        ) +
        ggplot2::geom_point(ggplot2::aes(color = peptide_type))
    } else if (plot_type == "iceLogo") {
      req(sig())
      req(input$il_p_thresh)
      mspms::plot_all_icelogos(
        sig_cleavage_data = dplyr::filter(sig(), !is.na(cleavage_seq)),
        pval = input$il_p_thresh,
        background_universe = all_possible_nmers()
      )
    } else if (plot_type == "Cleavage Count") {
      req(sig())
      mspms::plot_cleavages_per_pos(sig_cleavage_data = sig())
    }
  })

  heatmaply_output <- reactive({
    req(mspms_tidy_data())
    mspms_tidy_data() %>%
      plot_heatmap(show_dendrogram = c(FALSE, FALSE))
  })

  output$plotly_plot_output <- plotly::renderPlotly({
    heatmaply_output()
  })

  output$clicked_heatmap_plot <- renderPlot({
    req(heatmaply_output())
    column_order <- heatmaply_output()$x$layout$xaxis$ticktext
    click <- plotly::event_data("plotly_click", source = "A")
    req(click)
    peptide_clicked <- column_order[click$x]
    req(peptide_clicked)
    plot_data <- mspms_tidy_data() %>%
      dplyr::filter(peptide == peptide_clicked)
    mspms::plot_time_course(plot_data)
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$plot_type, "_plot_", Sys.Date(), ".", input$format, sep = "")
    },
    content = function(file) {
      plot_type <- input$plot_type
      if (plot_type == "PCA") {
        req(mspms_tidy_data())
        p <- mspms_tidy_data() %>%
          mspms::plot_pca()
      } else if (plot_type == "Volcano Plot") {
        req(input$log2fc_thresh)
        req(input$padj_thresh)
        req(stats_data())
        p <- stats_data() %>%
          mspms::plot_volcano(
            log2fc_threshold = input$log2fc_thresh,
            padj_threshold = input$padj_thresh
          ) +
          ggplot2::geom_point(ggplot2::aes(color = peptide_type))
      } else if (plot_type == "iceLogo") {
        req(all_possible_nmers())
        req(input$il_p_thresh)
        req(sig())
        p <- mspms::plot_all_icelogos(
          sig_cleavage_data = sig(),
          background_universe = all_possible_nmers(),
          pval = input$il_p_thresh
        )
      } else if (plot_type == "Cleavage Count") {
        req(sig())
        p <- mspms::plot_cleavages_per_pos(sig_cleavage_data = sig())
      }

      ggplot2::ggsave(file, p,
        device = input$format,
        width = input$width,
        height = input$height
      )
    }
  )

  ##############################################################################
  # Generating a standard MSPMS .html report.
  ##############################################################################
  output$download_report <- downloadHandler(
    filename = function() {
      paste("mspms_report", Sys.Date(), ".html", sep = "")
    },
    content = function(file = "report.html") {
      mspms::generate_report(
        prepared_data = prepared_data(),
        peptide_library = peptide_library(),
        n_residues = input$nresidues,
        output_file = "report.html"
      )
      file.copy("report.html", file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
