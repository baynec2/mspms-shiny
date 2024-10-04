library("DT")
library("shiny")
library("shinydashboardPlus")
library("mspms")
library("shinydashboard")
library("shinyjs")

ui <- dashboardPage(
  dashboardHeader(title = "MSP-MS"),
  dashboardSidebar(
    # Defining the sidebar menu
    sidebarMenu(
      # Making the tabs
      menuItem("About",
        tabName = "about",
        icon = icon("magnifying-glass-chart")
      ),
      menuItem("File Upload",
        tabName = "file_upload",
        icon = icon("file-upload")
      ),
      menuItem("Processed Data",
        tabName = "processed_data",
        icon = icon("database")
      ),
      menuItem("QC", tabName = "qc", icon = icon("check-square")),
      menuItem("Stats", tabName = "stats", icon = icon("star-of-life")),
      menuItem("DataViz", tabName = "viz", icon = icon("chart-simple")),
      menuItem("Report", tabName = "report", icon = icon("file-lines"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Custom CSS to gray out disabled tabs */
        .disabled-tab {
          pointer-events: none;
          opacity: 0.5;
        }
      "))
    ),
    ### About page, put instructions here ##
    tabItems(
      tabItem(
        tabName = "about",
        tags$head(
          # Include custom CSS to center the content and make it look nice
          tags$style(HTML("
        .main-container {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 80vh;
          flex-direction: column;
          text-align: center;
        }
        .logo {
          width: 300px;
          height: auto;
        }
        .instructions {
          font-size: 18px;
          margin-top: 20px;
          line-height: 1.6;
        }
      "))
        ),
        # Main content of the landing page
        div(
          class = "main-container",
          # Logo
          img(src = "mspms_logo.png", class = "logo"),

          # Welcome message
          h2("Welcome to Shiny Interface to the mspms R Package!"),

          # Instructions for use
          div(
            class = "instructions",
            p("Follow the instructions below to use the application:"),
            tags$ol(
              tags$li("Upload your data in the 'File Upload' tab."),
              tags$li("Once data is uploaded, you will be able to access
                          tabs to the left where you can look at QC plots,
                          statistics, data visualizations, and even generate a
                          standardized mspms .html report.")
            ),
            p("For support, please create an issue at https://github.com/baynec2/mspms-shiny")
          )
        )
      ),
      tabItem(
        tabName = "file_upload",
        fluidRow(
          box(
            title = "Upload files",
            # Putting the place to load the files
            fileInput("colData", "Upload colData", accept = ".csv"),
            fileInput("peaks", "Upload PEAKS file", accept = ".csv"),
            fileInput("fragpipe", "Upload Fragpipe file", accept = "tsv"),
            fileInput("proteome_discoverer", "Upload Proteome Discoverer file",
              accept = "tsv"
            ),
            fileInput("peptide_library", "Upload Peptide Library",
              accept = ".csv"
            ),
            sliderInput("nresidues", "N residues to show after cleavage",
              value = 4,
              min = 1,
              max = 14
            )
          )
        )
      ),
      tabItem(
        tabName = "qc",
        fluidRow(
          box(plotOutput("qc_plot_1"), width = 6, height = 10),
          box(plotOutput("qc_plot_2"), width = 6, height = 10)
        )
      ),
      tabItem(
        tabName = "processed_data",
        h1("processed data"),
        fluidRow(
          box(DT::DTOutput("processed_data"), width = 12),
          downloadButton(
            "downloadData",
            label = "Download"
          )
        )
      ),
      tabItem(
        tabName = "stats",
        fluidRow(
          box(
            DT::DTOutput("ttest_results")
          ),
          box(
            selectizeInput("peptide_selector", "Select Peptide(s) to plot",
              choices = NULL,
              multiple = TRUE
            ),
            plotOutput("time_course_plot")
          )
        ),
        fluidRow(
          downloadButton(
            "downloadttest",
            label = "Download t-test results"
          ),
        )
      ),
      tabItem(
        tabName = "viz",
        inputPanel(
          selectInput(
            "plot_type",
            label = "Select Type of Plot to Display",
            choices = c(
              "PCA", "Volcano Plot", "iceLogo",
              "Cleavage Count", "Heatmap"
            ),
            multiple = FALSE
          ),
          numericInput("width", "Width (inches):", 7),
          numericInput("height", "Height (inches):", 5),
          selectInput("format", "Download format:", choices = c("png", "pdf")),
          downloadButton("downloadPlot", "Download Plot")
        ),
        fluidRow(
          uiOutput(outputId = "dynamic_plot_output")
        )
      ),
      tabItem(
        tabName = "report",
        fluidRow(
          downloadButton(
            "download_report",
            label = "Download Report"
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Rendering the about page
  output$about <- renderUI({
    includeHTML("./about2.html")
  })

  ## Initially Hide Tabs ##
  shinyjs::runjs("$('.sidebar-menu a[data-value=\"qc\"]').addClass('disabled-tab');")
  shinyjs::runjs("$('.sidebar-menu a[data-value=\"processed_data\"]').addClass('disabled-tab');")
  shinyjs::runjs("$('.sidebar-menu a[data-value=\"stats\"]').addClass('disabled-tab');")
  shinyjs::runjs("$('.sidebar-menu a[data-value=\"viz\"]').addClass('disabled-tab');")
  shinyjs::runjs("$('.sidebar-menu a[data-value=\"report\"]').addClass('disabled-tab');")

  # Enable tabs and remove the grayed-out class when both files are uploaded
  observe({
    # Check if at least one of the three conditions is true
    if ((!is.null(input$peaks) && !is.null(input$colData)) |
      (!is.null(input$fragpipe) && !is.null(input$colData)) |
      (!is.null(input$proteome_discoverer) && !is.null(input$colData))) {
      # Use a loop to enable all relevant tabs by removing 'disabled-tab' class
      tabs_to_enable <- c("qc", "processed_data", "stats", "viz", "report")
      for (tab in tabs_to_enable) {
        shinyjs::runjs(paste0("$('.sidebar-menu a[data-value=\"", tab, "\"]').removeClass('disabled-tab');"))
      }
    }
  })


  peptide_library <- reactive({
    if (is.null(input$peptide_library$datapath)) {
      mspms::peptide_library
    } else {
      readr::read_csv(input$peptide_library$datapath)
    }
  })

  # observeEvent(input$colData, {
  #   showTab(inputId = "processed_data")
  # })
  # Reading in files for the type of data the user is uploading
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
    }
  })


  # Processing the data normalization data
  processed_qf <- reactive({
    req(prepared_data())
    mspms::process_qf(prepared_data())
  })

  # Converting to a tibble
  mspms_tidy_data <- reactive({
    req(processed_qf())
    mspms::mspms_tidy(processed_qf())
  })


  # Downloading processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mspms_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(mspms_tidy_data(), file)
    }
  )

  output$processed_data <- DT::renderDT(
    mspms_tidy_data(),
    options = list(scrollX = TRUE)
  )


  ### QC plots ###
  output$qc_plot_1 <- renderPlot({
    mspms::plot_qc_check(processed_qf())
  })


  output$qc_plot_2 <- renderPlot({
    req(processed_qf())
    mspms::plot_nd_peptides(processed_qf())
  })

  # Doing the t-tests
  log2fc_t_test_data <- reactive({
    req(processed_qf())
    processed_qf() %>%
      mspms::log2fc_t_test()
  })

  # Determining sig peptides
  sig <- reactive({
    req(log2fc_t_test_data())
    log2fc_t_test_data() %>%
      dplyr::filter(
        p.adj <= 0.05,
        log2fc >= 3
      )
  })

  # Downloading processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ttest_results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(log2fc_t_test_data(), file)
    }
  )

  output$ttest_results <- DT::renderDT(
    log2fc_t_test_data(),
    options = list(scrollX = TRUE)
  )

  peptide_names <- reactive({
    req(mspms_tidy_data())
    mspms_tidy_data() %>%
      dplyr::pull(peptide) %>%
      unique()
  })

  observeEvent(
    peptide_names(),
    {
      updateSelectizeInput(
        inputId = "peptide_selector",
        choices = peptide_names()
      )
    }
  )

  output$time_course_plot <- renderPlot({
    req(input$peptide_selector)
    filtered_data <- mspms_tidy_data() %>%
      dplyr::filter(peptide %in% input$peptide_selector)
    if (nrow(filtered_data) > 0) { # Check if there's data to plot
      p1 <- mspms::plot_time_course(filtered_data) +
        ggplot2::facet_wrap(~peptide)
      p1
    } else {
      ggplot2::ggplot() +
        ggplot2::geom_text(ggplot2::aes(
          x = 1, y = 1,
          label = "No data to display. Please select a peptide."
        )) +
        ggplot2::theme_void()
    }
  })


  ## Data Viz Plots ###
  # Data Viz Plots ###
  output$dynamic_plot_output <- renderUI({
    # Conditionally display either a plotly plot or a base R plot
    if (input$plot_type == "Heatmap") {
      plotly::plotlyOutput("plotly_plot_output") # For plotly objects
    } else {
      plotOutput("plot_output") # For base R plots
    }
  })

  # Setting nice ggplot2 defaults
  ggplot2::theme_set(
    ggplot2::theme_minimal()
  )

  # Render base R plots (PCA, Volcano Plot, iceLogo, Cleavage Count)
  output$plot_output <- renderPlot({
    plot_type <- input$plot_type
    if (plot_type == "PCA") {
      req(mspms_tidy_data()) # Ensure data is available
      mspms_tidy_data() %>%
        mspms::plot_pca()
    } else if (plot_type == "Volcano Plot") {
      req(log2fc_t_test_data()) # Ensure data is available
      log2fc_t_test_data() %>%
        mspms::plot_volcano()
    } else if (plot_type == "iceLogo") {
      req(sig()) # Ensure data is available
      mspms::plot_all_icelogos(sig_cleavage_data = sig())
    } else if (plot_type == "Cleavage Count") {
      req(sig()) # Ensure data is available
      mspms::plot_cleavages_per_pos(sig_cleavage_data = sig())
    }
  })
  
  output$plotly_plot_output <- plotly::renderPlotly({
    req(mspms_tidy_data())
    mspms_tidy_data() %>% 
      plot_heatmap()
  })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$plot_type, "_plot_", Sys.Date(), ".", input$format, sep = "")
    },
    content = function(file) {
      plot_type <- input$plot_type
      if (plot_type == "PCA") {
        p <- mspms_tidy_data() %>%
          mspms::plot_pca()
      } else if (plot_type == "Volcano Plot") {
        p <- log2fc_t_test_data() %>%
          mspms::plot_volcano()
      } else if (plot_type == "iceLogo") {
        p <- mspms::plot_all_icelogos(sig_cleavage_data = sig())
      } else if (plot_type == "Cleavage Count") {
        p <- mspms::plot_cleavages_per_pos(sig_cleavage_data = sig())
      }

      ggplot2::ggsave(file, p,
        device = input$format,
        width = input$width,
        height = input$height
      )
    }
  )

  ### Downloading Report Generated by mspms ###

  # Downloading processed data
  output$download_report <- downloadHandler(
    filename = function() {
      paste("mspms_report", Sys.Date(), ".html", sep = "")
    },
    content = function(file = "report.html") {
      mspms::generate_report(
        prepared_data = prepared_data(),
        peptide_library = peptide_library(),
        output_file = "report.html"
      )
      file.copy("report.html", file)
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)
