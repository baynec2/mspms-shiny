library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(mspms)
library(dplyr)

ui <- dashboardPage( # skin = "midnight",
  dashboardHeader(title = "MSP-MS"),
  dashboardSidebar(
    # Defining the sidebar menu
    sidebarMenu(
      # Making the tabs
      menuItem("About", tabName = "about", icon = icon("magnifying-glass-chart")),
      menuItem("File Upload", tabName = "file_upload", icon = icon("file-upload")),
      menuItem("Output", tabName = "output", icon = icon("database")),
      menuItem("Stats", tabName = "stats", icon = icon("star-of-life")),
      menuItem("DataViz", tabName = "viz", icon = icon("chart-simple"))
    )
  ),
  dashboardBody(
    ### About page, put instructions here ##
    tabItems(
      tabItem(
        tabName = "about",
        htmlOutput("about")
      ),
      tabItem(
        tabName = "file_upload",
        fluidRow(
          box(
            title = "Upload files",
            # Putting the place to load the files
            fileInput("upload1", "Upload design matrix", accept = ".csv"),
            fileInput("upload2", "Upload PEAKS LFQ file", accept = ".csv"),
            fileInput("upload3", "Upload PEAKS ID file", accept = ".csv"),
            fileInput("upload4", "Upload Proteome Discover", accept = ".xlsx")
          )
        )
        ),
        tabItem(
          tabName = "output",
          h1("normalized data"),
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
            box(DT::DTOutput("ttest_results"), width = 6),
            box(DT::DTOutput("anova_results"), width = 6)
          ),
          downloadButton(
            "downloadttest",
            label = "Download t-test"
          ),
          downloadButton(
            "downloadanova",
            label = "Download anova"
          )
        ),
        tabItem(
          tabName = "viz",
          fluidRow(
            box(plotOutput("PCA"), width = 6),
            box(plotly::plotlyOutput("heatmap"), width = 6)
          ),
          fluidRow(
            box(plotOutput("volcano_plot"), width = 6),
            box(plotOutput("icelogo"), width = 6)
          ),
          fluidRow(
            box(plotOutput("cleavage_count_per_position"), width = 6),
          )
        )
      )
    )
  )

# Define server logic
server <- function(input, output) {
  # Rendering the about page
  output$about <- renderUI({
    includeHTML("./about.html")
  })


  # Reading in the design matrix
  design_matrix <- reactive({
    readr::read_csv(input$upload1$datapath)
  })

  # Reading in files for the type of data the user is uploading

  prepared_data <- reactive({
    if (!is.null(input$upload4$datapath)) {
      mspms::prepare_pd(input$upload4$filepath)
    } else if (!is.null(input$upload2) & !is.null(input$upload3)) {
      mspms::prepare_peaks(
        input$upload2$datapath,
        input$upload3$datapath
      )
    }
  })

  # Processing the data normalization data
  mspms_data <- reactive({
    mspms::mspms(prepared_data(), design_matrix())
  })

  # Downloading processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mspms_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(mspms_data(), file)
    }
  )

  output$processed_data <- DT::renderDT(
    mspms_data(),
    options = list(scrollX = TRUE)
  )


  anova <- reactive({
    mspms_data() %>%
      mspms::mspms_anova()
  })


  # Downloading anova data
  output$downloadanova <- downloadHandler(
    filename = function() {
      paste("anova_results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(anova(), file)
    }
  )

  output$anova_results <- DT::renderDT(
    anova(),
    options = list(scrollX = TRUE)
  )

  # Doing the t-tests

  log2fc_t_test <- reactive({
    mspms_data() %>%
      mspms::log2fc_t_test()
  })


  # Downloading processed data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ttest_results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_csv(log2fc_t_test(), file)
    }
  )

  output$ttest_results <- DT::renderDT(
    log2fc_t_test(),
    options = list(scrollX = TRUE)
  )


  # doing some data visualizations.


  output$PCA <- renderPlot({
    mspms_data() %>%
      mspms::plot_pca()
  })

  output$heatmap <- plotly::renderPlotly({
    mspms_data() %>%
      mspms::plot_heatmap()
  })

  output$volcano_plot <- renderPlot({
    log2fc_t_test() %>%
      mspms::plot_volcano()
  })

  output$icelogo <- renderPlot({
    mspms_data() %>%
      mspms::plot_all_icelogos()
  })


  output$cleavage_count_per_position <- renderPlot({
    log2fc_t_test() %>%
      dplyr::filter(p.adj < 0.05, log2fc > 3) %>%
      mspms::count_cleavages_per_pos() %>%
      mspms::plot_cleavages_per_pos()
  })
}
# Run the application
shinyApp(ui = ui, server = server)
