library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPage(
  dashboardHeader(title = "MSP-MS"),
  dashboardSidebar(
    # Defining the sidebar menu
    sidebarMenu(
      menuItem("upload filres", tabName = "file_upload"),
      menuItem("Time Course", tabName = "time_course", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "file_upload",
        fluidRow(
          box(
            title = "Upload files",
            fileInput("peaks", "Upload PEAKS file", accept = ".csv")
          )
        )
      ),
      tabItem(
        tabName = "time_course",
        fluidRow(
          box(selectizeInput("peptide_selector", "peptide_selector", choices = NULL,
                             multiple = TRUE)),
          box(plotOutput(outputId="time_course_plot"))
        )
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({req(input$peaks)
    readr::read_csv(input$peaks$datapath)})

  unique_val <- reactive({
    unique(data()$Peptide)
  })

  observeEvent(
    unique_val(), {
    updateSelectizeInput(inputId = "peptide_selector", choices = unique_val())
  })

output$time_course_plot <- renderPlot({
  filtered = data() %>% 
    dplyr::filter(Peptide %in% input$peptide_selector)
  
  filtered %>% 
    ggplot2::ggplot(ggplot2::aes(Peptide,PTM))+
    ggplot2::geom_point()
  
})
}
# Run the application
shinyApp(ui = ui, server = server)
