#' Run camtrapviz
#' 
#' Runs the Shiny app to vizualise camera trap data
#'
#' @param ... Arguments to pass to shiny::shinyApp
#'
#' @return A working Shiny app
#' @export
run_camtrapviz <- function(...) {
  
  ui <- dashboardPage(
    dashboardHeader(title = "Camtrap dataviz"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Data import", tabName = "dataimport", 
                 icon = icon("th-list")),
        menuItem("Visualization", tabName = "visualization", 
                 icon = icon("eye-open", lib = "glyphicon"),
                 menuSubItem("Data summary", tabName = "summary",
                             icon = icon("dashboard")),
                 menuSubItem("Activity plot", tabName = "activity",
                             icon = icon("sun")),
                 menuSubItem("Map", tabName = "map",
                             icon = icon("map")) 
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dataimport",
                fluidRow(
                  box(h2("Data import module"),
                      h3("Input file"),
                      fileInput("input", "Choose file"),
                      h3("File overview"),
                      dataTableOutput("tab"),
                      width = 12),
                )
        ),
        tabItem(tabName = "summary",
                fluidRow(h2("Summary tab content"))
        ),
        tabItem(tabName = "activity",
                fluidRow(h2("Activity tab content"))
        ),
        tabItem(tabName = "map",
                fluidRow(h2("Map tab content"))
        )
      )
    )
  )
  
  server <- function(input, output) {
    
    output$tab <- renderDataTable({
      file <- input$input
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      utils::read.csv(file$datapath)
    })
  }
  
  # Run the application 
  shiny::shinyApp(ui = ui, server = server, ...)
}

