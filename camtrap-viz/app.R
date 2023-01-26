#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application that draws a histogram
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
    # tags$head( 
    #   tags$style(HTML(".main-sidebar { font-size: 18px; }"))
    # ),
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  # data(iris)
  
  output$tab <- renderDataTable({
    file <- input$input
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
