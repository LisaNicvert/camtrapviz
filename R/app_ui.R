#' UI-generating function
#'
#' @return The UI (HTML code)
#' 
#' @export
ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Camtrapviz"),
    dashboardSidebar(

# Sidebar -----------------------------------------------------------------

      
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

# Data import -------------------------------------------------------------

        
        tabItem(tabName = "dataimport",
                fluidRow(
                  box(h2("Data import"),
                      width = 12,
                      box(width = 12,
                          radioButtons("input_type",
                                       label = h3("Input file"),
                                       choices = list("Load example file" = 1,
                                                      "Upload file" = 2),
                                       inline = TRUE,
                                       selected = 1),
                          conditionalPanel(condition = "input.input_type == 1",
                                           fluidRow(column(4,
                                                           selectInput("example_file", "Dataset",
                                                                       choices = c("mica", "recordTableSample"))),
                                                    column(8,
                                                           style = "margin-top: 25px;", # To align with selectInput
                                                           textOutput("dyntext"))
                                           )
                          ),
                          conditionalPanel(condition = "input.input_type == 2",
                                           fluidRow(column(4,
                                                           fileInput("records_input", "Records table"),
                                                           separator_widget("records")),
                                                    column(8,
                                                           selectInput("spp_col", 
                                                                       "Species",
                                                                       choices = list("col1", "col2", "col3")),
                                                           selectInput("cam_col", 
                                                                       "Camera",
                                                                       choices = list("col1", "col2", "col3"))
                                                    )
                                           ),
                                           br(),
                                           checkboxInput("import_cameras",
                                                         "Import cameras table"),
                                           conditionalPanel(condition = "input.import_cameras",
                                                            fluidRow(column(4,
                                                                            fileInput("cameras_input", "Cameras table"),
                                                                            separator_widget("records")),
                                                                     column(8,
                                                                            selectInput("lat_col", 
                                                                                        "Latitude",
                                                                                        choices = list("col1", "col2", "col3")),
                                                                            selectInput("lon_col", 
                                                                                        "Longitude",
                                                                                        choices = list("col1", "col2", "col3")),
                                                                            selectInput("cam_col_cov", 
                                                                                        "Camera",
                                                                                        choices = list("col1", "col2", "col3")),
                                                                            "Dynamic list of remaining columns"
                                                                     ))
                                           ) # conditional cameras table panel
                          )
                      ), # End nested box #1
                      box(width = 12,
                          h3("File preview"),
                          h4("Records table"),
                          dataTableOutput("records_preview"),
                          h4("Cameras table"),
                          dataTableOutput("cameras_preview")
                      ), # End nested box #2
                  ) # End big box
                )
        ),

# Summary -----------------------------------------------------------------

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
}
  
 