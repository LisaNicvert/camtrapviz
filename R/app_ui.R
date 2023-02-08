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

## Import file -------------------------------------------------------------

                      radioButtons("input_type",
                                   label = h3("Input file"),
                                   choices = list("Load example file" = 1,
                                                  "Upload file" = 2),
                                   inline = TRUE,
                                   selected = 1),

### Example files widgets ---------------------------------------------------

                      
                      conditionalPanel(condition = "input.input_type == 1",
                                       fluidRow(column(4,
                                                       selectInput("example_file", "Dataset",
                                                                   choices = c("mica", "recordTableSample"))),
                                                column(8,
                                                       style = "margin-top: 25px;", # To align with selectInput
                                                       textOutput("dyntext"))
                                       )
                      ),

### Upload files widgets ----------------------------------------------------


                      conditionalPanel(condition = "input.input_type == 2",
                                       fluidRow(column(4,
                                                       h5("Records table"),
                                                       shinyFilesButton('records_input', 
                                                                        style = "margin-bottom: 25px",
                                                                        label = 'Choose records table', 
                                                                        title = 'Choose file',
                                                                        multiple = FALSE),
                                                       conditionalPanel(condition = "output.records_extension !== 'json'",
                                                                        separator_widget("records"))
                                                       ),
                                                column(8,
                                                       selectInput("spp_col", 
                                                                   "Species",
                                                                   choices = NULL),
                                                       selectInput("cam_col", 
                                                                   "Camera",
                                                                   choices = NULL),
                                                       radioButtons("datetime_or_timestamp",
                                                                    "Date / time column(s)",
                                                                    choices = c("Date and time" = "date_time",
                                                                                "Timestamp" = "timestamp"), 
                                                                    inline = TRUE),
                                                       conditionalPanel(condition = "input.datetime_or_timestamp == 'date_time'",
                                                                        selectInput("date_col", 
                                                                                    "Date",
                                                                                    choices = NULL),
                                                                        selectInput("time_col", 
                                                                                    "Time",
                                                                                    choices = NULL)),
                                                       conditionalPanel(condition = "input.datetime_or_timestamp == 'timestamp'",
                                                                        selectInput("timestamp_col", 
                                                                                    "Timestamp",
                                                                                    choices = NULL)
                                                                        ),
                                                       conditionalPanel(condition = "!input.import_cameras && output.records_extension !== 'json'",
                                                                        selectInput("lat_col", 
                                                                                    "Latitude",
                                                                                    choices = NULL),
                                                                        selectInput("lon_col", 
                                                                                    "Longitude",
                                                                                    choices = NULL)
                                                                        ),
                                                       selectInput("count_col", 
                                                                   "Count (optional)",
                                                                   choices = NULL),
                                                       selectInput("obs_col", 
                                                                   "Observation type (optional)",
                                                                   choices = NULL)
                                                )
                                       ),
                                       br(),
                                       conditionalPanel(condition = "output.records_extension !== 'json'",
                                                        checkboxInput("import_cameras",
                                                                      "Import cameras table")
                                                        ),
                                       conditionalPanel(condition = "input.import_cameras || output.records_extension === 'json'",
                                                        fluidRow(column(4,
                                                                        conditionalPanel(condition = "input.import_cameras",
                                                                                         fileInput("cameras_input", "Cameras table",
                                                                                                   accept = ".csv"),
                                                                        separator_widget("cameras")),
                                                                        conditionalPanel(condition = "output.records_extension === 'json'",
                                                                                         "Display filename here")
                                                                        ),
                                                                 column(8,
                                                                        selectInput("cam_col_cov", 
                                                                                    "Camera",
                                                                                    choices = NULL),
                                                                        selectInput("lat_col", 
                                                                                    "Latitude",
                                                                                    choices = NULL),
                                                                        selectInput("lon_col", 
                                                                                    "Longitude",
                                                                                    choices = NULL),
                                                                        "Dynamic list of remaining columns"
                                                                 ))
                                       ) # conditional cameras table panel
                      ),

# File previews -----------------------------------------------------------


                      # box(width = 12,
                      tabsetPanel(
                        tabPanel("Raw data preview",
                                 conditionalPanel(condition = "input.input_type == 1 || input.records_input !== 0",
                                                  h4("Records table"),
                                                  dataTableOutput("raw_records"),
                                                  h4("Cameras table"),
                                                  dataTableOutput("raw_cameras")
                                 )
                                 ),
                        tabPanel("Cleaned data preview",
                                 h4("Records table"),
                                 dataTableOutput("records"),
                                 h4("Cameras table"),
                                 dataTableOutput("cameras")
                                 )
                      ) # End tabsetPanel
                      # ), 
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
  
 