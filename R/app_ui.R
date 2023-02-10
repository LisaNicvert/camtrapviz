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
                  importUI("import")
                )
        ),

# Summary -----------------------------------------------------------------

        tabItem(tabName = "summary",
                box(width = 12,
                    h2("Summary tab content"),
                    dataTableOutput("test"),
                    textOutput("test2"))
        ),
        tabItem(tabName = "activity",
                box(width = 12, 
                    h2("Activity tab content"))
        ),
        tabItem(tabName = "map",
                box(width = 12,
                    h2("Map tab content"))
        )
      ) # End tabItems
    ) # End dashboardBody
  ) # End dashboardPage
}
  
 