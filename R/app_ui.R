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
        menuItem("Data summary", tabName = "summary",
                 icon = icon("dashboard")),
        menuItem("Activity plot", tabName = "activity",
                 icon = icon("sun")),
        menuItem("Map", tabName = "map",
                 icon = icon("map"))
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
                fluidRow(
                  summaryUI("summary")
                )
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
  
 