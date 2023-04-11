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
        menuItem("Data overview", tabName = "summary",
                 icon = icon("dashboard")),
        menuItem("All species", tabName = "all",
                 icon = icon("puzzle-piece")),
        menuItem("One species", tabName = "one")
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
                  # verbatimTextOutput("mapping_records"),
                  # verbatimTextOutput("mapping_cameras")
                  summaryUI("summary")
                )
        ),
        tabItem(tabName = "all",
                fluidRow(
                  allspeciesUI("allspecies")
                )
        ),
        tabItem(tabName = "one",
                box(width = 12,
                    h2("One species tab content")),
        ),
        tabItem(tabName = "two",
                box(width = 12,
                    h2("Two species tab content"))
                )
      ) # End tabItems
    ) # End dashboardBody
  ) # End dashboardPage
}
  
 