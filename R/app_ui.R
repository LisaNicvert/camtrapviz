#' UI-generating function
#'
#' @return The UI (HTML code)
#' 
#' @export
ui <- function() {
  dashboardPage(
    # title = "camtrapviz",
    skin = "black",
    dashboardHeader(
      title = "Camtrapviz"
      # title = box(
      #   tags$p("Camtrapviz"),
      #   tags$img(src='www/photo.png')
      # )
      ),
    dashboardSidebar(

# Sidebar -----------------------------------------------------------------

      
      sidebarMenu(
        menuItem("Data import", tabName = "dataimport", 
                 icon = icon("th-list")),
        menuItem("Select data", tabName = "selectdata",
                 icon = icon("filter")),
        menuItem("Data overview", tabName = "summary",
                 icon = icon("dashboard")),
        menuItem("All species", tabName = "all",
                 icon = icon("dice-five")),
        menuItem("One species", tabName = "one",
                 icon = icon("dice-one"))
      )
    ),
    dashboardBody(
      htmltools::tags$head(
        htmltools::includeCSS("www/theme.css")
        # htmltools::tags$link(rel = "stylesheet",
        #                      type = "text/css", href = here("www/theme.css"))
        
        #   htmltools::tags$style(htmltools::HTML('.control-label{
        #                                             color: blue;
        #                                         }
        #                                         p {
        #                                           color:red;
        #                                         }
        #                                         '
        #                                         )
        #              )
      ),
      tabItems(

# Data import -------------------------------------------------------------

        
        tabItem(tabName = "dataimport",
                fluidRow(
                  box(width = 12,
                      h2("Data import"),
                      importUI("import")
                      )
                )
        ),

# Select data -------------------------------------------------------------

        tabItem(tabName = "selectdata",
                fluidRow(
                  box(width = 12,
                      h2("Select data"),
                      selectUI("select")
                  )
                )
        ),

# Summary -----------------------------------------------------------------

        tabItem(tabName = "summary",
                fluidRow(
                  box(width = 12,
                      h2("Survey summary"),
                      summaryUI("summary"),
                  )
                )
        ),

# All species -------------------------------------------------------------

        tabItem(tabName = "all",
                fluidRow(
                  box(width = 12,
                      h2("All species analyses"),
                      allspeciesUI("allspecies")
                      )
                )
        ),

# One species -------------------------------------------------------------

        tabItem(tabName = "one",
                fluidRow(
                  box(width = 12,
                      h2("One species analyses"),
                      onespeciesUI("onespecies"),
                      # Download
                      column(width = 12,
                             align = "center",
                             style = "margin-top: 25px",
                             downloadButton("download_script", 
                                            "Download script")
                             )
                      )
                )
        )
      ) # End tabItems
    ) # End dashboardBody
  ) # End dashboardPage
}
  
 