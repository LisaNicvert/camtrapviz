#' UI-generating function
#'
#' @return The UI (HTML code)
ui <- function() {
  dashboardPage(
    # title = "camtrapviz",
    dashboardHeader(
      title = "Camtrapviz"
      # title = box(
      #   tags$p("Camtrapviz"),
      #   tags$img(src='img/photo.png', height = "20px",width = "20px")
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

# Body --------------------------------------------------------------------

    dashboardBody(
      htmltools::tags$head(
        css_dep()
      ),
      tabItems(

# Data import -------------------------------------------------------------


        tabItem(tabName = "dataimport",
                fluidRow(
                  box(width = 12,
                      h2("Data import"),
                      htmltools::hr(),
                      # tags$img(src='img/photo.png', 
                      #          height = "30px",width = "30px"),
                      importUI("import")
                      )
                )
        ),

# Select data -------------------------------------------------------------

        tabItem(tabName = "selectdata",
                fluidRow(
                  box(width = 12,
                      h2("Select data"),
                      htmltools::hr(),
                      selectUI("select")
                  )
                )
        ),

# Summary -----------------------------------------------------------------

        tabItem(tabName = "summary",
                fluidRow(
                  box(width = 12,
                      h2("Data overview"),
                      htmltools::hr(),
                      summaryUI("summary"),
                  )
                )
        ),

# All species -------------------------------------------------------------

        tabItem(tabName = "all",
                fluidRow(
                  box(width = 12,
                      h2("All species analyses"),
                      htmltools::hr(),
                      allspeciesUI("allspecies")
                      )
                )
        ),

# One species -------------------------------------------------------------

        tabItem(tabName = "one",
                fluidRow(
                  box(width = 12,
                      h2("One species analyses"),
                      htmltools::hr(),
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

