#' UI-generating function
#'
#' @noRd
#' @return The UI (HTML code)
ui <- function() {
  dashboardPage(
    dashboardHeader(
      title = "Camtrapviz"),
    dashboardSidebar(

# Sidebar -----------------------------------------------------------------


      sidebarMenu(
        menuItem("Home", tabName = "home",
                 icon = icon("house")),
        menuItem("Data import", tabName = "dataimport",
                 icon = icon("th-list")),
        menuItem("Select data", tabName = "selectdata",
                 icon = icon("filter")),
        menuItem("Data overview", tabName = "summary",
                 icon = icon("dashboard")),
        menuItem("All species", tabName = "all",
                 icon = icon("dice-five")),
        menuItem("One species", tabName = "one",
                 icon = icon("dice-one")),
        menuItem("About", tabName = "about",
                 icon = icon("circle-info"))
      )
    ),

# Body --------------------------------------------------------------------

    dashboardBody(
      htmltools::tags$head(
        css_dep()
      ),
      tabItems(
# Home -------------------------------------------------------------
        
        tabItem(tabName = "home",
                fluidRow(
                  box(width = 12,
                      h2("Home"),
                      htmltools::hr(),
                      homeUI("home")
                  )
                )
        ),

# Data import -------------------------------------------------------------


        tabItem(tabName = "dataimport",
                fluidRow(
                  box(width = 12,
                      h2("Data import"),
                      # h2("Module"),
                      htmltools::hr(),
                      # p("Contenu du module"),
                      # br(),
                      # br(),
                      # br(),
                      # br(),
                      # br()
                      # tags$img(src='images/photo.png',
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
                      # importUI("import"),
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
        ),

# About -------------------------------------------------------------

        tabItem(tabName = "about",
                fluidRow(
                  box(width = 12,
                      h2("About"),
                      htmltools::hr(),
                      aboutUI("about")
                  )
                )
        )
      ) # End tabItems
    ) # End dashboardBody
  ) # End dashboardPage
}

