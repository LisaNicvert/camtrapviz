#' UI-generating function
#'
#' @noRd
#' @return The UI (HTML code)
ui <- function() {
  dashboardPage(
    title = "camtrapviz",
    dashboardHeader(
      # title = "camtrapviz"
      title = div(div(img(src='www/logo.png', height = "30px"), # w = h*0.865
                      style = "height: 100%;"),
                  div("camtrapviz", class = "title",
                      style = "width: 100%"),
                  style = "display: flex; align-items: center; justify-content: space-between;")
      ),
    dashboardSidebar(

# Sidebar -----------------------------------------------------------------


      sidebarMenu(
        menuItem("Home", tabName = "home",
                 icon = icon("house")),
        menuItem("Import", tabName = "dataimport",
                 icon = icon("th-list")),
        menuItem("Filter", tabName = "selectdata",
                 icon = icon("filter")),
        menuItem("Overview", tabName = "summary",
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
                      div(div(img(src='www/logo.png', width = "100%"), # w = h*0.865
                                      style = "width: 15%"),  
                          div(h2("Camtrapviz"),
                              htmltools::hr(), 
                              style = "width: 82%"),
                          style = "display: flex; align-items: center; justify-content: space-between;"),
                      homeUI("home")
                  )
                )
        ),

# Import -------------------------------------------------------------


        tabItem(tabName = "dataimport",
                fluidRow(
                  box(width = 12,
                      h2("Import"),
                      htmltools::hr(),
                      importUI("import")
                      )
                )
        ),

# Filter -------------------------------------------------------------

        tabItem(tabName = "selectdata",
                fluidRow(
                  box(width = 12,
                      h2("Filter"),
                      htmltools::hr(),
                      selectUI("select")
                  )
                )
        ),

# Summary -----------------------------------------------------------------

        tabItem(tabName = "summary",
                fluidRow(
                  box(width = 12,
                      h2("Overview"),
                      htmltools::hr(),
                      summaryUI("summary"),
                  )
                )
        ),

# All species -------------------------------------------------------------

        tabItem(tabName = "all",
                fluidRow(
                  box(width = 12,
                      h2("All species"),
                      htmltools::hr(),
                      allspeciesUI("allspecies")
                      )
                )
        ),

# One species -------------------------------------------------------------

        tabItem(tabName = "one",
                fluidRow(
                  box(width = 12,
                      h2("One species"),
                      htmltools::hr(),
                      # Add species sliding list
                      fluidRow(column(width = 12,
                                      uiOutput("species_select")
                                      )
                      ),
                      uiOutput("onespecies"),
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

