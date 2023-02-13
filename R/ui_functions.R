#' Choose separator
#'
#' Function to create a widget to choose a column separator
#' 
#' @param prefix The prefix to use for the widget id (will be "prefix_sep")
#'
#' @return A checkboxGroupInput with id "prefix_sep" to choose between comma, 
#' tabulation and semicolon for the file separator
#' 
#' @export
separator_widget <- function(prefix) {
  radioButtons(paste(prefix, "sep", sep = "_"),
               label = "File separator",
               choices = c("Comma" = ",", 
                           "Tabulation" = "\t", 
                           "Semicolon" = ";"),
               selected = character(0),
               inline = TRUE)
}

#' Create a dashboard
#' 
#' Wrap a tagList inside a dashboard (used for testing modules)
#'
#' @param tagList The tagList
#' @param menu_title The title to display for the tab menu
#' @param dashboard_title Dashboard title
#'
#' @return The tagList wrapped in the dashboard
create_dashboard <- function(tagList, 
                             menu_title,
                             dashboard_title = "Camtrapviz") {
  
  dashboardPage(
    dashboardHeader(title = dashboard_title),
    dashboardSidebar(
      sidebarMenu(
        menuItem(menu_title,
                 tabName = "tab")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "tab",
                fluidRow(tagList)
        )
      )
    )
  )
  
}