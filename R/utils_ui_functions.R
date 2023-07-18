#' Choose separator
#'
#' Function to create a widget to choose a column separator
#' 
#' @param prefix The prefix to use for the widget id (will be "prefix_sep")
#'
#' @noRd
#' @return A checkboxGroupInput with id "prefix_sep" to choose between comma, 
#' tabulation and semicolon for the file separator
separator_widget <- function(prefix) {
  radioButtons(paste(prefix, "sep", sep = "_"),
               label = "File separator",
               choices = c("Comma" = ",", 
                           "Tabulation" = "\t", 
                           "Semicolon" = ";"),
               selected = character(0))
}

#' Create a dashboard
#' 
#' Wrap a tagList inside a dashboard (used for testing modules)
#'
#' @param tagList The tagList
#' @param menu_title The title to display for the tab menu
#' @param dashboard_title Dashboard title
#' 
#' @noRd
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


#' Select values
#' 
#' Function to create a set of widgets used to select
#' values based on a column or based o the values of another column
#'
#' @param prefix the prefix to use for radioButtons and select(ize)Input
#' widgets
#' @param item The item name to display in the widgets labels
#' @param manual_widget Widget to be displayed for manual choice.
#' If not provided, will default to a pickerinput.
#'
#' @noRd
#' @return A taglist of widgets
select_values <- function(prefix, item, 
                          manual_widget = NULL) {
  
  tagList(
    radioButtons(paste(prefix, "manually", sep = "_"), 
                 label = paste("Choose", item), 
                 choices = c("Manually" = "manually",
                             "Based on other column" = "other_col"), 
                 inline = TRUE),
    # Here to access the value of the radioButtons we use another JS way to access a value
    # usually we use input.val but here we use input['val'] because of the modules namespace
    # that will modify prefix -> ns-prefix
    conditionalPanel(condition = paste0("input['", prefix, "_manually'] == 'manually'"), 
                     # Case we choose the item manually
                     column(12,
                            class = "nomargin",
                            # Adjusted div to match default height of Shiny selectizeInput container
                            div(style = "height:63.5px; margin-bottom:15px",
                                if (is.null(manual_widget)) {
                                  shinyWidgets::pickerInput(paste(prefix, "select", sep = "_"),
                                                            paste(gsub("(^[[:alpha:]])", 
                                                                       "\\U\\1", item, perl = TRUE), # capitalize word
                                                                  "list"), 
                                                            multiple = TRUE,
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              `dropup-auto` = FALSE),
                                                            choices = NULL)
                                } else {
                                  manual_widget
                                }
                            )
                            )
    ),
    conditionalPanel(condition = paste0("input['", prefix, "_manually'] == 'other_col'"), 
                     # Case we choose the item based on other column
                     column(width = 6,
                            class = "nomarginleft",
                            selectizeInput(paste(prefix, "col", sep = "_"),
                                           "Choose a column", 
                                           choices = NULL)
                            ),
                     column(width = 6,
                            class = "nomarginright",
                            selectizeInput(paste(prefix, "col_val", sep = "_"),
                                           "Which value(s)?", 
                                           multiple = TRUE,
                                           choices = NULL)
                            )
                     )
  )
}