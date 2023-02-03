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
  checkboxGroupInput(paste(prefix, "sep", sep = "_"),
                     label = "File separator",
                     choices = c("Comma", "Tabulation", "Semicolon"),
                     inline = TRUE)
}
