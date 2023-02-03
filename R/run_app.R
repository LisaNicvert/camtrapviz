# library(here)
# source(here("R/app_server.R"))
# source(here("R/app_ui.R"))

#' Run camtrapviz
#' 
#' Runs the Shiny app to vizualise camera trap data
#'
#' @param ... Arguments to pass to shiny::shinyApp
#'
#' @return A working Shiny app
#' @export
run_camtrapviz <- function(...) {
  # Run the application 
  shiny::shinyApp(ui = ui, server = server, ...)
}
