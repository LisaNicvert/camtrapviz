#' Run Shiny app
#' 
#' Runs the Shiny app to visualize camera trap data.
#'
#' @param ... Arguments to pass to shiny::shinyApp besides server and ui
#'
#' @return A working Shiny app
#' @export
run_camtrapviz <- function(...) {
  # Run the application 
  shiny::shinyApp(ui = ui, server = server, ...)
}

#' Run standalone import module
#'
#' @param ... Arguments to pass to shiny::shinyApp besides server and ui
#'
#' @noRd
#' @return A working Shiny app
importApp <- function(...) {
  
  module <- importUI("import")
  
  ui <- create_dashboard(tagList = module,
                         menu_title = "Import data")
  
  server <- function(input, output, session) {
    importServer("import")
  }
  shiny::shinyApp(ui = ui, server = server, ...)  
}