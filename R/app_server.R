
#' Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return The server generating functions for Shiny
#' @export
server <- function(input, output, session) {
  
  import_val <- importServer("import")
  
  # Test for output values
  output$test <- renderDataTable({
    DT::datatable(import_val$camtrap_data()$data$deployments)
  })
  
  output$test2 <- renderText({
    paste(import_val$mapping_records())
  })
}