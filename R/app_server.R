
#' Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return The server generating functions for Shiny
#' @export
server <- function(input, output, session) {
  
  # Import reactives
  import_val <- importServer("import")
  
  # Summary reactives
  summaryServer("summary",
                camtrap_data = import_val$camtrap_data, 
                mapping_records = import_val$mapping_records,
                mapping_cameras = import_val$mapping_cameras)
}