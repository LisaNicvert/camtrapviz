# library(here)
# source(here("R/app_server.R"))
# source(here("R/app_ui.R"))

#' Run camtrapviz
#' 
#' Runs the Shiny app to vizualise camera trap data
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
#' @return A working Shiny app
#' @export
importApp <- function(...) {
  
  module <- importUI("import")
  
  ui <- create_dashboard(tagList = module,
                         menu_title = "Import data")
  
  server <- function(input, output, session) {
    importServer("import")
  }
  shiny::shinyApp(ui = ui, server = server, ...)  
}

#' Run standalone summary module
#'
#' @param ... Arguments to pass to shiny::shinyApp besides server and ui
#'
#' @return A working Shiny app
#' @export
summaryApp <- function(...) {
  
  # Create test data
  utils::data(mica, package = "camtraptor")
  mapping_records <- c("spp_col" = "vernacularNames.en",
                       "obs_col" = "observationType",
                       "cam_col" = "deploymentID",
                       "timestamp_col" = "timestamp",
                       "count_col" = "count")
  mapping_cameras <- c("cam_col_cov" = "deploymentID",
                       "lat_col_cov" = "latitude",
                       "lon_col_cov" = "longitude")
  
  # UI
  module <- summaryUI("summary")
  ui <- create_dashboard(tagList = module,
                         menu_title = "Summary")
  
  # Server
  server <- function(input, output, session) {
    summaryServer("summary",
                  camtrap_data = reactive(mica), 
                  mapping_records = reactive(mapping_records),
                  mapping_cameras = reactive(mapping_cameras))
  }
  shiny::shinyApp(ui = ui, server = server, ...)  
}
