summaryUI <- function(id) {
  tagList(

# Info box ----------------------------------------------------------------

    box(width = 12,
        h2("Survey summary"),
        h3("Overview"),
        fluidRow(infoBox("Cameras", 
                         icon = icon("camera"),
                         color = 'aqua',
                         value = 11),
                 infoBox("Species", 
                         icon = icon("paw"),
                         color = 'teal',
                         value = 33)
                 ),
        fluidRow(infoBox("Trapping nights", 
                         icon = icon("clock"),
                         color = 'fuchsia',
                         value = 150),
                 infoBox("Active", 
                         icon = icon("calendar"),
                         color = 'purple',
                         value = paste(Sys.Date(), Sys.Date() + 100, sep = " to "))
                 ),
        br(),
        h3("Camera activity"),
        fluidRow(
          girafeOutput(NS(id, "plot_occurrences"))
        ),
        br(),
        h3("Species count"),
        fluidRow(
          girafeOutput(NS(id, "plot_species"))
        ),
        )
    )
    
}

summaryServer <- function(id, 
                          camtrap_data, 
                          mapping_records,
                          mapping_cameras) {
  moduleServer(id, function(input, output, session) {
    
# Test reactive input -----------------------------------------------------
    stopifnot(is.reactive(camtrap_data))
    stopifnot(is.reactive(mapping_records))
    stopifnot(is.reactive(mapping_cameras))
    
    
    output$plot_occurrences <- renderGirafe({
      df <- camtrap_data()$data$observations

      gg <- plot_points(df,
                        camera_col = mapping_records()["cam_col"],
                        timestamp_col = mapping_records()["timestamp_col"],
                        spp_col = mapping_records()["spp_col"])
      
      x <- girafe(code = print(gg))
      x <- girafe_options(x,
                          opts_zoom(min = 1, max = 10))
      x
    })
    
    output$plot_species <- renderGirafe({
      df <- camtrap_data()$data$observations
      
      if ("obs_col" %in% names(mapping_records())) {
        obs_col <- mapping_records()["obs_col"]
      } else {
        obs_col <- NULL
      }
      
      if ("count_col" %in% names(mapping_records())) {
        count_col <- mapping_records()["count_col"]
      } else {
        count_col <- NULL
      }
      
      gg <- plot_species_bars(df, 
                              spp_col = mapping_records()["spp_col"],
                              obs_col = obs_col,
                              count_col = count_col)
      
      
      girafe(code = print(gg))
    })
    
  })
}