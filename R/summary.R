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
    # stopifnot(is.reactive(camtrap_data))
    # stopifnot(is.reactive(mapping_records))
    # stopifnot(is.reactive(mapping_cameras))
    
    
    output$plot_occurrences <- renderGirafe({
      df <- camtrap_data()$data$observations

      gg <- ggplot(df, aes(x = timestamp, y = deploymentID,
                           col = vernacularNames.en)) +
        geom_point(show.legend = FALSE) +
        ggtitle("Occurrences over time")
      
      girafe(code = print(gg))
    })
    
    output$plot_species <- renderGirafe({
      df <- camtrap_data()$data$observations
      
      gg <- ggplot(df, aes(x = vernacularNames.en)) +
        geom_bar() +
        ggtitle("Species count")
      
      girafe(code = print(gg))
    })
    
  })
}