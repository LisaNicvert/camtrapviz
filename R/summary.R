summaryUI <- function(id) {
  tagList(

# Info box ----------------------------------------------------------------

    box(width = 12,
        h2("Survey summary"),
        h3("Overview"),
        fluidRow(infoBox("Cameras", 
                         icon = icon("camera"),
                         color = 'aqua',
                         value = textOutput(NS(id, "ncameras")),
                         width = 6
                         ),
                 infoBox("Species", 
                         icon = icon("paw"),
                         color = 'teal',
                         value = textOutput(NS(id, "nspecies")),
                         width = 6
                         )
                 ),
        fluidRow(infoBox("Trapping nights", 
                         icon = icon("clock"),
                         color = 'fuchsia',
                         value = textOutput(NS(id, "sampling_length")),
                         width = 6
                         ),
                 infoBox("Active", 
                         icon = icon("calendar"),
                         color = 'purple',
                         value = textOutput(NS(id, "daterange")),
                         width = 6
                         )
                 ),
        br(),
        textOutput(NS(id, "sel")),
        fluidRow(
          column(width = 6,
                 h3("Map"),
                 outputCodeButton(leafletOutput(NS(id, "plot_map"),
                                                height = "400px")),
          ),
          column(width = 6,
                 h3("Camera activity"),
                 outputCodeButton(girafeOutput(NS(id, "plot_occurrences"),
                                               height = "400px"))
                 )
        ),
        h3("Species count"),
        fluidRow(
          column(width = 12,
                 outputCodeButton(girafeOutput(NS(id, "plot_species")))
                 )
        )
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
    
# Reactive general values -------------------------------------------------
    ncameras <- reactive({
      nrow(camtrap_data()$data$deployments)
    })
    
    nspecies <- reactive({
      # Get species column
      species_col <- mapping_records()$spp_col
      species <- camtrap_data()$data$observations[[species_col]]
      
      if (!is.null(mapping_records()$obs_col)) {
        # Filter to get only animal species
        obs_col <- mapping_records()$obs_col
        species <- species[camtrap_data()$data$observations[[obs_col]] == "animal"]
      }
      length(unique(species))
    })
    
    daterange <- reactive({
      if (!is.null(mapping_records()$timestamp_col)) {
        # We only have timestamp
        timestamp_col <- mapping_records()$timestamp_col
        date <- camtrap_data()$data$observations[[timestamp_col]]
        date <- as_date(date)
      } else if (!is.null(mapping_records()$date_col)) {
        # We only have date
        date_col <- mapping_records()$date_col
        date <- camtrap_data()$data$observations[[date_col]]
      } else {
        # No date or timestamp
        stop("Date or time must be present in data")
      }
      c(min(date), max(date))
    })
    
    sampling_length <- reactive({
      #### TO IMPROVE
      daterange()[2] - daterange()[1]
    })

# Infobox values ----------------------------------------------------------
    output$ncameras <- renderText({
      ncameras()
    })
    
    output$nspecies <- renderText({
      nspecies()
    })
    
    output$sampling_length <- renderText({
      as.numeric(sampling_length(), "days")
    })
    
    output$daterange <- renderText({
      minmax_date <- daterange()
      minmax_date <- format(minmax_date, "%d %b %Y")
      paste(minmax_date[1], "to", minmax_date[2])
    })
    
# Plots -------------------------------------------------------------------
    output$plot_map <- metaRender2(renderLeaflet, {
      # Create a meaningful name
      import_dat <- camtrap_data()
      
      metaExpr({
        df <- import_dat$data$deployments
        
        plot_map(df, 
                 lat_col = ..(unname(mapping_cameras()$lat_col)),
                 lon_col = ..(unname(mapping_cameras()$lon_col)),
                 cam_col = ..(unname(mapping_cameras()$cam_col)),
                 color = "black")
      })
    })
    
    # Girafe observer
    observeEvent(input$plot_occurrences_selected, {
      clicked_point <- input$plot_occurrences_selected
      cat(clicked_point)
      cat("\n")
      
      camdf <- camtrap_data()$data$deployments
      lat <- camdf[[mapping_cameras()$lat_col]][camdf[[mapping_cameras()$cam_col]] == clicked_point]
      lon <- camdf[[mapping_cameras()$lon_col]][camdf[[mapping_cameras()$cam_col]] == clicked_point]
      cat(paste(lat, lon))
      cat("\n")

      leafletProxy(mapId = "plot_map", session) %>% 
        removeMarker(layerId = clicked_point) %>%
        addCircles(lng = lon,
                   lat = lat,
                   layerId = clicked_point,
                   options = popupOptions(closeButton = FALSE),
                   col = "red")
    })
    
    output$sel <- renderText({
      paste(paste("Leaflet:", paste(input$plot_map_shape_click, collapse = ", ")),
            paste("Girafe:", input$plot_occurrences_selected)
            )
    })

    
    output$plot_occurrences <- metaRender2(renderGirafe, {
      # Define height
      unith <- ncameras()/4
      height <- max(5, 
                    unith/(1 + exp(-12*unith)))
      # Define width
      unitw <- as.numeric(daterange()[2] - daterange()[1], "days")/60 # One inch per 2 months
      width <- max(8,
                   unitw/(1 + exp(-24*unitw)))
      
      # Create a meaningful name
      import_dat <- camtrap_data()
      
      metaExpr({
        "# See  code in import tab to create import_dat"
        df <- import_dat$data$observations
        
        gg <- plot_points(df,
                          camera_col = ..(unname(mapping_records()$cam_col)),
                          spp_col = ..(unname(mapping_records()$spp_col)),
                          timestamp_col = ..(unname(mapping_records()$timestamp_col)),
                          time_col = ..(unname(mapping_records()$time_col)),
                          date_col = ..(unname(mapping_records()$date_col)))
        x <- girafe(ggobj = gg,
                    width_svg = ..(width),
                    height_svg = ..(height))
        x <- girafe_options(x,
                            opts_zoom(min = 0.5, max = 10),
                            opts_hover_inv(css = "opacity:0.3"),
                            opts_selection_inv(css = "opacity:0.3"),
                            opts_selection(type = "single"),
                            opts_hover(css = "")
                            )
        x
      }, bindToReturn = TRUE)
      
    })
    
    output$plot_species <- metaRender2(renderGirafe, {
      # Set height
      unit <- nspecies()/6
      height <- max(5, 
                    unit/(1 + exp(-12*unit)))
      
      # Create a meaningful name
      import_dat <- camtrap_data()
      
      metaExpr({
        "# See code in import tab to create import_dat"
        df <- import_dat$data$observations
        
        gg <- plot_species_bars(df, 
                                spp_col = ..(unname(mapping_records()$spp_col)),
                                obs_col = ..(unname(mapping_records()$obs_col)),
                                count_col = ..(unname(mapping_records()$count_col)))
        
        x <- girafe(ggobj = gg,
                    width_svg = 8,
                    height_svg = ..(height))
        x <- girafe_options(x,
                            opts_zoom(min = 0.5, max = 10))
        x
      })
      
    })
  
# Plots code --------------------------------------------------------------
    
    observeEvent(input$plot_map_output_code, {
      code <- expandChain(output$plot_map())
      displayCodeModal(code)
    })
    
    observeEvent(input$plot_occurrences_output_code, {
      code <- expandChain(output$plot_occurrences())
      displayCodeModal(code)
    })
    
    observeEvent(input$plot_species_output_code, {
      code <- expandChain(output$plot_species())
      displayCodeModal(code)
    })
    
  })
}
