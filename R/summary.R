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

# Plots -------------------------------------------------------------------

        
        textOutput(NS(id, "sel")),
        fluidRow(
          column(width = 6,
                 h3("Map"),
                 outputCodeButton(leafletOutput(NS(id, "plot_map"),
                                                height = "400px"))
                 ),
          column(width = 6,
                 h3("Camera activity"),
                 outputCodeButton(girafeOutput(NS(id, "plot_occurrences"),
                                               height = "400px"))
          )
        ),

# Tables ------------------------------------------------------------------

        h3("Cameras summary"),
        textOutput(NS(id, "check_cameras_records")),
        textOutput(NS(id, "check_cameras_cameras")),
        dataTableOutput(NS(id, "cameras_table")),
        verbatimTextOutput(NS(id, "code_cameras_table"))
        )
    )
    
}

summaryServer <- function(id, 
                          camtrap_data, 
                          mapping_records,
                          mapping_cameras,
                          crs) {
  moduleServer(id, function(input, output, session) {
    
# Test reactive input -----------------------------------------------------
    stopifnot(is.reactive(camtrap_data))
    stopifnot(is.reactive(mapping_records))
    stopifnot(is.reactive(mapping_cameras))
    stopifnot(is.reactive(crs))
    
# Reactive general values -------------------------------------------------
    ncameras <- reactive({
      nrow(cameras_values())
    })
    
    nspecies <- reactive({
      
      get_nspecies <- function(df, species_col, obs_col = NULL) {
        if (!is.null(obs_col)) {
          # Filter to get only animal species
          species <- species[df[[obs_col]] == "animal"]
        }
        n <- length(unique(species))
        return(n)
      }
      
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
    
    cameras_values <- metaReactive2({
      
      # Create intermediate variables
      df_records <- camtrap_data()$data$observations
      df_cam <- camtrap_data()$data$deployments
      
      mapping_records <- mapping_records()
      mapping_cameras <- mapping_cameras()
      
      metaExpr({
        summarize_cameras(df_records, 
                          cam_col = ..(mapping_records$cam_col),
                          timestamp_col = ..(mapping_records$timestamp_col),
                          date_col = ..(mapping_records$date_col),
                          time_col = ..(mapping_records$time_col),
                          dfcam = df_cam, 
                          cam_col_dfcam = ..(mapping_cameras$cam_col),
                          setup_col = ..(mapping_cameras$setup_col),
                          retrieval_col = ..(mapping_cameras$retrieval_col))
      })
      
    })
    

# Check cameras -----------------------------------------------------------
    
    cameras_status <- reactive({
      get_cameras_not_in(dfrecords = camtrap_data()$data$observations,
                         dfcameras = camtrap_data()$data$deployments,
                         cam_col_records = mapping_records()$cam_col,
                         cam_col_cameras = mapping_cameras()$cam_col)
    })
    
# Infobox values ----------------------------------------------------------
    output$ncameras <- renderText({
      ncameras()
    })
    
    output$nspecies <- renderText({
      nspecies()
    })
    
    output$sampling_length <- renderText({
      total_sampling <- sum(cameras_values()$sampling_length)
      total_sampling
    })
    
    output$daterange <- renderText({
      minmax_date <- daterange()
      minmax_date <- format(minmax_date, "%d %b %Y")
      paste(minmax_date[1], "to", minmax_date[2])
    })
    

# Check cameras message ---------------------------------------------------
    
    output$check_cameras_records <- renderText({
      print_check_cameras(cameras_status()$not_in_records,
                          type = "not_in_records")
    })
    
    output$check_cameras_cameras <- renderText({
      print_check_cameras(cameras_status()$not_in_cameras,
                          type = "not_in_cameras")
    })
    
# Tables ------------------------------------------------------------------
    output$cameras_table <- renderDataTable({
      DT::datatable(cameras_values(),
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE))
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
                 crs = ..(crs()),
                 cam_col = ..(unname(mapping_cameras()$cam_col)),
                 color = "black")
      })
    })
    
    # Girafe observer
    observeEvent(input$plot_occurrences_selected, {
      clicked_point <- input$plot_occurrences_selected
      
      camdf <- camtrap_data()$data$deployments
      lat <- camdf[[mapping_cameras()$lat_col]][camdf[[mapping_cameras()$cam_col]] == clicked_point]
      lon <- camdf[[mapping_cameras()$lon_col]][camdf[[mapping_cameras()$cam_col]] == clicked_point]

      leafletProxy(mapId = "plot_map", session) %>% 
        removeMarker(layerId = clicked_point) %>%
        addCircles(lng = lon,
                   lat = lat,
                   layerId = clicked_point,
                   options = popupOptions(closeButton = FALSE),
                   color = "red")
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
    
  
# Plots code --------------------------------------------------------------
    
    observeEvent(input$plot_map_output_code, {
      code <- expandChain(output$plot_map())
      displayCodeModal(code)
    })
    
    observeEvent(input$plot_occurrences_output_code, {
      code <- expandChain(output$plot_occurrences())
      displayCodeModal(code)
    })

# Tables code -------------------------------------------------------------
    
    output$code_cameras_table <- renderPrint({
      expandChain(cameras_values())
    })
    
  })
}
