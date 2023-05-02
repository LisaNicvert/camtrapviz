# UI ----------------------------------------------------------------------

summaryUI <- function(id) {
  tagList(

# Info box ----------------------------------------------------------------
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

    # textOutput(NS(id, "sel")),
    h3("Map"),
    outputCodeButton(leafletOutput(NS(id, "plot_map"),
                                   height = "500px")),
    h3("Camera activity"),
    outputCodeButton(girafeOutput(NS(id, "plot_occurrences"),
                                  height = "500px")),

# Tables ------------------------------------------------------------------
    h3("Summary tables"),
    tabsetPanel(
      tabPanel("Cameras",
               br(),
               textOutput(NS(id, "check_cameras_records")),
               textOutput(NS(id, "check_cameras_cameras")),
               br(),
               dataTableOutput(NS(id, "cameras_table")),
               actionButton(NS(id, "code_cameras_table"), 
                            "Show camera summary code", icon("code"),
                            style = "margin-top: 15px; margin-bottom: 15px;")
               ),
      tabPanel("Species",
               br(),
               br(),
               dataTableOutput(NS(id, "species_table")),
               actionButton(NS(id, "code_species_table"), 
                            "Show species summary code", icon("code"),
                            style = "margin-top: 15px; margin-bottom: 15px;")
               )
    )
  ) # end tagList
    
}


# Server ------------------------------------------------------------------

summaryServer <- function(id, 
                          camtrap_data, 
                          mapping_records,
                          mapping_cameras,
                          crs) {
  moduleServer(id, function(input, output, session) {
    
# Test reactive input -----------------------------------------------------
    # stopifnot(is.reactive(camtrap_data))
    stopifnot(is.reactive(mapping_records))
    stopifnot(is.reactive(mapping_cameras))
    stopifnot(is.reactive(crs))
    
# Create column names reactives -------------------------------------------
    spp_col <- reactive({
      unname(mapping_records()$spp_col)
    })    
    
    obs_col <- reactive({
      unname(mapping_records()$obs_col)
    })    
    
    cam_col_cam <- reactive({
      unname(mapping_cameras()$cam_col)
    })
    
    cam_col_rec <- reactive({
      unname(mapping_records()$cam_col)
    })
    
# Reactive general values -------------------------------------------------
    
    nspecies <- reactive({
      get_nspecies(df = camtrap_data()$data$observations,
                   species_col = spp_col(),
                   obs_col = obs_col(), 
                   keep_NA = FALSE)
    })

# Summarize cameras -------------------------------------------------------
    
    cameras_values <- metaReactive2({
      
      mapping_records <- mapping_records()
      mapping_cameras <- mapping_cameras()
      
      # Case no info in camera df
      if (is.null(mapping_cameras$setup_col) && is.null(mapping_cameras$retrieval_col)) { 
        # Don't use dfcam in function call
        metaExpr({
          summarize_cameras(..(camtrap_data())$data$observations, 
                            cam_col = ..(mapping_records$cam_col),
                            timestamp_col = ..(mapping_records$timestamp_col),
                            date_col = ..(mapping_records$date_col),
                            time_col = ..(mapping_records$time_col))
        })
      } else {
        metaExpr({
          summarize_cameras(..(camtrap_data())$data$observations, 
                            cam_col = ..(mapping_records$cam_col),
                            timestamp_col = ..(mapping_records$timestamp_col),
                            date_col = ..(mapping_records$date_col),
                            time_col = ..(mapping_records$time_col),
                            dfcam = ..(camtrap_data())$data$deployments, 
                            cam_col_dfcam = ..(mapping_cameras$cam_col),
                            setup_col = ..(mapping_cameras$setup_col),
                            retrieval_col = ..(mapping_cameras$retrieval_col))
        })
      }
    }, varname = "camvalues")
    

# Summarize species -------------------------------------------------------
    
    species_values <- metaReactive({
      dat <- ..(camtrap_data())$data$observations
      ncam <- nrow(..(cameras_values()))
      summarize_species(df = dat, 
                        species_col = ..(spp_col()), 
                        cam_col = ..(cam_col_rec()), 
                        obs_col = ..(obs_col()),
                        count_col = ..(unname(mapping_records()$count_col)),
                        ncam = ncam)
    }, varname = "sppvalues", bindToReturn = TRUE)


# Values extracted from cameras table -------------------------------------

    daterange <- reactive({
      c(min(cameras_values()$setup, na.rm = TRUE), 
        max(cameras_values()$retrieval, na.rm = TRUE))
    })
    
    ncameras <- reactive({
      nrow(cameras_values())
    })
# Check cameras -----------------------------------------------------------
    
    cameras_status <- reactive({
      get_cameras_not_in(dfrecords = camtrap_data()$data$observations,
                         dfcameras = camtrap_data()$data$deployments,
                         cam_col_records = cam_col_rec(),
                         cam_col_cameras = cam_col_cam())
    })
    
# Infobox values ----------------------------------------------------------
    output$ncameras <- renderText({
      ncameras()
    })
    
    output$nspecies <- renderText({
      nspecies()
    })
    
    output$sampling_length <- renderText({
      total_sampling <- sum(cameras_values()$sampling_length,
                            na.rm = TRUE)
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
    output$cameras_table <- metaRender(renderDataTable, {
        DT::datatable(..(cameras_values()),
                      filter = "none",
                      selection = "none",
                      options = list(scrollX = TRUE))
    })
    
    output$species_table <- metaRender(renderDataTable, {
      DT::datatable(..(species_values()),
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE))
    })
    
# Plots -------------------------------------------------------------------
    output$plot_map <- metaRender(renderLeaflet, {
      "# Plot map ---"
      plot_map(..(camtrap_data())$data$deployments, 
               lat_col = ..(unname(mapping_cameras()$lat_col)),
               lon_col = ..(unname(mapping_cameras()$lon_col)),
               crs = ..(crs()),
               cam_col = ..(cam_col_cam()),
               color = "black")
    })
    
    # Girafe observer
    observeEvent(input$plot_occurrences_selected, {
      clicked_point <- input$plot_occurrences_selected
      
      camdf <- camtrap_data()$data$deployments
      lat <- camdf[[mapping_cameras()$lat_col]][camdf[[mapping_cameras()$cam_col]] == clicked_point]
      lon <- camdf[[mapping_cameras()$lon_col]][camdf[[mapping_cameras()$cam_col]] == clicked_point]

      leafletProxy(mapId = "plot_map", session) |> 
        removeMarker(layerId = clicked_point) |>
        addCircles(lng = lon,
                   lat = lat,
                   layerId = clicked_point,
                   options = popupOptions(closeButton = FALSE),
                   color = "red")
    })
    
    # For debugging
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
      
      metaExpr({
        "# Species occurrences plot ---"
        
        "# ggplot plot"
        gg <- plot_points(..(camtrap_data())$data$observations,
                          camera_col = ..(cam_col_rec()),
                          spp_col = ..(spp_col()),
                          timestamp_col = ..(unname(mapping_records()$timestamp_col)),
                          time_col = ..(unname(mapping_records()$time_col)),
                          date_col = ..(unname(mapping_records()$date_col)),
                          cameras_list = levels(..(camtrap_data())$data$observations[[..(cam_col_rec())]]))
        
        "# ggiraph plot (interactive)"
        gi <- ggiraph::girafe(ggobj = gg,
                              width_svg = ..(width),
                              height_svg = ..(height))
        gi <- ggiraph::girafe_options(gi,
                                      opts_zoom(min = 0.5, max = 10),
                                      opts_hover_inv(css = "opacity:0.3"),
                                      opts_selection_inv(css = "opacity:0.3"),
                                      opts_selection(type = "single"),
                                      opts_hover(css = "")
                                      )
        gi
      }, bindToReturn = TRUE)
      
    })
    
  

# Print code --------------------------------------------------------------

## Plots code --------------------------------------------------------------
    
    observeEvent(input$plot_map_output_code, {
      code <- expandChain(output$plot_map())
      displayCodeModal(code)
    })
    
    observeEvent(input$plot_occurrences_output_code, {
      code <- expandChain(output$plot_occurrences())
      displayCodeModal(code)
    })
    

## Summarize cameras code --------------------------------------------------

    observeEvent(input$code_cameras_table, {
      code <- expandChain(cameras_values())
      displayCodeModal(code,
                       title = "Cameras table code")
    })
    
    observeEvent(input$code_species_table, {
      code <- expandChain(species_values())
      displayCodeModal(code,
                       title = "Species table code")
    })
    
# Return values -----------------------------------------------------------

  list(camtable = output$cameras_table,
       spptable = output$species_table,
       plot_map = output$plot_map,
       plot_occurrences = output$plot_occurrences)
    
  })
}
