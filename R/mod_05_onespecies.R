
# UI ----------------------------------------------------------------------

onespeciesUI <- function(id) {
  tagList(

# Choose species ----------------------------------------------------------

    fluidRow(column(width = 12,
                    uiOutput(NS(id, "species_select"))
                    )
             ),
    # Activity plot -----------------------------------------------------------

    h3("Activity plot"),
    fluidRow(column(width = 3,
                    numericInput(NS(id, "adj"),
                                 "Bandwidth adjustment",
                                 min = 1, max = 100, 
                                 step = 0.1,
                                 value = 1)),
             column(width = 9, 
                    outputCodeButton(girafeOutput(NS(id, "density_plot")),
                                     height = "400px")
                    )
             ),

# Species count ---------------------------------------------------------------------
             h3("Species count"),
             fluidRow(
               column(width = 12,
                      conditionalPanel("output.lonlat", 
                                       ns = NS(id),
                                       radioButtons(NS(id, "plot_type"),
                                                    label = "Plot type",
                                                    choices = c("Map" = "map",
                                                                "Barplot" = "bar"))
                      )),
               column(width = 12,
                      conditionalPanel("input.plot_type === 'map' && output.lonlat", 
                                       ns = NS(id),
                                       outputCodeButton(leafletOutput(NS(id, "plot_abundance_map")),
                                                        height = "500px")
                      ),
                      conditionalPanel("input.plot_type === 'bar' || !output.lonlat", 
                                       ns = NS(id),
                                       outputCodeButton(girafeOutput(NS(id, "plot_abundance")),
                                                        height = "500px")
                      )
               )
             )

# Tests -------------------------------------------------------------------
    # dataTableOutput(NS(id, "test"))
             
  )
}


# Server ------------------------------------------------------------------

onespeciesServer <- function(id,
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
    obstype_col <- reactive({
      unname(mapping_records()$obstype_col)
    })
    
    spp_col <- reactive({
      unname(mapping_records()$spp_col)
    })
    
    cam_col_cam <- reactive({
      unname(mapping_cameras()$cam_col)
    })
    
    cam_col_rec <- reactive({
      unname(mapping_records()$cam_col)
    })
    
    
    time_col <- reactive({
      unname(mapping_records()$time_col)
    })
    datetime_col <- reactive({
      unname(mapping_records()$datetime_col)
    })
    
    # Create lonlat reactive --------------------------------------------------
    output$lonlat <- reactive({
      # Return TRUE if lon and lat are provided
      ifelse(!is.null(mapping_cameras()$lat_col) & !is.null(mapping_cameras()$lon_col),
             TRUE, FALSE)
    })
    outputOptions(output, 'lonlat', 
                  suspendWhenHidden = FALSE)
    
    # Species selection -------------------------------------------------------
    
    ## Get species df ----------------------------------------------------------
    species_df <- reactive({
      # Validate to wait for filtering
      validate(need(nrow(camtrap_data()$data$observations) != 0, 
                    "Cannot analyze an empty table: plese check data filtering"))
      
      camtrapviz::get_unique_species(camtrap_data()$data$observations,
                                     spp_col = spp_col(), obstype_col = obstype_col(),
                                     return_df = TRUE)
    })
    
    ## Create selectInput ------------------------------------------------------
    
    output$species_select <- renderUI({
      # Create choices df
      choices <- as.list(rownames(species_df()))
      names(choices) <- species_df()[[spp_col()]]
      
      selectInput(NS(id, "species"),
                  label = "Choose species",
                  choices = choices)
    })


    ## Get filtered data ------------------------------------------------------

    filtered_records <- metaReactive2({
      # Get values to filter on ---
      # Get rows corresponding to selected obstype_col
      has_type <- !is.null(obstype_col())
      
      if (has_type) {
        # Get all selected values
        all_filter <- species_df()[input$species, ]
        
        # Get species values
        spp_filter <- all_filter[all_filter[[obstype_col()]] == "animal", ]
        spp_filter <- spp_filter[[spp_col()]]
        
        # Get obs type values
        obs_filter <- all_filter[all_filter[[obstype_col()]] != "animal", ]
        obs_filter <- obs_filter[[obstype_col()]]
      } else {
        # Obs filter is NULL
        obs_filter <- NULL
        # spp filter is all species
        spp_filter <- species_df()[input$species, ]
      }
      
      metaExpr({
        # Define has_type variable
        has_type <- ..(has_type)
        
        "# Get filters ---"
        spp_filter <- ..(spp_filter)
        obs_filter <- ..(obs_filter)
        
        "# Initialize result ---"
        filtered_records <- ..(camtrap_data())$data$observations
        
        "# Filter ---"
        if (has_type) {
          filtered_records <- filtered_records |>
            dplyr::filter(.data[[..(obstype_col())]] %in% obs_filter | .data[[..(spp_col())]] %in% spp_filter)
        } else {
          # Filter spp_col
          filtered_records <- filtered_records |>
            dplyr::filter(.data[[..(spp_col())]] %in% spp_filter)
        }
        filtered_records
      }, bindToReturn = TRUE)
      
    }, varname = "filtered_records")

    filtered_records_table <- metaReactive({
      DT::datatable(..(filtered_records()),
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE))
    }, bindToReturn = TRUE, varname = "filtered_records_table")
    
    # Density -----------------------------------------------------------------

    
    ## Compute density ---------------------------------------------------------
    density <- metaReactive2({
      
      validate(need(nrow(filtered_records()) != 0, 
                    "Waiting for records data..."))

      # Get time
      if (!is.null(datetime_col())) {
        metaExpr({
          "# Get von Mises density ---"
          datetime <- ..(filtered_records())[[..(datetime_col())]]
          time <- format(datetime, format = "%H:%M:%S")
          time <- chron::times(time)
          
          time_rad <- as.numeric(time)*2*pi
          activity::fitact(time_rad, adj = ..(input$adj))
        }, bindToReturn = TRUE)

      } else {
        metaExpr({
          "# Get von Mises density ---"
          time <- ..(filtered_records())[[..(time_col())]]

          time_rad <- as.numeric(time)*2*pi
          activity::fitact(time_rad, adj = ..(input$adj))
        }, bindToReturn = TRUE)

      }
    }, varname = "density")

    
    ## Density plot ------------------------------------------------------------

    output$density_plot <- metaRender(renderGirafe, {
      "# Plot density ---"
      dfplot <- ..(filtered_records())
      
      # Add times column
      if (!is.null(..(datetime_col()))) {
        datetime <- dfplot[[..(datetime_col())]]
        time <- format(datetime, format = "%H:%M:%S")
        time <- chron::times(time)
        dfplot$time <- time
        time_col <- "time"
      } else {
        time_col <- ..(time_col())
      }
      
      pdf_df <- as.data.frame(..(density())@pdf)
      
      gg <- plot_activity(dffit = pdf_df,
                          time_dffit = "x",
                          y_fit = "y",
                          dfrec = dfplot,
                          time_dfrec = time_col,
                          unit = "clock",
                          freq = TRUE,
                          interactive = TRUE)
      
      gi <- ggiraph::girafe(ggobj = gg)
      gi <- ggiraph::girafe_options(gi,
                                    opts_hover(css = "fill:orange"),
                                    opts_hover_inv(css = "opacity:0.3"))
      gi
    })
    
    observeEvent(input$density_plot_output_code, {
      code <- expandChain(output$density_plot())
      displayCodeModal(code)
    })
    

    # Abundance plot ----------------------------------------------------------- 
    
    abundance_df <- metaReactive({
      "# Get species abundance ---"
      summarize_species(..(filtered_records()),
                        cam_col = ..(cam_col_rec()),
                        spp_col = ..(spp_col()),
                        count_col = ..(mapping_cameras()$count_col), 
                        by_cam = TRUE,
                        keep_all_camera_levels = TRUE)
    }, bindToReturn = TRUE, varname = "abundance_df")
    
    ## Abundance map ----------------------------------------------------------- 
    
    output$plot_abundance_map <- metaRender(renderLeaflet, {
      "# Plot abundance map ---"
      abundance <- ..(abundance_df())$individuals
      names(abundance) <- ..(abundance_df())[[..(cam_col_rec())]]
      
      "# Set hovering labels (replace NA with 'No data')"
      labels <- ifelse(is.na(abundance), 
                       "No data", abundance)
      
      plot_map(..(camtrap_data())$data$deployments, 
               lat_col = ..(unname(mapping_cameras()$lat_col)),
               lon_col = ..(unname(mapping_cameras()$lon_col)),
               crs = ..(crs()),
               cam_col = ..(cam_col_cam()),
               radius = abundance,
               color = "black",
               label = labels,
               rescale = TRUE)
    })
    

    ## Abundance barplot -------------------------------------------------------

    output$plot_abundance <- metaRender2(renderGirafe, {
      hw <- get_hw(nrow(abundance_df()))
      height <- hw$height
      width <- hw$width
      
      metaExpr({
        "# Plot abundance ---"
        "# Replace NA with zero (cameras with no data) to discard warning"
        abundance_df_plot <- ..(abundance_df())
        abundance_df_plot[is.na(abundance_df_plot)] <- 0
        
        gg <- plot_diversity(abundance_df_plot, 
                             div_col = "individuals", 
                             cam_col = ..(cam_col_rec()),
                             interactive = TRUE) +
          ylab("Count") +
          xlab("Cameras")
        
        "# ggiraph plot (interactive)"
        gi <- ggiraph::girafe(ggobj = gg,
                              width_svg = ..(width),
                              height_svg = ..(height))
        gi <- ggiraph::girafe_options(gi,
                                      opts_zoom(min = 0.5, max = 10),
                                      opts_selection(type = "none"))
        gi
      })
      
    })
    
    observeEvent(input$plot_abundance_map_output_code, {
      code <- expandChain(output$plot_abundance_map())
      displayCodeModal(code)
    })
    
    observeEvent(input$plot_abundance_output_code, {
      code <- expandChain(output$plot_abundance())
      displayCodeModal(code)
    })

# Tests -------------------------------------------------------------------
    
    # output$test <- renderDataTable({
    #   filtered_records()
    # })
    

    # Return values -----------------------------------------------------------

    return(list(filtered_records = filtered_records_table,
                density_plot = output$density_plot,
                abundance_map = output$plot_abundance_map))
      
    
  })
}
