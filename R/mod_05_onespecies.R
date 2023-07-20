
# UI ----------------------------------------------------------------------

onespeciesUI <- function(id) {
  tagList(

# Choose species ----------------------------------------------------------

    fluidRow(column(width = 12,
                    selectInput(NS(id, "species"),
                                   label = "Choose species",
                                   choices = NULL)
                    )
             ),
    # Activity plot -----------------------------------------------------------

    h3("Activity plot"),
    fluidRow(column(width = 3,
                    numericInput(NS(id, "k"),
                                 "Number of mixture components",
                                 min = 1, max = 5, value = 3)),
             column(width = 9, 
                    outputCodeButton(girafeOutput(NS(id, "density_plot")),
                                     height = "400px")
                    )
             ),

# Map ---------------------------------------------------------------------
             h3("Presence map"),
             fluidRow(
               column(width = 12,
                      outputCodeButton(leafletOutput(NS(id, "plot_abundance")),
                                       height = "500px")
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
    obs_col <- reactive({
      unname(mapping_records()$obs_col)
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
    timestamp_col <- reactive({
      unname(mapping_records()$timestamp_col)
    })
    

    # Species selection -------------------------------------------------------
    
    ## Get species df ----------------------------------------------------------
    species_df <- reactive({
      # Validate to wait for filtering
      validate(need(nrow(camtrap_data()$data$observations) != 0, 
                    "Cannot analyze an empty table: plese check data filtering"))
      
      camtrapviz::get_unique_species(camtrap_data()$data$observations,
                              spp_col = spp_col(), obs_col = obs_col())
    })
    
    ## Update selectInput ------------------------------------------------------
    
    observe({
      choices <- as.list(rownames(species_df()))
      names(choices) <- species_df()[[spp_col()]]
      updateSelectInput(session = session,
                        "species",
                        choices = choices)
    })


    ## Get filtered data ------------------------------------------------------

    filtered_records <- metaReactive2({
      # Get values to filter on ---
      # Get rows corresponding to selected obs_col
      has_type <- !is.null(obs_col())
      
      if (has_type) {
        # Get all selected values
        all_filter <- species_df()[input$species, ]
        
        # Get species values
        spp_filter <- all_filter[all_filter[[obs_col()]] == "animal", ]
        spp_filter <- spp_filter[[spp_col()]]
        
        # Get obs type values
        obs_filter <- all_filter[all_filter[[obs_col()]] != "animal", ]
        obs_filter <- obs_filter[[obs_col()]]
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
            dplyr::filter(.data[[..(obs_col())]] %in% obs_filter | .data[[..(spp_col())]] %in% spp_filter)
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
      # Get time
      if (!is.null(timestamp_col())) {
        metaExpr({
          "# Get von Mises density ---"
          datetime <- ..(filtered_records())[[..(timestamp_col())]]
          times <- format(datetime, format = "%H:%M:%S")
          times <- chron::times(times)

          mod <- fit_vonMises(times, k = ..(input$k))
          vonMises_density(mod, unit = "hour")
        }, bindToReturn = TRUE)

      } else {
        metaExpr({
          "# Get von Mises density ---"
          times <- ..(filtered_records())[[..(time_col())]]

          mod <- fit_vonMises(times, k = ..(input$k))
          vonMises_density(mod, unit = "hour")
        }, bindToReturn = TRUE)

      }
    }, varname = "density")

    
    ## Density plot ------------------------------------------------------------

    output$density_plot <- metaRender(renderGirafe, {
      "# Plot density ---"
      dfplot <- ..(filtered_records())
      
      if (!is.null(..(timestamp_col()))) {
        datetime <- dfplot[[..(timestamp_col())]]
        times <- format(datetime, format = "%H:%M:%S")
        times <- chron::times(times)
      } else {
        times <- dfplot[[..(time_col())]]
      }
      
      # Select only species and time for dfplot
      dfplot <- dfplot |> 
        dplyr::select(.data[[..(spp_col())]]) |> 
        dplyr::mutate(time = as.numeric(times)*24)
      
      gg <- ggplot(..(density())) +
        geom_histogram_interactive(data = dfplot, 
                       aes(x = time,
                           y = after_stat(density),
                           tooltip = paste0("Density: ", round(after_stat(density), 3), "\n",
                                            "Time: ", after_stat(x)),
                           data_id = after_stat(x)),
                       binwidth = 1,
                       alpha = 0.7) +
        geom_line(aes(x = x, y = density)) +
        scale_x_continuous(breaks = seq(0, 24, by = 4)) +
        xlab("Time (hours)") +
        ylab("Density") +
        theme_linedraw()
      
      gi <- ggiraph::girafe(ggobj = gg)
      gi <- ggiraph::girafe_options(gi,
                                    opts_hover(css = "fill:orange"))
      gi
    })
    
    observeEvent(input$density_plot_output_code, {
      code <- expandChain(output$density_plot())
      displayCodeModal(code)
    })
    

    # Abundance map ----------------------------------------------------------- 
    
    output$plot_abundance <- metaRender(renderLeaflet, {
      "# Plot map ---"
      
      "# Get species abundance"
      abundance_df <- get_diversity_table(..(filtered_records()),
                                          cam_col = ..(cam_col_rec()),
                                          spp_col = ..(spp_col()),
                                          count_col = ..(mapping_cameras()$count_col), 
                                          keep_all_levels = TRUE)
      abundance <- abundance_df$count
      names(abundance) <- abundance_df[[..(cam_col_rec())]]
      
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
                abundance_map = output$plot_abundance))
      
    
  })
}
