
# UI ----------------------------------------------------------------------

onespeciesUI <- function(id) {
  tagList(
    h3("Choose species"),
    fluidRow(column(width = 12,
                    selectInput(NS(id, "species"),
                                   label = "Choose species",
                                   choices = NULL)
                    )
             ),
    fluidRow(column(width = 6,
                    h3("Activity plot"),
                    numericInput(NS(id, "k"),
                                 "Number of mixture components",
                                 min = 1, max = 5, value = 3),
                    dataTableOutput(NS(id, "dat")),
                    actionButton(NS(id, "code_test"), 
                                 "Show von Mises code", icon("code"),
                                 style = "margin-top: 15px; margin-bottom: 15px;")
                    ),
             column(width = 6,
                    h3("Presence map")
                    )
             )
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
    
    # Get species df ----------------------------------------------------------
    species_df <- reactive({
      # Validate to wait for filtering
      validate(need(nrow(camtrap_data()$data$observations) != 0, 
                    "Cannot analyze an empty table: plese check data filtering"))
      
      camtrapviz::get_species(camtrap_data()$data$observations,
                              spp_col = spp_col(), obs_col = obs_col())
    })
    
    # Update selectInput ------------------------------------------------------
    
    observe({
      choices <- as.list(rownames(species_df()))
      names(choices) <- species_df()[[spp_col()]]
      updateSelectInput(session = session,
                        "species",
                        choices = choices)
    })
    

# Get filtered data ------------------------------------------------------

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

# Compute density ---------------------------------------------------------
    density <- metaReactive2({
      # Get time
      if (!is.null(timestamp_col())) {
        metaExpr({
          "# Get von Mises density ---"
          datetime <- ..(filtered_records())[[timestamp_col()]]
          times <- format(datetime, format = "%H:%M:%S")
          times <- chron::times(times)

          mod <- fit_vonMises(times, k = ..(input$k))
          vonMises_density(mod, unit = "hour")
        }, bindToReturn = TRUE)

      } else {
        metaExpr({
          "# Get von Mises density ---"
          times <- ..(filtered_records())[[time_col()]]

          mod <- fit_vonMises(times, k = ..(input$k))
          vonMises_density(mod, unit = "hour")
        }, bindToReturn = TRUE)

      }
    }, varname = "density")

    output$dat <- renderDataTable({
      density()
    })
    

# Codes -------------------------------------------------------------------
    
    observeEvent(input$code_test, {
      code <- expandChain(density())
      displayCodeModal(code)
    })
    
    
    
  })
}