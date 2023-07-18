
# UI ----------------------------------------------------------------------


selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Select values -----------------------------------------------------------
    fluidRow(
      class = "nomargin",
      column(width = 6,
             class = "nomarginleft",
             select_values(prefix = NS(id, "spp"),
                           item = "species"),
             textOutput(NS(id, "species_list"))
      ),
      column(width = 6,
             class = "nomarginright",
             select_values(prefix = NS(id, "cam"),
                           item = "cameras"),
             textOutput(NS(id, "cameras_list"))
      )
    ),
    br(),
    fluidRow(
      class = "nomargin",
      column(width = 6,
             class = "nomarginleft",
             select_values(prefix = NS(id, "daterange"),
                           item = "pictures",
                           manual_widget = uiOutput(NS(id, "daterange"))),
             textOutput(NS(id, "daterange_text"))
      ),
      column(width = 6,
             class = "nomarginright",
             girafeOutput(NS(id, "plot_preview"))
      )
    ),
    
    # Show code ---------------------------------------------------------------
    column(width = 12,
           class = "nomargin",
           actionButton(NS(id, "code_filter"), 
                        "Show data filtering code", icon("code"),
                        style = "margin-top: 25px; margin-bottom: 15px;")
    )
    
  )
}


# Server ------------------------------------------------------------------


selectServer <- function(id,
                         camtrap_data, 
                         mapping_records,
                         mapping_cameras) {
  moduleServer(
    id,
    function(input, output, session) {

      # Test reactive input -----------------------------------------------------
      
      # stopifnot(is.reactive(camtrap_data))
      stopifnot(is.reactive(mapping_records))
      stopifnot(is.reactive(mapping_cameras))
      

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
      

      # Create daterange widget -------------------------------------------------
      
      col_filter_range <- reactive({
        # Get the column to use to filter the range on
        # if date_col is in records we'll use this
        # Else we'll use timestamp
        if (!is.null(mapping_records()$date_col)) {
          return(mapping_records()$date_col)
        } else {
          return(mapping_records()$timestamp_col)
        }
      })
      
      default_daterange <- reactive({
        # Define default range from records
        range <- range(camtrap_data()$data$observations[[col_filter_range()]])
        range <- as.Date(range)
        range <- c(range[1], range[2] + 1) # Add one day to last picture
        range
      })
      
      output$daterange <- renderUI({
        shiny::dateRangeInput(NS(id, "daterange_select"), 
                              label = "Date range", 
                              start = default_daterange()[1], end = default_daterange()[2],
                              min = default_daterange()[1], max = default_daterange()[2])
      })
      

      
  
      # Get species and cameras -------------------------------------------------
      
      species_df <- reactive({
        camtrapviz::get_species(camtrap_data()$data$observations,
                                spp_col =  spp_col(), obs_col = obs_col())
      })
      
      cameras <- reactive({
        
        cam_cam <- camtrap_data()$data$deployments[[cam_col_cam()]]
        cam_rec <- unique(camtrap_data()$data$observations[[cam_col_rec()]])
        
        cam <- get_cameras(cam_cam, cam_rec)
        
        return(cam)
      })
      

      # Display species and cameras ---------------------------------------------
  
      output$species_list <- renderText({
        species <- species_df()[input$spp_select, ]
  
        if (is.data.frame(species)) {
          species <- species[[spp_col()]]
        }
        
        paste("Selected species:", paste(species, collapse = ", "))
      })
        
      output$cameras_list <- renderText({
        paste("Selected cameras:", paste(input$cam_select, collapse = ", "))
      })
      
      output$daterange_text <- renderText({
        paste("Daterange:", paste(input$daterange_select[1],
                                  input$daterange_select[2], sep = " -- "))
      })
      
      # Update selectInput ------------------------------------------------------
      
      default_species <- reactive({
        if (is.null(obs_col())) {
          # All species selected
          default <- rownames(species_df())
        } else {
          # Get animal species only
          is_animal <- species_df()[[obs_col()]] == "animal"
          default <- rownames(species_df()[is_animal, ])
        }
        default
      })
      
      observe({
        choices <- as.list(rownames(species_df()))
        names(choices) <- species_df()[[spp_col()]]
        shinyWidgets::updatePickerInput(session = session,
                                        "spp_select",
                                        choices = choices,
                                        selected = default_species())
      })
      
      observe({
        shinyWidgets::updatePickerInput(session = session,
                                        "cam_select",
                                        choices = cameras(),
                                        selected = cameras())
      })
      

      # Filter tables -----------------------------------------------------------

      dat_filtered <- metaReactive2({
        # Validate statement
        validate(need(input$daterange_select, "Wait for date range"))
        validate(need(input$cam_select, "Wait for cameras selection"))
        validate(need(input$spp_select, "Wait for species selection"))
        
        
        # Get values to filter on ---
        # Get rows corresponding to selected obs_col
        has_type <- !is.null(obs_col())
        
        if (has_type) {
          # Get all selected values
          all_filter <- species_df()[input$spp_select, ]
          
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
          spp_filter <- species_df()[input$spp_select, ]
        }
        
        # Get rows corresponding to selected cam_col
        cam_filter <- input$cam_select
        
        metaExpr({
          # Define has_type variable
          has_type <- ..(has_type)
          
          "# Get filters ---"
          spp_filter <- ..(spp_filter)
          cam_filter <- ..(cam_filter)
          obs_filter <- ..(obs_filter)
          
          "# Initialize result ---"
          dat_filtered <- ..(camtrap_data())
          # Add rowid for plot (to know which rows have been discarded)
          if (!tibble::has_rownames(camtrap_data()$data$observations)) {
            dat_filtered$data$observations <- dat_filtered$data$observations |> 
              tibble::rowid_to_column()
          }
          "# Filter ---"
          # Filter obs_col
          # Check has type and length != 0, 
          # in case there were only animals selected
          "# Filter species"
          if (has_type) {
            dat_filtered$data$observations <- dat_filtered$data$observations |>
              dplyr::filter(.data[[..(obs_col())]] %in% obs_filter | .data[[..(spp_col())]] %in% spp_filter)
          } else {
            # Filter spp_col
            dat_filtered$data$observations <- dat_filtered$data$observations |>
              dplyr::filter(.data[[..(spp_col())]] %in% spp_filter)
          }
          
          "# Filter cameras"
          dat_filtered$data$observations <- dat_filtered$data$observations |>
            dplyr::filter(.data[[..(cam_col_rec())]] %in% cam_filter)
          dat_filtered$data$deployments <- dat_filtered$data$deployments |>
            dplyr::filter(.data[[..(cam_col_cam())]] %in% cam_filter)
          
          "# Filter daterange"
          daterange_filter <- as.Date(..(as.character(input$daterange_select)))
          dat_filtered$data$observations <- dat_filtered$data$observations |>
            dplyr::filter(dplyr::between(.data[[..(col_filter_range())]], 
                                         daterange_filter[1],
                                         daterange_filter[2])
                          )
          "# Cameras to factor ---"
          cameras_list <- get_cameras(dat_filtered$data$observations[[..(cam_col_rec())]],
                                      dat_filtered$data$deployments[[..(cam_col_cam())]])
          
          dat_filtered$data$observations[[..(cam_col_rec())]] <- factor(dat_filtered$data$observations[[..(cam_col_rec())]],
                                                                        levels = cameras_list)
          dat_filtered$data$deployments[[..(cam_col_cam())]] <- factor(dat_filtered$data$deployments[[..(cam_col_cam())]],
                                                                       levels = cameras_list)
          
          dat_filtered
        }, bindToReturn = TRUE, localize = FALSE)
        
      }, varname = "dat_filtered")
      
      # Print code --------------------------------------------------------------
      
      observeEvent(input$code_filter, {
        code <- expandChain(dat_filtered())
        displayCodeModal(code,
                         title = "Data filtering code")
      })
      

      # Plot preview ------------------------------------------------------------
      output$plot_preview <- renderGirafe({
        # Get plot width and height
        hw <- get_hw(length(cameras()), 
                     default_daterange())
        height <- hw$height
        width <- hw$width
        
        # Initialize plot data
        dfplot <- camtrap_data()$data$observations |> 
          tibble::rowid_to_column()
        
        dffil <- dat_filtered()$data$observations
        
        kept <- dfplot$rowid %in% dffil$rowid
        dfplot$kept <- kept
        
        # # Filter species that are selected
        # if (!is.null(obs_col())) {
        #   # Get all selected values
        #   all_filter <- species_df()[input$spp_select, ]
        #   
        #   # Get species values
        #   spp_filter <- all_filter[all_filter[[obs_col()]] == "animal", ]
        #   spp_filter <- spp_filter[[spp_col()]]
        #   
        #   # Get obs type values
        #   obs_filter <- all_filter[all_filter[[obs_col()]] != "animal", ]
        #   obs_filter <- obs_filter[[obs_col()]]
        # } else {
        #   # Obs filter is NULL
        #   obs_filter <- NULL
        #   # spp filter is all species
        #   spp_filter <- species_df()[input$spp_select, ]
        # }
        # 
        # if(!is.null(obs_col())) { # If some species in the list are in obs_col
        #   # Take obs_col into account
        #   dfplot <- dfplot |> 
        #     mutate(kept = ifelse(.data[[obs_col()]] %in% obs_filter | .data[[spp_col()]] %in% spp_filter,
        #                          TRUE, FALSE))
        # } else {
        #   dfplot <- dfplot |> 
        #     mutate(kept = ifelse(.data[[spp_col()]] %in% spp_filter,
        #                          TRUE, FALSE))
        # }
        # 
        # # Filter date range
        # dfplot <- dfplot |>
        #   mutate(kept = ifelse(dplyr::between(.data[[col_filter_range()]], 
        #                                       input$daterange_select[1],
        #                                       input$daterange_select[2]),
        #                        TRUE, FALSE))
        # 
        # # Filter cameras
        # dfplot <- dfplot |>
        #   mutate(kept = ifelse(.data[[cam_col_rec()]] %in% input$cam_select,
        #                        TRUE, FALSE))
        
        cols <- c("black", "grey")
        names(cols) <- c("TRUE", "FALSE")
      
        gg <- plot_points(dfplot,
                          camera_col = cam_col_rec(),
                          points_col = "kept",
                          timestamp_col = mapping_records()$timestamp_col,
                          time_col = mapping_records()$time_col,
                          date_col = mapping_records()$date_col, 
                          date_limits = as.POSIXct(default_daterange(),
                                                   tz = "UTC"), 
                          cols = cols,
                          interactive = TRUE) +
          ggplot2::geom_vline(xintercept = as.POSIXct(input$daterange_select,
                                                      tz = "UTC"), 
                              linetype = "dashed")
        
        ggiraph::girafe(ggobj = gg, 
                        width_svg = width,
                        height_svg = height)
        
        # gi <- ggiraph::girafe_options(gi,
        #                               opts_zoom(min = 0.5, max = 10),
        #                               opts_hover_inv(css = "opacity:0.3"),
        #                               opts_selection_inv(css = "opacity:0.3"),
        #                               opts_selection(type = "single"),
        #                               opts_hover(css = ""))
      })
      
      # Return values -----------------------------------------------------------
      return(list(camtrap_data = dat_filtered))
    }
  )
}