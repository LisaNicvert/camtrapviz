
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
                           manual_widget = uiOutput(NS(id, "daterange")))
      ),
      column(width = 6,
             class = "nomarginright",
             girafeOutput(NS(id, "plot_preview"),
                          height = "100%")
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
                         mapping_cameras,
                         tz) {
  moduleServer(
    id,
    function(input, output, session) {
      

      # Helper function ---------------------------------------------------------
      
      #' Update columns
      #' 
      #' Function to update column names to choose from
      #'
      #' @param session session
      #' @param prefix prefix to use to find the widgets
      #' @param df dataframe to pick columns in
      #'
      #' @return Nothing but modifies the existing selectizeInput
      #' @noRd
      update_columns <- function(session,
                                 prefix, 
                                 df) {
          updateSelectizeInput(session = session,
                               paste(prefix, "col", 
                                     sep = "_"),
                               choices = colnames(df))
      }
      
      #' Update column values
      #' 
      #' Function to update values to choose from
      #'
      #' @param session session
      #' @param prefix prefix to use to find the widgets
      #' @param df dataframe to pick columns in
      #'
      #' @return Nothing but modifies the existing selectizeInput
      #' @noRd
      update_column_choices <- function(session, prefix, df) {
        col <- input[[paste(prefix, "col", sep = "_")]]
        choices_col <- sort(unique(df[[col]]), 
                            na.last = TRUE)
        
        updateSelectizeInput(session = session,
                             paste(prefix, "col_val", sep = "_"),
                             choices = choices_col)
      }

      # Test reactive input -----------------------------------------------------
      
      # stopifnot(is.reactive(camtrap_data))
      stopifnot(is.reactive(mapping_records))
      stopifnot(is.reactive(mapping_cameras))
      stopifnot(is.reactive(tz))

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
      
      default_daterange <- reactive({
        # Define default range from records
        if (!is.null(mapping_records()$timestamp_col)) {
          data_dates <- camtrap_data()$data$observations[[mapping_records()$timestamp_col]]
          data_dates <- as.Date(data_dates)
        } else {
          data_dates <- camtrap_data()$data$observations[[mapping_records()$date_col]]
        }

        # Only execute if type is correct
        validate(need("Date" %in% class(data_dates) | "POSIXt" %in% class(data_dates),
                      "A valid date or datetime must be provided."))
        
        range <- range(data_dates)
        range <- as.Date(range)
        range <- c(range[1], range[2] + 1) # Add one day to last picture
        
        return(range)
      })
      
      output$daterange <- renderUI({
        shiny::dateRangeInput(NS(id, "daterange_select"), 
                              label = "Date range", 
                              start = default_daterange()[1], end = default_daterange()[2],
                              min = default_daterange()[1], max = default_daterange()[2])
      })
      


      # Update columns ----------------------------------------------------------
      
      # Species ---
      observe({
        update_columns(session = session, 
                       prefix = "spp", 
                       df = camtrap_data()$data$observations)
      }) |> shiny::bindEvent(camtrap_data()$data$observations)
      
      observe({
        update_column_choices(session = session, 
                              prefix = "spp", 
                              df = camtrap_data()$data$observations)
      }) |> shiny::bindEvent(input$spp_col)
      
      # Cam ---
      observe({
        update_columns(session = session, 
                       prefix = "cam", 
                       df = camtrap_data()$data$deployments)
      }) |> shiny::bindEvent(camtrap_data()$data$deployments)
      
      observe({
        update_column_choices(session = session, 
                              prefix = "cam", 
                              df = camtrap_data()$data$deployments)
      }) |> shiny::bindEvent(input$cam_col)
      
      # Daterange ---
      observe({
        update_columns(session = session, 
                       prefix = "daterange", 
                       df = camtrap_data()$data$observations)
      }) |> shiny::bindEvent(camtrap_data()$data$observations)
      
      observe({
        update_column_choices(session = session, 
                              prefix = "daterange", 
                              df = camtrap_data()$data$observations)
      }) |> shiny::bindEvent(input$daterange_col)

      # Get selected species and cameras ----------------------------------------------------

      selected_spp_id <- reactive({
        if (input$spp_manually == "manually") {
          # If manual selection, just use the input$spp_select values
          res <- input$spp_select
          validate(need(!is.null(res), 
                        "You need to keep at least one species"))
        } else {
          # Filter species corresponding to specified values in
          # another column
          df <- camtrap_data()$data$observations
          
          validate(need(!all(is.null(input$spp_col_val)), 
                        "You need to keep at least one species"))
          
          filtered_df <- df[df[[input$spp_col]] %in% input$spp_col_val, ]
          
          res_df <- get_unique_species(filtered_df,
                                       spp_col =  spp_col(),
                                       obs_col = obs_col(),
                                       reorder = TRUE,
                                       return_df = TRUE)
          res <- res_df$ID
        }
        res
      })
      
      
      selected_cam <- reactive({
        if (input$cam_manually == "manually") {
          res <- input$cam_select
          validate(need(!is.null(res), 
                        "You need to keep at least one camera"))
        } else {
          df <- camtrap_data()$data$deployments
          validate(need(!all(is.null(input$cam_col_val)), 
                        "You need to keep at least one camera"))
          res <- df[df[[input$cam_col]] %in% input$cam_col_val, ]
          res <- res[[cam_col_cam()]]
        }
        res
      })
  
      # Get species and cameras -------------------------------------------------
      
      species_df <- reactive({
        get_unique_species(camtrap_data()$data$observations,
                           spp_col =  spp_col(), obs_col = obs_col(),
                           reorder = TRUE,
                           return_df = TRUE)
      })
      
      cameras <- reactive({
        
        cam_cam <- camtrap_data()$data$deployments[[cam_col_cam()]]
        cam_rec <- unique(camtrap_data()$data$observations[[cam_col_rec()]])
        
        cam <- get_cameras(cam_cam, cam_rec)
        
        return(cam)
      })
      

      # Display species and cameras ---------------------------------------------
  
      output$species_list <- renderText({
        
        # Get df subset where ID is in selected_spp_id()
        species <- species_df()[species_df()$ID %in% selected_spp_id(), ]
        # Get species names
        species <- species[[spp_col()]]
        
        paste("Selected species:", paste(species, collapse = ", "))
      })
        
      output$cameras_list <- renderText({
        paste("Selected cameras:", paste(selected_cam(), collapse = ", "))
      })
      
      # Update selectInput ------------------------------------------------------
      
      default_species <- reactive({
        if (is.null(obs_col())) {
          # All species selected
          default <- species_df()$ID
        } else {
          # Get animal species only
          is_animal <- species_df()[[obs_col()]] == "animal"
          default <- species_df()$ID[is_animal]
        }
        default
      })
      
      observe({
        if (input$spp_manually == "manually") {
          choices <- as.list(species_df()$ID)
          names(choices) <- species_df()[[spp_col()]]
          shinyWidgets::updatePickerInput(session = session,
                                          "spp_select",
                                          choices = choices,
                                          selected = default_species())
        }
      })
      
      observe({
        shinyWidgets::updatePickerInput(session = session,
                                        "cam_select",
                                        choices = cameras(),
                                        selected = cameras())
      })
      

      # Filter tables -----------------------------------------------------------

      dat_filtered <- metaReactive2({
        # Validate ---
        if (input$daterange_manually != "manually") {
          validate(need(!all(is.null(input$daterange_col_val)), 
                        "You need to keep some pictures"))
        }
        
        # Get values to filter on ---
        # Get rows corresponding to selected obs_col
        has_type <- !is.null(obs_col())
        
        if (has_type) {
          # Get species/observations to filter out
          all_filter <- species_df()[!(species_df()$ID %in% selected_spp_id()), ]
          
          # Get species values
          spp_filter <- all_filter[all_filter[[obs_col()]] == "animal", ]
          spp_filter <- spp_filter[[spp_col()]]
          
          # Get obs type values
          obs_filter <- all_filter[all_filter[[obs_col()]] != "animal", ]
          obs_filter <- obs_filter[[obs_col()]]
        } else {
          # Obs filter is NULL
          obs_filter <- NULL
          # Get species to filter out
          spp_filter <- species_df()[!(species_df()$ID %in% selected_spp_id()), ]
          spp_filter <- spp_filter[[spp_col()]]
        }
        
        # Get cameras to filter out
        cam_filter <- cameras()[!cameras() %in% selected_cam()]
        
        # Get date range
        if (input$daterange_manually == "manually") {
          # A date range is provided
          if (is.null(input$daterange_select)) {
            daterange <- default_daterange()
          } else {
            daterange <- input$daterange_select
          }
          custom_col <- NULL
          custom_filter <- NULL
        } else {
          # Custom column and values are provided
          daterange <- NULL
          custom_col <- input$daterange_col
          df <- camtrap_data()$data$observations
          custom_filter <- unique(df[[custom_col]][!(df[[custom_col]] %in% input$daterange_col_val)])
        }
      
        metaExpr({
          "# Get filters ---"
          spp_filter <- ..(spp_filter)
          cam_filter <- ..(cam_filter)
          obs_filter <- ..(obs_filter)
          date_filter <- ..(daterange)
          custom_filter <- ..(custom_filter)
          
          "# Filter ---"
          # Filter obs_col
          # Check has type and length != 0, 
          # in case there were only animals selected
          filter_data(..(camtrap_data()), 
                      spp_col = ..(spp_col()),
                      spp_filter = spp_filter,
                      obs_col = ..(mapping_records()$obs_col),
                      obs_filter = obs_filter,
                      cam_col_rec = ..(cam_col_rec()),
                      cam_col_cam = ..(cam_col_cam()),
                      cam_filter = cam_filter,
                      daterange = date_filter,
                      timestamp_col = ..(mapping_records()$timestamp_col),
                      date_col = ..(mapping_records()$date_col),
                      time_col = ..(mapping_records()$time_col),
                      custom_col = ..(custom_col),
                      custom_filter = custom_filter,
                      cameras_as_factor = TRUE)

        }, bindToReturn = TRUE, localize = FALSE)
        
      }, varname = "dat_filtered")
      
      # Plot preview ------------------------------------------------------------
      output$plot_preview <- renderGirafe({
        
        # Get plot width and height
        hw <- get_hw(length(cameras()), 
                     default_daterange())
        height <- hw$height
        width <- hw$width
        
        # Initialize plot data
        dfplot <- camtrap_data()$data$observations
        
        dffil <- dat_filtered()$data$observations
        
        kept <- rownames(dfplot) %in% rownames(dffil)
        dfplot$kept <- kept
        
        # Add species column 
        dfplot <- dfplot |> 
          mutate(spp_col = get_all_species(dfplot,
                                           spp_col = spp_col(),
                                           obs_col =  mapping_records()$obs_col,
                                           return_df = FALSE))
        
        cols <- c("black", "grey")
        names(cols) <- c("TRUE", "FALSE")
        
        gg <- plot_points(dfplot,
                          camera_col = cam_col_rec(),
                          points_col = "kept",
                          timestamp_col = mapping_records()$timestamp_col,
                          time_col = mapping_records()$time_col,
                          date_col = mapping_records()$date_col, 
                          tooltip_info = "spp_col",
                          date_format = "%d-%b-%Y",
                          date_limits = as.POSIXct(default_daterange(),
                                                   tz = tz()), 
                          cols = cols,
                          tz = tz(),
                          interactive = TRUE)
          
        # Add vertical lines
        if (input$daterange_manually == "manually") {
          gg <- gg + 
            ggplot2::geom_vline(xintercept = as.POSIXct(input$daterange_select,
                                                        tz = tz()), 
                                linetype = "dashed")
        }
        
        gi <- ggiraph::girafe(ggobj = gg, 
                              width_svg = width,
                              height_svg = height)
        
        gi <- ggiraph::girafe_options(gi,
                                      opts_zoom(min = 0.5, max = 10),
                                      opts_hover_inv(css = "opacity:0.3"),
                                      opts_selection_inv(css = "opacity:0.3"),
                                      opts_selection(type = "none"),
                                      opts_hover(css = ""))
      })
      
      # Print code --------------------------------------------------------------
      
      observeEvent(input$code_filter, {
        code <- expandChain(dat_filtered())
        displayCodeModal(code,
                         title = "Data filtering code")
      })
      
      # Return values -----------------------------------------------------------
      return(list(camtrap_data = dat_filtered))
    }
  )
}
