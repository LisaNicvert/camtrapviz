
# UI ----------------------------------------------------------------------


selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 6,
           h3("Species"),

# Select values -----------------------------------------------------------

           select_values(prefix = NS(id, "spp"),
                         item = "species"),
           textOutput(NS(id, "species_list"))
           ),
    column(width = 6,
           h3("Cameras"),
           select_values(prefix = NS(id, "cam"),
                         item = "cameras"),
           textOutput(NS(id, "cameras_list"))
           ),

# Preview -----------------------------------------------------------------
    column(width = 12,
           h3("Filtered data preview"),
           tabsetPanel(
             tabPanel("Records",
                      br(),
                      column(width = 12,
                             dataTableOutput(NS(id, "rec_filter_preview"))
                             )
                      ),
             tabPanel("Cameras",
                      br(),
                      column(width = 12,
                             dataTableOutput(NS(id, "cam_filter_preview"))
                             )
                      )
             )
           ),

    # Show code ---------------------------------------------------------------
      
    column(width = 12,
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
          
          "# Filter ---"
          # Filter obs_col
          # Check has type and length != 0, 
          # in case there were only animals selected
          if (has_type) {
            dat_filtered$data$observations <- dat_filtered$data$observations |>
              dplyr::filter(.data[[..(obs_col())]] %in% obs_filter | .data[[..(spp_col())]] %in% spp_filter)
          } else {
            # Filter spp_col
            dat_filtered$data$observations <- dat_filtered$data$observations |>
              dplyr::filter(.data[[..(spp_col())]] %in% spp_filter)
          }
          
          # Filter cameras_col
          dat_filtered$data$observations <- dat_filtered$data$observations |>
            dplyr::filter(.data[[..(cam_col_rec())]] %in% cam_filter)
          dat_filtered$data$deployments <- dat_filtered$data$deployments |>
            dplyr::filter(.data[[..(cam_col_cam())]] %in% cam_filter)
          
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
      
      # Data preview ------------------------------------------------------------
      output$rec_filter_preview <- renderDataTable({
        DT::datatable(dat_filtered()$data$observations,
                      filter = "none",
                      selection = "none",
                      options = list(scrollX = TRUE))
        
      })
      
      output$cam_filter_preview <- renderDataTable({
        DT::datatable(dat_filtered()$data$deployments,
                      filter = "none",
                      selection = "none",
                      options = list(scrollX = TRUE))
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