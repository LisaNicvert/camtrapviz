
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
    mapping_records()$obs_col
  })
      
  spp_col <- reactive({
    mapping_records()$spp_col
  })
  
  cam_col_cam <- reactive({
    mapping_cameras()$cam_col
  })
  
  cam_col_rec <- reactive({
    mapping_records()$cam_col
  })
  
# Get species and cameras -------------------------------------------------
      
      species_df <- reactive({
        
        if (!is.null(obs_col())) {
          # Get (unique) species ---
          spp_df <- camtrap_data()$data$observations[c(obs_col(), spp_col())] |>
            distinct(.keep_all = TRUE)
          
          # Replace values with not animal ---
          # Get non-animals
          is_non_animal <- spp_df[[obs_col()]] != "animal"
          spp_df[[spp_col()]][is_non_animal] <- spp_df[[obs_col()]][is_non_animal]
          
          # Arrange with non-animal last ---
          # Get factor levels
          levels <- unique(spp_df[[obs_col()]], na.last = TRUE)
          levels <- c("animal", levels[levels != "animal"])
          
          spp_df[[obs_col()]] <- factor(spp_df[[obs_col()]], 
                                      levels = levels)
          spp_df <- spp_df |> dplyr::arrange(spp_df[[obs_col()]],
                                             spp_df[[spp_col()]])
        } else {
          spp_df <- camtrap_data()$data$observations[spp_col()] |>
            distinct(.keep_all = TRUE)
          spp_df <- spp_df |> dplyr::arrange(spp_df[[spp_col()]])
        }
        
        # Add ID
        spp_df <- as.data.frame(spp_df)
        rownames(spp_df) <- paste("ID", 1:nrow(spp_df), sep = "_")
        
        return(spp_df)
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
      species <- species[[spp_col()]]
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

      dat_filtered <- reactive({
        dat_filtered <- camtrap_data()
        
        # Filter species
        selected <- input$spp_select
        
        # Get rows corresponding to selected obs_col
        if (is.null(obs_col())) {
          obs_filter <- species_df()[input$spp_select, ]
          obs_filter <- obs_filter[[obs_col()]]
          
          dat_filtered$data$observations <- dat_filtered$data$observations |>
            dplyr::filter(.data[[obs_col()]] %in% obs_filter)
        }
        
        # Get rows corresponding to selected spp_col
        spp_filter <- species_df()[input$spp_select, ]
        spp_filter <- spp_filter[[spp_col()]]
        
        dat_filtered$data$observations <- dat_filtered$data$observations |>
          dplyr::filter(.data[[spp_col()]] %in% spp_filter)
        
        # Get rows corresponding to cameras_col
        dat_filtered$data$observations <- dat_filtered$data$observations |>
          dplyr::filter(.data[[cam_col_rec()]] %in% input$cam_select)
        dat_filtered$data$deployments <- dat_filtered$data$deployments |>
          dplyr::filter(.data[[cam_col_cam()]] %in% input$cam_select)
        
        
        return(dat_filtered)
      })
      

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
      

# Return values -----------------------------------------------------------
      
      return(list(camtrap_data = dat_filtered))
    }
  )
}