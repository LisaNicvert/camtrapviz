
# UI ----------------------------------------------------------------------


selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 6,
           h3("Species"),
           select_values(prefix = NS(id, "spp"),
                         item = "species"),
           textOutput(NS(id, "species_list"))
           ),
    column(width = 6,
           h3("Cameras"),
           select_values(prefix = NS(id, "cam"),
                         item = "cameras"),
           textOutput(NS(id, "cameras_list"))
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
      

# Get species and cameras -------------------------------------------------
      
      species_df <- reactive({
        # Get column names
        spp_col <- mapping_records()$spp_col
        obs_col <- mapping_records()$obs_col
        
        if (!is.null(obs_col)) {
          # Get (unique) species ---
          spp_df <- camtrap_data()$data$observations[c(obs_col, spp_col)] |>
            distinct(.keep_all = TRUE)
          
          # Replace values with not animal ---
          # Get non-animals
          is_non_animal <- spp_df[[obs_col]] != "animal"
          spp_df[[spp_col]][is_non_animal] <- spp_df[[obs_col]][is_non_animal]
          
          # Arrange with non-animal last ---
          # Get factor levels
          levels <- unique(spp_df[[obs_col]], na.last = TRUE)
          levels <- c("animal", levels[levels != "animal"])
          
          spp_df[[obs_col]] <- factor(spp_df[[obs_col]], 
                                      levels = levels)
          spp_df <- spp_df |> dplyr::arrange(spp_df[[obs_col]],
                                             spp_df[[spp_col]])
        } else {
          spp_df <- camtrap_data()$data$observations[spp_col] |>
            distinct(.keep_all = TRUE)
          spp_df <- spp_df |> dplyr::arrange(spp_df[[spp_col]])
        }
        
        return(spp_df)
      })
      
      cameras <- reactive({
        
        cam_cam <- camtrap_data()$data$deployments[[mapping_cameras()$cam_col]]
        cam_rec <- unique(camtrap_data()$data$observations[[mapping_records()$cam_col]])
        
        cam <- get_cameras(cam_cam, cam_rec)
        
        return(cam)
      })
      

# Display species and cameras ---------------------------------------------

    output$species_list <- renderText({
      paste("Selected species:", paste(input$spp_select, collapse = ", "))
    })
      
    output$cameras_list <- renderText({
      paste("Selected cameras:", paste(input$cam_select, collapse = ", "))
    })
      
# Update selectInput ------------------------------------------------------
      
      default_species <- reactive({
        if (is.null( mapping_records()$obs_col)) {
          # All species selected
          default <- species_df()[[mapping_records()$spp_col]]
        } else {
          default <- species_df()[[mapping_records()$spp_col]][species_df()[[mapping_records()$obs_col]] == "animal"]
        }
      })
      
      observe({
        shinyWidgets::updatePickerInput(session = session,
                                        "spp_select",
                                        choices = species_df()[[mapping_records()$spp_col]],
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
        
      })      
    }
  )
}