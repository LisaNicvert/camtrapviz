
# UI ----------------------------------------------------------------------


selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 6,
           h3("Species"),
           select_values(prefix = NS(id, "spp"),
                         item = "species")
           ),
    column(width = 6,
           h3("Cameras"),
           select_values(prefix = NS(id, "cam"),
                         item = "cameras")
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
      
      species <- reactive({
        spp_col <- mapping_records()$spp_col
        obs_col <- mapping_records()$obs_col
        
        spp <- camtrap_data()$data$observations[[spp_col]]
        
        if (!is.null(obs_col)) {
          # Get type
          obs <- camtrap_data()$data$observations[[obs_col]]
          
          # Replace NAs with type
          non_animal <- obs[obs != "animal"]
          spp[obs != "animal"] <- non_animal
        }
        
        sort(unique(spp, NA.last = TRUE), na.last = TRUE)
      })
      
      cameras <- reactive({
        
        cam_cam <- camtrap_data()$data$deployments[[mapping_cameras()$cam_col]]
        cam_rec <- unique(camtrap_data()$data$observations[[mapping_records()$cam_col]])
        
        cam <- get_cameras(cam_cam, cam_rec)
        
        return(cam)
      })
      
# Update selectInput ------------------------------------------------------
      
      observe({
        shinyWidgets::updatePickerInput(session = session,
                                        "spp_select",
                                        choices = species(),
                                        selected = species())
      })
      
      observe({
        shinyWidgets::updatePickerInput(session = session,
                                        "cam_select",
                                        choices = cameras(),
                                        selected = cameras())
      })
      
      
    }
  )
}