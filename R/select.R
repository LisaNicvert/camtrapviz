selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(h2("Select data"),
        width = 12,
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
        ) # End module box
    
  )
}

selectServer <- function(id,
                         camtrap_data, 
                         mapping_records,
                         mapping_cameras) {
  moduleServer(
    id,
    function(input, output, session) {

# Test reactive input -----------------------------------------------------
      
      stopifnot(is.reactive(camtrap_data))
      stopifnot(is.reactive(mapping_records))
      stopifnot(is.reactive(mapping_cameras))
    }
  )
}