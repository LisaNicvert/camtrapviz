allspeciesUI <- function(id) {
  tagList(
    box(width = 12,
        
# Diversity map -----------------------------------------------------------
      h2("All species analyses"),
      h3("Diversity map"),
      fluidRow(
        column(width = 4,
               selectInput(NS(id, "divtype"), 
                           label = "Diversity index",
                           choices = c("Number of species (richness)",
                                       "Shannon diversity",
                                       "Simpson diversity"))
        ),
        column(width = 8,
               # outputCodeButton(leafletOutput(NS(id, "plot_map"),
               #                                height = "400px"))
        )
      ),
      
# Counts per species ------------------------------------------------------
      h3("Species count"),
      fluidRow(
        column(width = 4,
               selectInput(NS(id, "facet"), 
                           label = "Facetting variable",
                           choices = NULL)
        ),
        column(width = 8,
               outputCodeButton(girafeOutput(NS(id, "plot_species")))
        )
      )
    )
  )

}
allspeciesServer <- function(id,
                             camtrap_data, 
                             mapping_records,
                             mapping_cameras,
                             crs) {
  moduleServer(id, function(input, output, session) {
    
# Test reactive input -----------------------------------------------------
    stopifnot(is.reactive(camtrap_data))
    stopifnot(is.reactive(mapping_records))
    stopifnot(is.reactive(mapping_cameras))
    stopifnot(is.reactive(crs))

# Plot species ------------------------------------------------------------

    output$plot_species <- metaRender2(renderGirafe, {
      # Set height
      # unit <- nspecies()/6
      unit <- 12
      height <- max(5, 
                    unit/(1 + exp(-12*unit)))
      
      # Create a meaningful name
      import_dat <- camtrap_data()
      
      metaExpr({
        "# See code in import tab to create import_dat"
        df <- import_dat$data$observations
        
        gg <- plot_species_bars(df, 
                                spp_col = ..(unname(mapping_records()$spp_col)),
                                obs_col = ..(unname(mapping_records()$obs_col)),
                                count_col = ..(unname(mapping_records()$count_col)))
        
        x <- girafe(ggobj = gg,
                    width_svg = 8,
                    height_svg = ..(height))
        x <- girafe_options(x,
                            opts_zoom(min = 0.5, max = 10))
        x
      })
      
    })
    

# Plots code --------------------------------------------------------------
    
    observeEvent(input$plot_species_output_code, {
      code <- expandChain(output$plot_species())
      displayCodeModal(code)
    })
  })
}