allspeciesUI <- function(id) {
  tagList(
        
# Diversity map -----------------------------------------------------------

    h3("Diversity map"),
    fluidRow(
      column(width = 4,
             selectInput(NS(id, "divtype"), 
                         label = "Diversity index",
                         choices = c("Number of species (richness)" = "richness",
                                     "Shannon diversity" = "shannon",
                                     "Simpson diversity" = "simpson"))
      ),
      column(width = 8,
             outputCodeButton(leafletOutput(NS(id, "plot_diversity"),
                                            height = "400px"))
      ),
      column(width = 12,
             dataTableOutput(NS(id, "diversity_table")))
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

}
allspeciesServer <- function(id,
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
    spp_col <- reactive({
      unname(mapping_records()$spp_col)
    })    
    
    obs_col <- reactive({
      unname(mapping_records()$obs_col)
    })    
    
    cam_col_cam <- reactive({
      unname(mapping_cameras()$cam_col)
    })
    
    cam_col_rec <- reactive({
      unname(mapping_records()$cam_col)
    })
    

# Species diversity table -------------------------------------------------
  diversity_df <- metaReactive({
    "# Summarize species and cameras ---"
    get_diversity_table(..(camtrap_data())$data$observations,
                        cam_col = ..(cam_col_rec()),
                        spp_col = ..(spp_col()),
                        count_col = ..(mapping_cameras()$count_col), 
                        keep_all_levels = TRUE)
    
  }, varname = "diversity_df", bindToReturn = TRUE)
    
  output$diversity_table <- renderDataTable({
    DT::datatable(diversity_df(),
                  filter = "none",
                  selection = "none",
                  options = list(scrollX = TRUE))
  })
  

# Compute diversity indices -----------------------------------------------

  diversity <- metaReactive({
    "# Get diversity indices ---"
    if ("empty" %in% colnames(..(diversity_df()))) {
      diversity_df <- ..(diversity_df()) |>
        group_by(.data[[..(cam_col_rec())]]) |>
        summarise(richness = ifelse(all(empty), 
                                    NA, n()) , 
                  shannon = ifelse(all(empty),
                                   NA, -sum((count/sum(count))*log(count/sum(count)))),
                  simpson = ifelse(all(empty),
                                   NA, sum(count*(count-1))/(sum(count)*(sum(count)-1))),
                  .groups = "drop")
    } else {
      diversity_df <- ..(diversity_df()) |>
        group_by(.data[[..(cam_col_rec())]]) |>
        summarise(richness = n() , 
                  shannon = -sum((count/sum(count))*log(count/sum(count))),
                  simpson = sum(count*(count-1))/(sum(count)*(sum(count)-1)),
                  .groups = "drop")
    }
    

    "# Choose selected index for plot"
    res <- diversity_df[[..(input$divtype)]]
    names(res) <- diversity_df[[..(cam_col_rec())]]
    res
  })
    
# Plot map ----------------------------------------------------------------
    
    output$plot_diversity <- metaRender(renderLeaflet, {
      "# Plot map ---"
      # Labels (replace NA with "No data")
      labels <- ifelse(is.na(..(diversity())), 
                       "No data", ..(diversity()))
      
      plot_map(..(camtrap_data())$data$deployments, 
               lat_col = ..(unname(mapping_cameras()$lat_col)),
               lon_col = ..(unname(mapping_cameras()$lon_col)),
               crs = ..(crs()),
               cam_col = ..(cam_col_cam()),
               circle_radii = ..(diversity()),
               color = "black",
               label = labels,
               rescale = TRUE)
    })
    
# Plot species ------------------------------------------------------------

    output$plot_species <- metaRender2(renderGirafe, {
      # Set height
      nspecies <- nrow(summarize_species(camtrap_data()$data$observations,
                                         species_col = spp_col(), 
                                         obs_col = obs_col()))
      unit <- nspecies/6
      height <- max(5, 
                    unit/(1 + exp(-12*unit)))
      metaExpr({
        "# Species abundance barplot ---"
        "# ggplot plot"
        gg <- plot_species_bars(..(camtrap_data())$data$observations, 
                                spp_col = spp_col(),
                                obs_col = obs_col(),
                                count_col = ..(unname(mapping_records()$count_col)))
        
        "# ggiraph plot (interactive)"
        gi <- ggiraph::girafe(ggobj = gg,
                              width_svg = 8,
                              height_svg = ..(height))
        gi <- ggiraph::girafe_options(gi,
                                      opts_zoom(min = 0.5, max = 10))
        gi
      })
      
    })
    

# Plots code --------------------------------------------------------------
    # Map ---
    observeEvent(input$plot_diversity_output_code, {
      code <- expandChain(output$plot_diversity())
      displayCodeModal(code)
    })
    
    # Species bars ---
    observeEvent(input$plot_species_output_code, {
      code <- expandChain(output$plot_species())
      displayCodeModal(code)
    })
    
    
    
  })
}