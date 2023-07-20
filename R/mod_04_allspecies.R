# UI ----------------------------------------------------------------------

allspeciesUI <- function(id) {
  # Create namespace variable
  ns <- NS(id)
  tagList(
# Counts per species ------------------------------------------------------
    h3("Species count"),
    fluidRow(
      column(width = 4,
             conditionalPanel(condition = "output.count_col",
                              ns = ns,
                              radioButtons(NS(id, "bars_count"),
                                           label = "Show count of:",
                                           choices = list("Capture events" = "npic",
                                                          "Individuals" = "nindiv"))
                              ),
             # textOutput(NS(id, "count")),
             selectInput(NS(id, "facet"), 
                         label = "Facetting variable",
                         choices = NULL)
      ),
      column(width = 8,
             outputCodeButton(girafeOutput(NS(id, "plot_species")))
      )
    ),
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
      )
    )
  )

}


# Server ------------------------------------------------------------------


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
    

# Was count provided? -----------------------------------------------------
    count_col <- reactive({
      unname(mapping_records()$count_col)
    })
    
    output$count_col <- reactive({
      if ( is.null(count_col()) ) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    })
    
    outputOptions(output, 'count_col', 
                  suspendWhenHidden = FALSE)

# Compute diversity indices -----------------------------------------------
  diversity_df <- metaReactive({
    "# Summarize species and cameras ---"
    count_df <- get_diversity_table(..(camtrap_data())$data$observations,
                                    cam_col = ..(cam_col_rec()),
                                    spp_col = ..(spp_col()),
                                    count_col = ..(mapping_cameras()$count_col), 
                                    keep_all_levels = TRUE)
    
    "# Compute diversity indices ---"
    get_diversity_indices(count_df, 
                          spp_col =  ..(spp_col()),
                          cam_col = ..(cam_col_rec()))
  }, bindToReturn = TRUE, varname = "diversity_df")
    
  diversity_table <- metaReactive({
    DT::datatable(..(diversity_df()),
                  filter = "none",
                  selection = "none",
                  options = list(scrollX = TRUE))
  }, bindToReturn = TRUE, varname = "diversity_table")


# Plot species ------------------------------------------------------------
  
  output$plot_species <- metaRender2(renderGirafe, {
    
    # Set height
    nspecies <- nrow(summarize_species(camtrap_data()$data$observations,
                                       species_col = spp_col(), 
                                       obs_col = obs_col()))
    unit <- nspecies/6
    height <- max(5, 
                  unit/(1 + exp(-12*unit)))
    
    if (input$bars_count == "nindiv") {
      # user wants to see count_col: set count_col to name of count_col
      count_col <- count_col()
    } else if (input$bars_count == "npic") { 
      # user wants to see npic: set count_col to NULL
      count_col <- NULL
    }
    
    metaExpr({
      "# Species abundance barplot ---"
      "# ggplot plot"
      gg <- plot_species_bars(..(camtrap_data())$data$observations, 
                              spp_col = ..(spp_col()),
                              obs_col = ..(obs_col()),
                              count_col = ..(count_col),
                              interactive = TRUE)
      
      "# ggiraph plot (interactive)"
      gi <- ggiraph::girafe(ggobj = gg,
                            width_svg = 8,
                            height_svg = ..(height))
      gi <- ggiraph::girafe_options(gi,
                                    opts_zoom(min = 0.5, max = 10))
      gi
    })
    
  })
  
# Plot map ----------------------------------------------------------------
    
    output$plot_diversity <- metaRender(renderLeaflet, {
      "# Plot map ---"
      
      "# Choose selected index for plot"
      index <- ..(diversity_df())[[..(input$divtype)]]
      names(index) <- ..(diversity_df())[[..(cam_col_rec())]]
      
      "# Set hovering labels (replace NA with 'No data')"
      labels <- ifelse(is.na(index), 
                       "No data", index)
      
      plot_map(..(camtrap_data())$data$deployments, 
               lat_col = ..(unname(mapping_cameras()$lat_col)),
               lon_col = ..(unname(mapping_cameras()$lon_col)),
               crs = ..(crs()),
               cam_col = ..(cam_col_cam()),
               radius = index,
               color = "black",
               label = labels,
               rescale = TRUE)
    })
    

# Plots code --------------------------------------------------------------

    # Species bars ---
    observeEvent(input$plot_species_output_code, {
      code <- expandChain(output$plot_species())
      displayCodeModal(code)
    })
  
    # Map ---
    observeEvent(input$plot_diversity_output_code, {
      code <- expandChain(output$plot_diversity())
      displayCodeModal(code)
    })
    

# Return values -----------------------------------------------------------
    return(list(diversity_table = diversity_table,
                diversity_map = output$plot_diversity,
                species_bars = output$plot_species))
    
  })
}