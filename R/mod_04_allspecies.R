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
# Diversity plot -----------------------------------------------------------

    h3("Diversity"),
    fluidRow(
      column(width = 4,
             selectInput(NS(id, "divtype"), 
                         label = "Diversity index",
                         choices = NULL),
             conditionalPanel("output.lonlat", 
                              ns = NS(id),
                              radioButtons(NS(id, "plot_type"),
                                           label = "Plot type",
                                           choices = c("Map" = "map",
                                                       "Barplot" = "bar"))
                              )
      ),
      column(width = 8,
             conditionalPanel("input.plot_type === 'map' && output.lonlat", 
                              ns = NS(id),
                              outputCodeButton(leafletOutput(NS(id, "plot_diversity_map"),
                                                             height = "400px"))
                              ),
             conditionalPanel("!output.lonlat || input.plot_type === 'bar'", 
                              ns = NS(id),
                              outputCodeButton(girafeOutput(NS(id, "plot_diversity"),
                                                            height = "500px"))
             )
      )
    )
  )

}


# Server ------------------------------------------------------------------


allspeciesServer <- function(id,
                             camtrap_data, 
                             mapping_records,
                             mapping_cameras,
                             cam_summary,
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
    
    obstype_col <- reactive({
      unname(mapping_records()$obstype_col)
    })    
    
    cam_col_cam <- reactive({
      unname(mapping_cameras()$cam_col)
    })
    
    cam_col_rec <- reactive({
      unname(mapping_records()$cam_col)
    })
    
# Create lonlat reactive --------------------------------------------------
    output$lonlat <- reactive({
      # Return TRUE if lon and lat are provided
      ifelse(!is.null(mapping_cameras()$lat_col) & !is.null(mapping_cameras()$lon_col),
             TRUE, FALSE)
    })
    outputOptions(output, 'lonlat', 
                  suspendWhenHidden = FALSE)
    
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


# Diversity index list ----------------------------------------------------
  diversity_list <- c("Number of species (richness)" = "richness",
                      "Shannon diversity" = "shannon",
                      "Simpson diversity" = "simpson")
  
  updateSelectInput(session = session,
                    "divtype",
                    choices = diversity_list)
  
# Compute diversity indices -----------------------------------------------
  sppcam_summary <- metaReactive({
    "# Summarize species and cameras ---"
    summarize_species(..(camtrap_data())$data$observations,
                      cam_col = ..(cam_col_rec()),
                      spp_col = ..(spp_col()),
                      count_col = ..(mapping_records()$count_col), 
                      by_cam = TRUE,
                      dfcam = ..(cam_summary()))
  }, bindToReturn = TRUE, varname = "sppcam_summary")
  
  diversity_df <- metaReactive({
    "# Compute diversity indices ---"
    get_diversity_indices(..(sppcam_summary()), 
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
                                       spp_col = spp_col(), 
                                       obstype_col = obstype_col()))
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
                              obstype_col = ..(obstype_col()),
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
  

# Plot diversity ----------------------------------------------------------


## Plot diversity map ------------------------------------------------------
    
    output$plot_diversity_map <- metaRender(renderLeaflet, {
      "# Plot map ---"
      
      "# Choose selected index for plot"
      index <- ..(diversity_df())[[..(input$divtype)]]
      names(index) <- ..(diversity_df())[[..(cam_col_rec())]]
      
      plot_map(..(camtrap_data())$data$deployments, 
               lat_col = ..(unname(mapping_cameras()$lat_col)),
               lon_col = ..(unname(mapping_cameras()$lon_col)),
               crs = ..(crs()),
               cam_col = ..(cam_col_cam()),
               radius = index,
               color = "black",
               label = round(index, 3),
               rescale = TRUE)
    })
  

## Plot diversity barplot ------------------------------------------------------
  
  
  output$plot_diversity <- metaRender2(renderGirafe, {
    hw <- get_hw(nrow(diversity_df()))
    height <- hw$height
    width <- hw$width
    
    metaExpr({
      "# Plot diversity ---"
      "# Replace NA with zero (cameras with no data) to discard warning"
      diversity_df_plot <- ..(diversity_df())
      diversity_df_plot[is.na(diversity_df_plot)] <- 0
      
      cam_vec <- ..(camtrap_data())$data$deployments[[..(cam_col_cam())]]
      
      gg <- plot_diversity(diversity_df_plot, 
                           div_col = ..(input$divtype), 
                           cam_col = ..(cam_col_rec()),
                           cam_vec = cam_vec,
                           interactive = TRUE) +
        ylab(..(names(diversity_list[diversity_list == input$divtype]))) +
        xlab("Cameras")
      
      "# ggiraph plot (interactive)"
      gi <- ggiraph::girafe(ggobj = gg,
                            width_svg = ..(width),
                            height_svg = ..(height))
      gi <- ggiraph::girafe_options(gi,
                                    opts_zoom(min = 0.5, max = 10),
                                    opts_selection(type = "none"))
      gi
    })
  })
    

# Plots code --------------------------------------------------------------

    # Species bars ---
    observeEvent(input$plot_species_output_code, {
      code <- expandChain(output$plot_species())
      displayCodeModal(code)
    })
  
    # Diversity map ---
    observeEvent(input$plot_diversity_map_output_code, {
      code <- expandChain(output$plot_diversity_map())
      displayCodeModal(code)
    })
    
    # Diversity plot ---
    observeEvent(input$plot_diversity_output_code, {
      code <- expandChain(output$plot_diversity())
      displayCodeModal(code)
    })
    
# Return values -----------------------------------------------------------
    
    div_plot <- reactive({
      if (input$plot_type == 'map' && output$lonlat()) {
        return(output$plot_diversity_map())
      } else {
        return(output$plot_diversity())
      }
    })
    
    return(list(diversity_table = diversity_table,
                sppcam_summary = sppcam_summary,
                diversity_plot = div_plot,
                species_bars = output$plot_species))
    
  })
}
