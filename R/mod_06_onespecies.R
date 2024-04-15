
# UI ----------------------------------------------------------------------

onespeciesUI <- function(id) {
  tagList(

# Choose species ----------------------------------------------------------

    fluidRow(column(width = 12,
                    uiOutput(NS(id, "species_select"))
                    ),
             ),
    # Activity plot -----------------------------------------------------------

    h3("Activity plot"),
    fluidRow(column(width = 3,
                    numericInput(NS(id, "adj"),
                                 "Bandwidth adjustment",
                                 min = 1, max = 100, 
                                 step = 0.1,
                                 value = 1)),
             column(width = 9, 
                    outputCodeButton(girafeOutput(NS(id, "density_plot")),
                                     height = "400px")
                    )
             ),

# Species count ---------------------------------------------------------------------
             h3("Species count"),
             fluidRow(
               column(width = 6, 
                      selectInput(NS(id, "value"),
                                  label = "Plotted value",
                                  choices = NULL)
               ),
               column(width = 6,
                      conditionalPanel("output.lonlat", 
                                       ns = NS(id),
                                       radioButtons(NS(id, "plot_type"),
                                                    label = "Plot type",
                                                    choices = c("Map" = "map",
                                                                "Barplot" = "bar"))
                      )),
               column(width = 12,
                      conditionalPanel("input.plot_type === 'map' && output.lonlat", 
                                       ns = NS(id),
                                       outputCodeButton(leafletOutput(NS(id, "plot_abundance_map")),
                                                        height = "500px")
                      ),
                      conditionalPanel("input.plot_type === 'bar' || !output.lonlat", 
                                       ns = NS(id),
                                       outputCodeButton(girafeOutput(NS(id, "plot_abundance")),
                                                        height = "500px")
                      )
               )
             )
             
  )
}


# Server ------------------------------------------------------------------

onespeciesServer <- function(id,
                             camtrap_data, 
                             mapping_records,
                             mapping_cameras,
                             sppcam_summary,
                             crs) {
  moduleServer(id, function(input, output, session) {
    
    # Test reactive input -----------------------------------------------------
    # stopifnot(is.reactive(camtrap_data))
    stopifnot(is.reactive(mapping_records))
    stopifnot(is.reactive(mapping_cameras))
    stopifnot(is.reactive(crs))
    
    # Create column names reactives -------------------------------------------
    obstype_col <- reactive({
      unname(mapping_records()$obstype_col)
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
    
    
    time_col <- reactive({
      unname(mapping_records()$time_col)
    })
    datetime_col <- reactive({
      unname(mapping_records()$datetime_col)
    })
    
    # Create lonlat reactive --------------------------------------------------
    output$lonlat <- reactive({
      # Return TRUE if lon and lat are provided
      ifelse(!is.null(mapping_cameras()$lat_col) & !is.null(mapping_cameras()$lon_col),
             TRUE, FALSE)
    })
    outputOptions(output, 'lonlat', 
                  suspendWhenHidden = FALSE)
    
    # Species selection -------------------------------------------------------
    
    ## Get species df ----------------------------------------------------------
    species_df <- reactive({
      # Validate to wait for filtering
      validate(need(nrow(camtrap_data()$data$observations) != 0, 
                    "Cannot analyze an empty table: plese check data filtering"))
      
      camtrapviz::get_unique_species(camtrap_data()$data$observations,
                                     spp_col = spp_col(), 
                                     obstype_col = obstype_col(),
                                     return_df = TRUE,
                                     add_ID = TRUE)
    })
    
    ## Create selectInput ------------------------------------------------------
    
    output$species_select <- renderUI({
      # Create choices df
      choices <- as.list(species_df()$ID)
      names(choices) <- species_df()[[spp_col()]]
      
      selectInput(NS(id, "species"),
                  label = "Choose species",
                  choices = choices)
    })


    ## Get filtered data ------------------------------------------------------
    
    focus_spp <- reactive({
      # Get the focus species ---
      
      # Get subset of the df with selected values
      selected <- species_df()[species_df()$ID %in% input$species, ]
      
      # Separate spp and obstype filters
      get_obs_spp(selected,
                  spp_col = spp_col(),
                  obstype_col = obstype_col())
    })

    focus_spp_records <- metaReactive2({
      
      spp_filter <- focus_spp()$spp
      obstype_filter <- focus_spp()$obstype
      
      # Separate the code in 2 for lisibility
      if (!is.null(spp_filter)) {
        metaExpr({
          "# Get focus species data ---"
          ..(camtrap_data())$data$observations |> 
            dplyr::filter(.data[[..(spp_col())]] == ..(spp_filter))
        }, bindToReturn = TRUE)
      } else if (!is.null(obstype_filter)) {
        metaExpr({
          "# Get focus species data ---"
          ..(camtrap_data())$data$observations |> 
            dplyr::filter(.data[[..(obstype_col())]] == ..(obstype_filter))
        }, bindToReturn = TRUE)
      }
    }, varname = "focus_spp_records")

    focus_spp_records_table <- metaReactive({
      DT::datatable(..(focus_spp_records()),
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE))
    }, bindToReturn = TRUE, varname = "focus_spp_records_table")
    
    # Density -----------------------------------------------------------------

    
    ## Compute density ---------------------------------------------------------
    density <- metaReactive2({
      
      validate(need(nrow(focus_spp_records()) != 0, 
                    "Waiting for records data..."))

      # Get time
      if (!is.null(datetime_col())) {
        metaExpr({
          "# Get density ---"
          datetime <- ..(focus_spp_records())[[..(datetime_col())]]
          time <- format(datetime, format = "%H:%M:%S")
          time <- chron::times(time)
          
          time_rad <- as.numeric(time)*2*pi
          activity::fitact(time_rad, adj = ..(input$adj))
        }, bindToReturn = TRUE)

      } else {
        metaExpr({
          "# Get density ---"
          time <- ..(focus_spp_records())[[..(time_col())]]

          time_rad <- as.numeric(time)*2*pi
          activity::fitact(time_rad, adj = ..(input$adj))
        }, bindToReturn = TRUE)

      }
    }, varname = "density")

    
    ## Density plot ------------------------------------------------------------

    output$density_plot <- metaRender(renderGirafe, {
      "# Plot density ---"
      dfplot <- ..(focus_spp_records())
      
      # Add times column
      if (!is.null(..(datetime_col()))) {
        datetime <- dfplot[[..(datetime_col())]]
        time <- format(datetime, format = "%H:%M:%S")
        time <- chron::times(time)
        dfplot$time <- time
        time_col <- "time"
      } else {
        time_col <- ..(time_col())
      }
      
      pdf_df <- as.data.frame(..(density())@pdf)
      
      gg <- plot_activity(dffit = pdf_df,
                          time_dffit = "x",
                          y_fit = "y",
                          dfrec = dfplot,
                          time_dfrec = time_col,
                          unit = "clock",
                          freq = TRUE,
                          interactive = TRUE)
      
      gi <- ggiraph::girafe(ggobj = gg)
      gi <- ggiraph::girafe_options(gi,
                                    opts_hover(css = "fill:orange"),
                                    opts_hover_inv(css = "opacity:0.3"))
      gi
    })
    
    observeEvent(input$density_plot_output_code, {
      code <- expandChain(output$density_plot())
      displayCodeModal(code)
    })
    
    # Abundance plot ----------------------------------------------------------- 
    
    abundance_list <- c("Abundance" = "individuals",
                        "Sightings" = "sightings",
                        "Proportion of individuals" = "sightings_prop",
                        "Proportion of sightings" = "individuals_prop",
                        "Relative abundance index (individuals)" = "individuals_RAI",
                        "Relative abundance index (sightings)" = "sightings_RAI")
    
    updateSelectInput(session = session,
                      "value",
                      choices = abundance_list)
  
    focus_spp_summary <- metaReactive2({
      # Get subset of the df with selected values
      all_filter <- species_df()[species_df()$ID %in% input$species, ]
      
      # Separate spp and obstype filters
      filters <- get_obs_spp(all_filter,
                             spp_col = spp_col(),
                             obstype_col = obstype_col())
      spp_filter <- filters$spp
      obstype_filter <- filters$obstype
      
      # Separate the code in 2 for lisibility
      if (!is.null(spp_filter)) {
        metaExpr({
          "# Get focus species data ---"
          ..(sppcam_summary()) |> 
            dplyr::filter(.data[[..(spp_col())]] == ..(spp_filter))
        }, bindToReturn = TRUE)
      } else if (!is.null(obstype_filter)) {
        metaExpr({
          "# Get focus species data ---"
          ..(sppcam_summary()) |> 
            dplyr::filter(.data[[..(obstype_col())]] == ..(obstype_filter))
        }, bindToReturn = TRUE)
      }
      
    }, varname = "focus_spp_summary")
    
    
    ## Abundance map ----------------------------------------------------------- 
    
    output$plot_abundance_map <- metaRender(renderLeaflet, {
      "# Plot abundance map ---"
      abundance <- ..(focus_spp_summary())[[..(input$value)]]
      names(abundance) <- ..(focus_spp_summary())[[..(cam_col_rec())]]
      
      plot_map(..(camtrap_data())$data$deployments, 
               lat_col = ..(unname(mapping_cameras()$lat_col)),
               lon_col = ..(unname(mapping_cameras()$lon_col)),
               crs = ..(crs()),
               cam_col = ..(cam_col_cam()),
               radius = abundance,
               color = "black",
               label = round(abundance, 3),
               rescale = TRUE)
    })
    

    ## Abundance barplot -------------------------------------------------------

    output$plot_abundance <- metaRender2(renderGirafe, {
      hw <- get_hw(nrow(focus_spp_summary()))
      height <- hw$height
      width <- hw$width
      
      metaExpr({
        "# Plot abundance ---"
        "# Replace NA with zero (cameras with no data) to discard warning"
        focus_spp_summary_plot <- ..(focus_spp_summary())
        focus_spp_summary_plot[is.na(focus_spp_summary_plot)] <- 0
        
        gg <- plot_diversity(focus_spp_summary_plot, 
                             div_col = ..(input$value), 
                             cam_col = ..(cam_col_rec()),
                             interactive = TRUE) +
          ylab(..(names(abundance_list[abundance_list == input$value]))) +
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
    
    observeEvent(input$plot_abundance_map_output_code, {
      code <- expandChain(output$plot_abundance_map())
      displayCodeModal(code)
    })
    
    observeEvent(input$plot_abundance_output_code, {
      code <- expandChain(output$plot_abundance())
      displayCodeModal(code)
    })

    # Return values -----------------------------------------------------------
    
    abd_plot <- reactive({
      if (input$plot_type == 'map' && output$lonlat()) {
        return(output$plot_abundance_map())
      } else {
        return(output$plot_abundance())
      }
    })
    
    focus_spp_name <- reactive({
      spp <- focus_spp()
      
      if (!is.null(spp$obstype)) {
        spp$obstype
      } else {
        spp$spp
      }
      
    })
    
    abundance_value <- reactive({
      names(abundance_list[abundance_list == input$value])
    })

    return(list(focus_spp = focus_spp_name,
                focus_spp_records = focus_spp_records,
                density_plot = output$density_plot,
                abundance_value = abundance_value,
                abundance_plot = abd_plot))
    
  })
}
