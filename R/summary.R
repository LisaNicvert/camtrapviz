summaryUI <- function(id) {
  tagList(

# Info box ----------------------------------------------------------------

    box(width = 12,
        h2("Survey summary"),
        h3("Overview"),
        fluidRow(infoBox("Cameras", 
                         icon = icon("camera"),
                         color = 'aqua',
                         value = textOutput(NS(id, "ncameras"))
                         ),
                 infoBox("Species", 
                         icon = icon("paw"),
                         color = 'teal',
                         value = textOutput(NS(id, "nspecies"))
                         )
                 ),
        fluidRow(infoBox("Trapping nights", 
                         icon = icon("clock"),
                         color = 'fuchsia',
                         value = textOutput(NS(id, "sampling_length"))),
                 infoBox("Active", 
                         icon = icon("calendar"),
                         color = 'purple',
                         value = textOutput(NS(id, "daterange")))
                 ),
        br(),
        h3("Camera activity"),
        fluidRow(
          column(width = 12,
                 div(
                   style = "height:600px; width:100%;",
                   girafeOutput(NS(id, "plot_occurrences"), 
                                height = "600px")
                 )
                 )
        ),
        br(),
        h3("Species count"),
        fluidRow(
          column(width = 12,
                 style = "height:600px; width:100%;",
                 girafeOutput(NS(id, "plot_species"), height = "600px")
                 )
        ),
        )
    )
    
}

summaryServer <- function(id, 
                          camtrap_data, 
                          mapping_records,
                          mapping_cameras) {
  moduleServer(id, function(input, output, session) {
    
# Test reactive input -----------------------------------------------------
    stopifnot(is.reactive(camtrap_data))
    stopifnot(is.reactive(mapping_records))
    stopifnot(is.reactive(mapping_cameras))
    

# Reactive general values -------------------------------------------------
    ncameras <- reactive({
      nrow(camtrap_data()$data$deployments)
    })
    
    nspecies <- reactive({
      # Get species column
      species_col <- mapping_records()["spp_col"]
      species <- camtrap_data()$data$observations[[species_col]]
      
      if ("obs_col" %in% names(mapping_records())) {
        # Filter to get only animal species
        obs_col <- mapping_records()["obs_col"]
        species <- species[camtrap_data()$data$observations[[obs_col]] == "animal"]
      }
      
      length(unique(species))
    })
    
    daterange <- reactive({
      if ("timestamp_col" %in% names(mapping_records())) {
        # We only have timestamp
        timestamp_col <- mapping_records()["timestamp_col"]
        date <- camtrap_data()$data$observations[[timestamp_col]]
        date <- as_date(date)
      } else if ("date_col" %in% names(mapping_records())) {
        # We only have date
        date_col <- mapping_records()["date_col"]
        date <- camtrap_data()$data$observations[[date_col]]
      } else {
        # No date or timestamp
        stop("Date or time must be present in data")
      }
      c(min(date), max(date))
    })
    
    sampling_length <- reactive({
      #### TO IMPROVE
      daterange()[2] - daterange()[1]
    })

# Infobox values ----------------------------------------------------------
    output$ncameras <- renderText({
      ncameras()
    })
    
    output$nspecies <- renderText({
      nspecies()
    })
    
    output$sampling_length <- renderText({
      as.numeric(sampling_length(), "days")
    })
    
    output$daterange <- renderText({
      minmax_date <- daterange()
      minmax_date <- format(minmax_date, "%d %b %Y")
      paste(minmax_date[1], "to", minmax_date[2])
    })
    
# Plots -------------------------------------------------------------------
    output$plot_occurrences <- renderGirafe({
      df <- camtrap_data()$data$observations
      
      if ("timestamp_col" %in% names(mapping_records())) {
        timestamp_col <- mapping_records()["timestamp_col"]
        time_col <- NULL
        date_col <- NULL
      } else if ("time_col" %in% names(mapping_records()) &
                 "date_col" %in% names(mapping_records())) {
        timestamp_col <- NULL
        time_col <- mapping_records()["time_col"]
        date_col <- mapping_records()["date_col"]
      } else {
        stop("timestapl_col or time_col and date_col should be available")
      }

      gg <- plot_points(df,
                        camera_col = mapping_records()["cam_col"],
                        timestamp_col = timestamp_col,
                        time_col = time_col,
                        date_col = date_col,
                        spp_col = mapping_records()["spp_col"])
      
      # Define height
      unith <- ncameras()/4
      height <- max(5, 
                    unith/(1 + exp(-12*unith)))
      
      # Define width
      unitw <- as.numeric(daterange()[2] - daterange()[1], "days")/60 # One inch per 2 months
      width <- max(8,
                   unitw/(1 + exp(-24*unitw)))
      
      x <- girafe(ggobj = gg,
                  width_svg = width,
                  height_svg = height)
      x <- girafe_options(x,
                          opts_zoom(min = 0.5, max = 10))
      x
    })
    
    output$plot_species <- renderGirafe({
      df <- camtrap_data()$data$observations
      
      if ("obs_col" %in% names(mapping_records())) {
        obs_col <- mapping_records()["obs_col"]
      } else {
        obs_col <- NULL
      }
      
      if ("count_col" %in% names(mapping_records())) {
        count_col <- mapping_records()["count_col"]
      } else {
        count_col <- NULL
      }
      
      gg <- plot_species_bars(df, 
                              spp_col = mapping_records()["spp_col"],
                              obs_col = obs_col,
                              count_col = count_col)
      

      unit <- nspecies()/6
      height <- max(5, 
                    unit/(1 + exp(-12*unit)))
      x <- girafe(ggobj = gg,
                  width_svg = 8,
                  height_svg = height)
      x <- girafe_options(x,
                          opts_zoom(min = 0.5, max = 10))
      x
    })
    
  })
}