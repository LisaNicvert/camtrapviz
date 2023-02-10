
#' Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return The server generating functions for Shiny
#' @export
server <- function(input, output, session) {


# Setup variables ---------------------------------------------------------

  # Define roots for ShinyFiles 
  roots <- c("home" = fs::path_home())
  
  # Define placeholder for optional columns
  nullval <- "Not present in data"
  
  # Define columns for which empty is allowed
  empty_allowed <- c("count_col", "obs_col")
  

  # Define camera columns
  cameras_cols_wanted <- c("cam_col_cov",
                           "lat_col_cov",
                           "lon_col_cov")
  
  # Define example data column mappings
  example_mapping_records <- list(mica = c("spp_col" = "vernacularNames.en",
                                           "obs_col" = "observationType",
                                           "cam_col" = "deploymentID",
                                           "timestamp_col" = "timestamp",
                                           "count_col" = "count"),
                                  recordTableSample = c("spp_col" = "Species",
                                                        "cam_col" = "Station",
                                                        "timestamp_col" = "DateTimeOriginal"))
  example_mapping_cameras <- list(mica = c("cam_col_cov" = "deploymentID",
                                           "lat_col_cov" = "latitude",
                                           "lon_col_cov" = "longitude"),
                                  recordTableSample = c("cam_col_cov" = "Station",
                                                        "lat_col_cov" = "utm_y",
                                                        "lon_col_cov" = "utm_x"))
  
# Load example data -------------------------------------------------------
  utils::data(mica, package = "camtraptor")
  utils::data(recordTableSample, package = "camtrapR")
  utils::data(camtraps, package = "camtrapR")
  

# Description for example datasets ----------------------------------------
  output$dyntext <- renderText({
    if(input$example_file == "mica") {
      "Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany (camtrapDP format)"
    } else {
      "Sample dataset from the camtrapR package (camtrapR format)"
    }
  })
  

# Read files --------------------------------------------------------------
  

## Shiny file listener -----------------------------------------------------
  shinyFileChoose(input, 'records_input', 
                  root = c("home" = fs::path_home()),
                  filetypes = c('csv', 'json'))
  

## Get imported file extension ---------------------------------------------
  records_extension <- reactive({
    # Get file
    file <- shinyFiles::parseFilePaths(roots,
                                       input$records_input)
    
    # Ensure file is loaded
    file_path <- file$datapath
    validate(need(file_path != '', "Please upload file"))
    
    # Get file extension
    ext <- tools::file_ext(file_path)
    ext
  })
  
  output$records_extension <- reactive({
    records_extension()
  })
  outputOptions(output, 'records_extension', 
                suspendWhenHidden = FALSE)
  

## Get raw data -------------------------------------------------------------

  dat_raw <- reactive({
    
    if (input$input_type == 1) { # Example dataset
      if(input$example_file == "mica") {
        res <- mica
      } else {
        res <- list(data = list(observations = recordTableSample,
                                deployments = camtraps))
      }
    } else if (input$input_type == 2) { # Uploaded dataset
      # Get file
      file <- shinyFiles::parseFilePaths(roots,
                                         input$records_input)
      # Ensure file is loaded before continuing
      req(file)
      
      # Get file_path
      file_path <- file$datapath
      validate(need(file_path != '', "Please upload a records file"))
      
      # Get separator values
      sep_records <- input$records_sep
      sep_cameras <- input$cameras_sep
      
      # User wants to import a camera file?
      if (input$import_cameras) {
        # Get file
        camera_file <- input$cameras_input
        
        # Ensure file is loaded before continuing
        req(camera_file)
        
        # Get camera_path
        cameras_path <- camera_file$datapath
        validate(need(cameras_path != '', "Please upload camera file"))
      } else {
        cameras_path <- NULL
      }
      
      # Read data
      res_all <- read_data(file_path, 
                           sep_records,
                           cameras_path,
                           sep_cameras)
      
      # Update records separator
      updateRadioButtons(inputId = "records_sep",
                         selected = res_all$sep$sep_records)
      
      # Update camera separator
      updateRadioButtons(inputId = "cameras_sep",
                         selected = res_all$sep$sep_cameras)
      
      # Get data
      res <- res_all$camtrap_data
    }
    return(res)
  })
  

  # Columns mapping (records) ---------------------------------------------------------
  
  # Get input data columns
  records_cols <- reactive({
    colnames(dat_raw()$data$observations)
  })
  
  # Get the columns we want
  records_cols_wanted <- reactive({
    if (input$datetime_or_timestamp == 'timestamp') {
      datetime_widgets <- "timestamp_col"
    } else if (input$datetime_or_timestamp == 'date_time') {
      datetime_widgets <- c("date_col", "time_col")
    }
    widget_list <- c("spp_col", "cam_col",
                     datetime_widgets,
                     "count_col", "obs_col")
    
    if (!input$import_cameras) { 
      # User doesn't want to import a camera file
      widget_list <- c(widget_list,
                       "lat_col", "lon_col")
    }
    widget_list
  })
  
  # Default columns mapping for records
  default_records <- reactive({
    # Ensure records table was imported and is available 
    #   (i.e. no example data and data correctly loaded)
    req(input$records_input)
    
    validate(need(!(nullval %in% records_cols()),
                  paste("Please provide a dataframe with no column named",
                        nullval)))
    
    # Find default names
    default_names <- find_default_colnames(records_cols_wanted(),
                                           records_cols(),
                                           empty_allowed,
                                           empty_placeholder = nullval)
    default_names
  })
  
  # Update selection list and default names 
  # in selectInput for records
  observeEvent(input$records_input, {
    if (input$input_type == 2) { # Only update widgets for manual import
      for(w in records_cols_wanted()) {
        # Get default
        default_name <- default_records()[[w]]
          
        if(w %in% empty_allowed) {
          updateSelectInput(session = session,
                            inputId = w,
                            choices = c(nullval, records_cols()),
                            selected = default_name)
        } else {
          updateSelectInput(session = session,
                            inputId = w,
                            choices = records_cols(),
                            selected = default_name)
        }
      }
    }
  })
  
  # Initialize date/time/stamp upon input change
  observeEvent(input$datetime_or_timestamp, {
    if (input$input_type == 2) { # Only update widgets for manual import
      widgets <- records_cols_wanted()
      time_widgets <- widgets[which(widgets %in% c("date_col", "time_col", "timestamp_col"))]
      
      for(tw in time_widgets) {
        # Get default
        default_name <- default_records()[[tw]]
        
        # Cannot be empty since it is a datetime
        updateSelectInput(session = session,
                          inputId = tw,
                          choices = records_cols(),
                          selected = default_name)
        
      }
    }
  })
  
  
  # Mapping value for records columns
  mapping_records <- reactive({
    if (input$input_type == 1) { # Example files
      # Get known mapping
      res <- example_mapping_records[[input$example_file]]
    } else if (input$input_type == 2) { # Uploaded files
      # Get the values selected by the user
      
      # Get relevant widgets
      widgets <- records_cols_wanted()
      
      # Get selected values for widgets
      res <- vector(mode = "character", 
                    length = length(widgets))
      for(i in 1:length(widgets)) {
        res[i] <- input[[widgets[i]]]
      }
      names(res) <- widgets
    }
    res
  })
  
  # Column mapping (cameras) ------------------------------------------------
  
  # Get input cameras columns
  cameras_cols <- reactive({
    if (!is.null(dat_raw()$data$deployments)) { # Camera file was provided
      res <- colnames(dat_raw()$data$deployments)
    } else { # Camera is in data
      res <- NULL
    }
  })
  
  # Default columns mapping for cameras
  default_cameras <- reactive({
    # Find default names
    if (!is.null(cameras_cols())) {
      default_names <- find_default_colnames(cameras_cols_wanted,
                                             cameras_cols())
    } else {
      default_names <- NULL
    }
    default_names
  })
  
  # Update selection list and default names 
  # in selectInput for cameras
  observe({
    if (input$input_type == 2) { # Only update widgets for manual import
      if (!is.null(dat_raw()$data$deployments)) { # Camera file was provided
        for(w in cameras_cols_wanted) {
          # Get default
          default_name <- default_cameras()[[w]]
          
          updateSelectInput(session = session,
                            inputId = w,
                            choices = cameras_cols(),
                            selected = default_name)
        }
      }
    }
  })
  
  # Mapping value for records columns
  mapping_cameras <- reactive({
    if (input$input_type == 1) { # Example files
      # Get known mapping
      res <- example_mapping_cameras[[input$example_file]]
    } else if (input$input_type == 2) { # Uploaded files
      # Get the values selected by the user
      if (!is.null(cameras_cols())) { # Camera file was provided
        # Get relevant widgets
        widgets <- cameras_cols_wanted
      } else { # Camera file was not provided
        widgets <- gsub("_cov$", "", cameras_cols_wanted)
      }
      # Get selected values for widgets
      res <- vector(mode = "character", 
                    length = length(widgets))
      for(i in 1:length(widgets)) {
        res[i] <- input[[widgets[i]]]
      }
      names(res) <- widgets
    }
    res
  })
  
  
  # Clean data --------------------------------------------------------------
  records_select <- reactive({
    # Get relevant columns (all except "Not in data")
    records_select <- mapping_records()[mapping_records() != nullval]
    records_select <- records_select[which(!names(records_select) %in% c("lat_col", "lon_col"))]
    
    records_select
  })
  
  dat <- reactive({
    # Copy raw data
    dat <- dat_raw()
    
    # Records ---
    dat$data$observations <- format_table(dat$data$observations,
                                         records_select())
    
    # Cameras ---
    if ("cam_col" %in% names(mapping_cameras())) { 
      # Split data if camera covariates are in records
      cameras <- dat$data$observations %>%
        dplyr::select(any_of(unname(mapping_cameras())), 
                      everything())
      
      cameras <- cameras %>% distinct()
      dat$data$deployments <- cameras
    }
    
    dat$data$deployments <- format_table(dat$data$deployments,
                                         mapping_cameras())
    
    return(dat)
  })
  
# File input preview ------------------------------------------------------

## Raw data ----------------------------------------------------------------
  output$raw_records <- renderDataTable({
    DT::datatable(dat_raw()$data$observations,
                  filter = "none",
                  selection = "none",
                  options = list(scrollX = TRUE))
  })
  
  output$raw_cameras <- renderDataTable({
    DT::datatable(dat_raw()$data$deployments,
                  filter = "none",
                  selection = "none",
                  options = list(scrollX = TRUE))
  })
  

## Cleaned data ------------------------------------------------------------
  output$records <- renderDataTable({
    DT::datatable(dat()$data$observations,
                  filter = "none",
                  selection = "none",
                  options = list(scrollX = TRUE)) %>%
      DT::formatStyle(records_select(),
                      backgroundColor = '#F5EE9E')
  })
  
  output$cameras <- renderDataTable({
    DT::datatable(dat()$data$deployments,
                  filter = "none",
                  selection = "none",
                  options = list(scrollX = TRUE)) %>%
      DT::formatStyle(mapping_cameras(),
                      backgroundColor = '#F5EE9E')
  })
  
  
}