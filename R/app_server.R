
#' Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return The server generating functions for Shiny
#' @export
server <- function(input, output, session) {

# Define roots for ShinyFiles ---------------------------------------------
  roots <- c("home" = fs::path_home())
  
# Define placeholder for optional columns ---------------------------------
  nullval <- "Not present in data"
  
# Define columns for which empty is allowed -------------------------------
  empty_allowed <- "count_col"
  
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
  output$records_extension <- reactive({
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
  outputOptions(output, 'records_extension', 
                suspendWhenHidden = FALSE)
  

## Get dataset -------------------------------------------------------------

  dat <- reactive({
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
  
      # Ensure file is loaded
      file_path <- file$datapath
      
      validate(need(file_path != '', "Please upload file"))
      
      # Get file extension
      ext <- tools::file_ext(file_path)
      
      if (ext == "csv") { # User uploaded a csv file
        # Get separator value
        sep <- input$records_sep
        
        # Read csv
        res_records <- read_csv(file_path = file_path, 
                                column_separator = sep)
        
        # Update file separator
        updateRadioButtons(inputId = "records_sep",
                           selected = res_records$sep)
        
        if (input$import_cameras) { # User wants to import a camera file
          # Get file
          file <- input$cameras_input
          req(file)
          
          file_path <- file$datapath
          
          # Get separator value
          sep <- input$cameras_sep
          
          # Read csv
          res_cameras <- read_csv(file_path = file_path, 
                                  column_separator = sep)
          
          # Update file separator
          updateRadioButtons(inputId = "cameras_sep",
                             selected = res_cameras$sep)
        } else { # User doesn't want to import a camera file
          res_cameras <- NULL
        }
        
        # Final result is a list with one component $data
        #   This is done to match the structure of a camtrapDP file in R
        #   The list has 2 sub-components:
        #   $observations (records) and $deployments (cameraS)
        res <- list(data = list(observations = res_records$dat,
                                deployments = res_cameras$dat))
        
      } else if (ext == "json") { #   CamtrapDP format
        res <- camtraptor::read_camtrap_dp(file_path, media = FALSE)
      } else { # Unknown extension
        validate(need(ext == "csv" || ext == "json", 
                      "Please upload a csv file or a json datapackage"))
      }
      
    }
    return(res)
  })
  

# Columns mapping ---------------------------------------------------------
  
  # Get input data columns
  records_cols <- reactive({
    colnames(dat()$data$observations)
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
                     "count_col")
    
    if (!input$import_cameras) { # User doesn't want to import a camera file
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
  })
  
  # Mapping value for records columns
  mapping_records <- reactive({
    # Get relevant widgets
    widgets <- records_cols_wanted()
    
    # Get selected values
    res <- vector(mode = "charactger", 
                  length = length(widgets))
    for(i in 1:length(widgets)) {
      res[i] <- input[[widgets[i]]]
    }
    names(res) <- widgets
    
    cat(res[1])
    res
  })

# File input preview ------------------------------------------------------
  output$records_preview <- renderDataTable({
    dat_head <- utils::head(dat()$data$observations, 5)
    
    DT::datatable(dat_head,
                  filter = "none",
                  selection = "none",
                  options = list(dom = 't',
                                 scrollX = TRUE))
  })
  
  output$cameras_preview <- renderDataTable({
    cam_head <- utils::head(dat()$data$deployments, 5)
    
    DT::datatable(cam_head,
                  filter = "none",
                  selection = "none",
                  options = list(dom = 't',
                                 scrollX = TRUE))
  })
  
  
}