
#' Server
#'
#' @param input Shiny input
#' @param output Shiny output
#'
#' @return The server generating functions for Shiny
#' @export
server <- function(input, output) {
  

# Load example data -------------------------------------------------------
  utils::data(mica, package = "camtraptor")
  utils::data(recordTableSample, package = "camtrapR")
  utils::data(camtraps, package = "camtrapR")
  
  output$dyntext <- renderText({
    if(input$example_file == "mica") {
      "Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany (camtrapDP format)"
    } else {
      "Sample dataset from the camtrapR package (camtrapR format)"
    }
  })
  

# Data --------------------------------------------------------------------
  dat <- reactive({
    if (input$input_type == 1) {
      if(input$example_file == "mica") {
        res <- mica
      } else {
        res <- list(data = list(observations = recordTableSample,
                                deployments = camtraps))
      }
    } else if (input$input_type == 2) {
      file <- input$records_input
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      if (is.null(input$records_sep)) { # Unspecified file separator
        # Default to comma separator
        fsep <- ","
        records <- utils::read.csv(file$datapath, sep = fsep)
        
        # Try tab
        if (ncol(records) == 1) {
          fsep <- "\t"
          records <- utils::read.csv(file$datapath, sep = fsep)
        }
        # Try semicolon
        if (ncol(records) == 1) {
          fsep <- ";"
          records <- utils::read.csv(file$datapath, sep = fsep)
        }
        # Display warning
        if (ncol(records) == 1) {
          warning("Only one column detected: check file separator")
        }
        
        # Update file separator
        updateRadioButtons(inputId = "records_sep",
                           selected = fsep)
        
      } else { # File separator is specified
        records <- utils::read.csv(file$datapath, sep = input$records_sep)
      }
      
      if (input$import_cameras) {
        cameras <- NA # to code
      } else {
        cameras <- NULL
      }
      
      res <- list(data = list(observations = records,
                              deployments = cameras))
    }
    return(res)
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