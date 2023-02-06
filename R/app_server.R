
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
      # Get file
      file <- input$records_input
      req(file)
      
      file_path <- file$datapath
      
      # Get separator value
      sep <- input$records_sep
      
      # Read csv
      res_records <- read_csv(file_path = file_path, 
                              column_separator = sep)

      # Update file separator
      updateRadioButtons(inputId = "records_sep",
                         selected = res_records$sep)

      if (input$import_cameras) {
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
      } else {
        res_cameras <- NULL
      }
      
      res <- list(data = list(observations = res_records$dat,
                              deployments = res_cameras$dat))
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