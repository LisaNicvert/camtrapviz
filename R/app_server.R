data()
server <- function(input, output) {
  
  output$dyntext <- renderText({
    if(input$example_file == "mica") {
      "Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany (camtrapDP format)"
    } else {
      "Sample dataset from the camtrapR package (camtrapR format)"
    }
  })
  
  dat <- reactive({
    if (input$input_type == 1) {
      if(input$example_file == "mica") {
        utils::data(mica, package = "camtraptor")
        res <- list(records = mica$data$observations,
                    cameras = mica$data$deployments)
      } else {
        utils::data(recordTableSample, package = "camtrapR")
        utils::data(camtraps, package = "camtrapR")
        res <- list(records = recordTableSample,
                    cameras = camtraps)
      }
    } else if (input$input_type == 2) {
      file <- input$records_input
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      utils::read.csv(file$datapath)
    }
    return(res)
  })
  
  output$recordsipsum <- renderTable({
    # utils::data(recordTableSample, package = "camtrapR")
    # df <- recordTableSample %>% select(Species,
    #                                    Station,
    #                                    DateTimeOriginal)
    # utils::head(df)
    utils::head(dat()$records)
  })
  
  output$camerasipsum <- renderTable({
    # utils::data(camtraps, package = "camtrapR")
    # df <- camtraps %>% select(Station,
    #                           utm_x,
    #                           utm_y)
    # utils::head(df)
    utils::head(dat()$cameras)
  })
  
  
}