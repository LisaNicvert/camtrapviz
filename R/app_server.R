data()
server <- function(input, output) {
  
  output$dyntext <- renderText({
    if(input$example_file == "mica") {
      "Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany (camtrapDP format)"
    } else {
      "Sample dataset from the camtrapR package (camtrapR format)"
    }
  })
  
  output$tab <- renderDataTable({
    file <- input$input
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    utils::read.csv(file$datapath)
  })
  
  output$recordsipsum <- renderTable({
    utils::data(recordTableSample, package = "camtrapR")
    df <- recordTableSample %>% select(Species,
                                       Station,
                                       DateTimeOriginal)
    utils::head(df)
  })
  
  output$camerasipsum <- renderTable({
    utils::data(camtraps, package = "camtrapR")
    df <- camtraps %>% select(Station,
                              utm_x,
                              utm_y)
    utils::head(df)
  })
  
  
}