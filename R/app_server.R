data()
server <- function(input, output) {
  
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