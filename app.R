# Launch the ShinyApp (Do not remove this comment)
# for the LBBE shiny server

shiny::addResourcePath('www', system.file("app/www", package = "camtrapviz"))

camtrapviz::run_camtrapviz()
