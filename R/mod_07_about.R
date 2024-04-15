# UI ------------------------------------------------------------------
aboutUI <- function(id) {
  tagList(
    p("Camtrapviz is a free and open-source web application and 
      R package that I developed during my PhD (between 2023 and 2024)
      using R Shiny."),
    p(tagList("The source code is available at",
              a("https://github.com/LisaNicvert/camtrapviz"),
              "and documentation for the package is available",
              a("here", href="https://lisanicvert.github.io/camtrapviz/"), 
            "(created with pkgdown).")),
    p(tagList("To report bugs or request new features, please",
            a("open an issue on GitHub", href="https://github.com/LisaNicvert/camtrapviz/issues"), 
            ".")),
    p(tagList("To cite this app, use:",
              shiny::htmlOutput(NS(id, "cite"))))
    
  )
}

# Server ------------------------------------------------------------------
aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$cite <- renderText({
      cit <- citation(package = "camtrapviz")
      format(cit, "html")
      })
  })
}