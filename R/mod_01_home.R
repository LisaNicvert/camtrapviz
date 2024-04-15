# UI ----------------------------------------------------------------------

homeUI <- function(id) {
  tagList(
    br(),
    p("camtrapviz is an application and R package to visualize camera trap data. 
      It is intended for data where species have already been annotated and compiled in tables."),
    h3("Analysis modules"),
    p("The different modules allow to import, filter and analyze camera trap data.
      First, you must choose data with the import module. All other modules are optional
      and can be run in any order. To launch the modules, navigate the sidebar on the left."),
    img(src = 'www/camtrapviz.png', width = "100%", 
        style = "margin-bottom: 1.5em; margin-top: 1.5em;"),
    p("First, you must import data from the included example files or from your own dataset.
      Then, you can filter data based on species, cameras of date range.
      The overview module provides general information about the data, 
      such as the number of cameras, the survey duration or a map.
      The two last modules allow to get more information on the detected species (abundance and diversity)
      or on a particular, user-chosen species (activity pattern, detections by camera)."),
    h3("Code export"),
    p("For each analysis, a button allows to show the R code to reproduce the analysis.
      The complete report with all analyses can also be exported as a R Markdown 
      and htlm document."),
    br(),
    htmltools::hr(class = "hrthin"),
    img(src = "www/LBBE-Logotype-Version-baseline-CMJN.png", height = "40px")
  )
}

# Server ----------------------------------------------------------------------

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}