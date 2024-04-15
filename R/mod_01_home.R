# UI ----------------------------------------------------------------------

homeUI <- function(id) {
  tagList(
    img(src='www/logo.png', height = "150px",width = "129.75px"), # w = h*0.865
    p("camtrapviz is an application to visualize camera trap data."),
    br(),
    img(src="www/LBBE-Logotype-Version-baseline-CMJN.png", height = "40px")
  )
}

# Server ----------------------------------------------------------------------

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}