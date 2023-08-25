
#' Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @noRd
#' @return The server generating functions for Shiny
server <- function(input, output, session) {
  
  # Import reactives --------------------------------------------------------
  import_val <- importServer("import")
  
  # Test
  # output$mapping_records <- renderText({
  #   paste(paste(names(import_val$mapping_records())),
  #         paste(import_val$mapping_records()))
  # })
  # 
  # output$mapping_cameras <- renderText({
  #   paste(paste(names(import_val$mapping_cameras())),
  #         paste(import_val$mapping_cameras()))
  # })
  

  # Select data reactives ---------------------------------------------------
  select_val <- selectServer("select",
                             camtrap_data = import_val$camtrap_data,
                             mapping_records = import_val$mapping_records,
                             mapping_cameras = import_val$mapping_cameras,
                             tz = import_val$tz)
  

  # Summary reactives -------------------------------------------------------
  summary_val <- summaryServer("summary",
                               camtrap_data = select_val$camtrap_data,
                               mapping_records = import_val$mapping_records,
                               mapping_cameras = import_val$mapping_cameras,
                               crs = import_val$crs,
                               tz = import_val$tz)
  

  # All species -------------------------------------------------------------
  allspecies_val <- allspeciesServer("allspecies",
                                     camtrap_data = select_val$camtrap_data,
                                     mapping_records = import_val$mapping_records,
                                     mapping_cameras = import_val$mapping_cameras,
                                     cam_summary = summary_val$cam_summary,
                                     crs = import_val$crs) 

  # One species -------------------------------------------------------------
  onespecies_val <- onespeciesServer("onespecies",
                                     camtrap_data = select_val$camtrap_data,
                                     mapping_records = import_val$mapping_records,
                                     mapping_cameras = import_val$mapping_cameras,
                                     sppcam_summary = allspecies_val$sppcam_summary,
                                     crs = import_val$crs) 
  
  # Download handler --------------------------------------------------------
  
  output$download_script <- downloadHandler(
    filename = "camtrapviz.zip", 
    content = function(file) {
      ec <- shinymeta::newExpansionContext()
      shinymeta::buildRmdBundle(
        report_template = system.file("Rmd/report.Rmd",
                                      package = "camtrapviz"),
        output_zip_path = file, 
        vars = list(data_cleaning = expandChain(invisible(import_val$camtrap_data()), 
                                                .expansionContext = ec),
                    data_filtering = expandChain(invisible(select_val$camtrap_data()),
                                                 .expansionContext = ec),
                    camtable = expandChain(summary_val$camtable(),
                                           .expansionContext = ec),
                    spptable = expandChain(summary_val$spptable(),
                                           .expansionContext = ec),
                    plot_map = expandChain(summary_val$plot_map(),
                                           .expansionContext = ec),
                    plot_occurrences = expandChain(summary_val$plot_occurrences(),
                                .expansionContext = ec),
                    plot_species_bars = expandChain(allspecies_val$species_bars(),
                                                    .expansionContext = ec),
                    diversity_table = expandChain(allspecies_val$diversity_table(),
                                                  .expansionContext = ec),
                    plot_diversity = expandChain(allspecies_val$diversity_map(),
                                                  .expansionContext = ec),
                    focus_spp_records = expandChain(onespecies_val$focus_spp_records(),
                                                    .expansionContext = ec),
                    density_plot = expandChain(onespecies_val$density_plot(),
                                               .expansionContext = ec),
                    abundance_map = expandChain(onespecies_val$abundance_map(),
                                                .expansionContext = ec)
                    ),
        render_args = list(output_format = "html_document")
      )
    }
  )
}