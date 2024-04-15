
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
  
  # About -------------------------------------------------------------
  aboutServer("about") 

  # Download handler --------------------------------------------------------
  
  # Get raw data
  raw_data <- reactive({
    res <- import_val$raw_data_info()
    res <- res[c("rec_path", "cam_path")]
    
    if (!is.null(res$rec_path)) { # Data was imported
      # Get extension
      ext <- tools::file_ext(res$rec_path)
      
      if (ext == "json") {
        # If it is a datapackage, return empty list
        res <- list()
      } else { 
        # Else if not a datapackage
        if (is.null(res$cam_path)) {
          # If there is no camera file
          res <- res[1]
          names(res) <- c("raw_records.csv")
        } else {
          # There is a camera and a records file
          names(res) <- c("raw_records.csv", "raw_cameras.csv")
        }
      }
    } else { # Data imported via R
      res = list()
    }
    res
  })
  
  
  output$download_script <- downloadHandler(
    filename = "camtrapviz.zip", 
    content = function(file) {
      ec <- shinymeta::newExpansionContext()
      
      # Replace files names in expression when source files are included in the zip folder
      if (length(raw_data() != 0)) { # Case csv file(s) were imported
        if (length(raw_data()) == 2) { # Records AND cameras imported
          ec$substituteMetaReactive(import_val$read_data, function() {
            metaExpr(read_data("raw_records.csv", 
                               sep_rec = ..(import_val$raw_data_info()$rec_sep),
                               "raw_cameras.csv",
                               sep_cam = ..(import_val$raw_data_info()$cam_sep)))
          })
        } else if (length(raw_data()) == 1) { # Only records imported
          ec$substituteMetaReactive(import_val$read_data, function() {
            metaExpr(read_data("raw_records.csv", 
                               sep_rec = ..(import_val$raw_data_info()$rec_sep)))
          })
        }
      }
      
      shinymeta::buildRmdBundle(
        report_template = system.file("Rmd/report.Rmd",
                                      package = "camtrapviz"),
        output_zip_path = file, 
        vars = list(data_reading = expandChain(invisible(import_val$read_data()), 
                                                .expansionContext = ec),
                    data_cleaning = expandChain(invisible(import_val$camtrap_data()), 
                                                .expansionContext = ec),
                    data_filtering = expandChain(invisible(select_val$camtrap_data()),
                                                 .expansionContext = ec),
                    filtering_plot = expandChain(select_val$filtering_plot(),
                                                 .expansionContext = ec),
                    ncameras = summary_val$ncameras(),
                    nspecies = summary_val$nspecies(),
                    sampling_length = summary_val$sampling_length(),
                    daterange = summary_val$daterange(),
                    camtable = expandChain(summary_val$camtable(),
                                           .expansionContext = ec),
                    spptable = expandChain(summary_val$spptable(),
                                           .expansionContext = ec),
                    has_lonlat = summary_val$has_lonlat(),
                    plot_map_all = expandChain(summary_val$plot_map(),
                                               .expansionContext = ec),
                    plot_occurrences = expandChain(summary_val$plot_occurrences(),
                                .expansionContext = ec),
                    count_unit = allspecies_val$count_unit(),
                    plot_species_bars = expandChain(allspecies_val$species_bars(),
                                                    .expansionContext = ec),
                    diversity_table = expandChain(allspecies_val$diversity_table(),
                                                  .expansionContext = ec),
                    diversity_index = allspecies_val$diversity_index(),
                    plot_diversity = expandChain(allspecies_val$diversity_plot(),
                                                  .expansionContext = ec),
                    focus_spp = onespecies_val$focus_spp(),
                    focus_spp_records = expandChain(onespecies_val$focus_spp_records(),
                                                    .expansionContext = ec),
                    density_plot = expandChain(onespecies_val$density_plot(),
                                               .expansionContext = ec),
                    abundance_value = onespecies_val$abundance_value(),
                    abundance_plot = expandChain(onespecies_val$abundance_plot(),
                                                .expansionContext = ec)
                    ),
        render_args = list(output_format = "html_document"),
        include_files = raw_data()
      )
    }
  )
}