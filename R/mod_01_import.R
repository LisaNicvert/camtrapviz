# UI ----------------------------------------------------------------------

importUI <- function(id) {
  # Create namespace variable
  ns <- NS(id)
  tagList(
    # Input type --------------------------------------------------------------
    radioButtons(NS(id, "input_type"),
                 label = "How to you want to import data?",
                 choices = list("Load example file" = 1,
                                "Upload file" = 2),
                 inline = TRUE,
                 selected = 1),
    br(),

# Example files widgets ---------------------------------------------------

    conditionalPanel(condition = "input.input_type == 1", ns = ns,
                     fluidRow(column(4,
                                     selectInput(NS(id, "example_file"), "Dataset",
                                                 choices = c("mica", "recordTableSample"))),
                              column(8,
                                     style = "margin-top: 30px;", # To align with selectInput
                                     textOutput(NS(id, "dyntext"))),
                     ),
                     br()
    ),

# Upload files widgets ----------------------------------------------------

conditionalPanel(condition = "input.input_type == 2", ns = ns,
                 fluidRow(column(6,
                                 h4("Records table"),
                                 htmltools::div(
                                   class = "upload_head",
                                   shinyFilesButton(NS(id, 'records_input'), 
                                                    style = "margin-bottom: 25px",
                                                    label = 'Choose records table', 
                                                    title = 'Choose file',
                                                    multiple = FALSE)
                                 ),
                                 uiOutput(NS(id, "records_col"))
                                 ),
                          column(6,
                                 h4("Cameras table"),
                                 htmltools::div(
                                   class = "upload_head",
                                   conditionalPanel(condition = "output.records_extension !== 'json'",
                                                    ns = ns,
                                                    # Display only if it is not a JSON file
                                                    checkboxInput(NS(id, "import_cameras"),
                                                                  "Import cameras table"),
                                                    
                                   ),
                                   conditionalPanel(condition = "input.import_cameras && output.records_extension !== 'json'",
                                                    ns = ns,
                                                    # Display only if it is not a JSON file and user wants to import a camera
                                                    shinyFilesButton(NS(id, "cameras_input"), 
                                                                     style = "margin-bottom: 25px",
                                                                     label = 'Choose cameras table', 
                                                                     title = 'Choose file',
                                                                     multiple = FALSE))
                                   ),
                                 conditionalPanel(condition = "input.import_cameras || output.records_extension === 'json'",
                                                  ns = ns,
                                                  # Display if user wants to import a camera or if it is a json file
                                                  uiOutput(NS(id, "cameras_col"))
                                 )
                                 ) # Cameras column
                          ),
                 br()
                 ), # conditionalPanel upload file widgets

# File previews -----------------------------------------------------------
                 conditionalPanel(condition = "input.input_type == 1 || input.records_input !== 0",
                                  ns = ns,
                                  tabsetPanel(
                                    tabPanel("Raw data preview",
                                              h4("Records table"),
                                              dataTableOutput(NS(id, "raw_records")),
                                              conditionalPanel(condition = "input.input_type == 1 || input.import_cameras || output.records_extension === 'json'",
                                                               ns = ns,
                                                               h4("Cameras table"),
                                                               dataTableOutput(NS(id, "raw_cameras"))
                                              )
                                            
                                    ),
                                    tabPanel("Cleaned data preview",
                                             actionButton(NS(id, "code_import"), 
                                                          "Show data cleaning code", icon("code"),
                                                          style = "margin-top: 25px; margin-bottom: 15px;"),
                                             h4("Records table"),
                                             dataTableOutput(NS(id, "records")),
                                             h4("Cameras table"),
                                             dataTableOutput(NS(id, "cameras")),
                                             downloadButton(NS(id, "downolad_cleaned_data"),
                                                            label = "Downolad cleaned data")
                                    )
                                 ) # End tabsetPanel
                 ) # End conditional panel
    ) # End taglist
}


# Server ------------------------------------------------------------------

importServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
# Server functions -----------------------------------------------------------
    
    # Update selected columns
    #
    # @param widget_list A vector of widget names you want to update
    # @param default A named list, names are the widget names
    # and values are the default for this widget selected among "choices"
    # @param choices The available choices to display in the selectInput
    # (if empty_allowed, nullval will also be added.)
    # @param empty_allowed A vector of widget names that are allowed to be empty
    # @param nullval Character, the placeholder to use in the choices in case 
    # empty_allowed.
    #
    # @return Updates the widgets defined with widget_list
    update_selected_columns <- function(widget_list, 
                                        default,
                                        choices,
                                        empty_allowed,
                                        nullval) {
      
      for(w in widget_list) {
        # Get default
        default_name <- default[[w]]
        
        if(w %in% empty_allowed) {
          updateSelectInput(session = session,
                            inputId = w,
                            choices = c(nullval, choices),
                            selected = default_name)
        } else {
          updateSelectInput(session = session,
                            inputId = w,
                            choices = choices,
                            selected = default_name)
        }
      }
    }
    
    # Create a widget
    # 
    # @param wid widget ID
    # @param label widget label
    # @param details details to display when hovering (i)
    # @param empty_allowed are empty values allowed? if yes, 
    # will add "(optional)" to tha label
    # @param selectize use selectizeInput insetad of selectInput?
    # @param ... additional arguments passed to selectInput
    # 
    # @return A selectInput element
    create_widget <- function(wid, label, details, empty_allowed, 
                              selectize = FALSE, width = 12,
                              class = "nomargin",
                              ...) {
      if (empty_allowed) {
        # Add "optional" in label
        label <- paste(label, "(optional)")
      }
      
      selectwidth = "100%"
      
      if (selectize) {
        res <- column(width,
                      class = class,
                      selectizeInput(NS(id, wid),
                              label = htmltools::tags$span(label,
                                                           htmltools::tags$div(
                                                             class = "mytooltip",
                                                             icon("circle-info"),
                                                             htmltools::tags$p(class = "mytooltiptext",
                                                                               details)
                                                           )), 
                              choices = NULL,
                              width = selectwidth,
                              ...))
      } else {
        res <- column(width,
                      class = class,
                      selectInput(NS(id, wid),
                          label = htmltools::tags$span(label,
                                                      htmltools::tags$div(
                                                        class = "mytooltip",
                                                        icon("circle-info"),
                                                        htmltools::tags$p(class = "mytooltiptext",
                                                                          details)
                                                      )), 
                          choices = NULL,
                          width = selectwidth,
                          ...))
      }
      
      return(res)
    }
    
    # Create a list of widgets
    # 
    # @param df A dataframe with columns
    #   widget, label, empty_allowed, details
    # @param ... additional arguments passed to selectInput
    # 
    # @return A list of selectInput
    create_widget_list <- function(df, selectize = FALSE,
                                   width = rep(12, nrow(df)),
                                   class = rep("nomargin", nrow(df)),
                                   ...) {
      n <- nrow(df)
      res <- vector(mode = "list", length = n)
      
      for (i in 1:n){
        wid <- df$widget[i]
        label <- df$label[i]
        details <- df$details[i]
        empty_allowed <- df$empty_allowed[i]
        widthi <- width[i]
        classi <- class[i]
        res[[i]] <- create_widget(wid, label, details, empty_allowed,
                                  selectize = selectize,
                                  width = widthi,
                                  class = classi)
      }
      return(res)
    }
    
    # Get mapping
    # 
    # Get the mapping to apply for columns from currently selected values
    # 
    # @param mapping_list A list (initialized to NULL)
    # for which we want to update the values
    # @param relevant_widgets The widgets to look at in the app to update values
    # @param nullval The placeholder when the column is not present in the data
    # @param example_mapping List for the example mapping (names of the list must correspond to 
    # possible values of input$example_file)
    #
    # @return A list named as the widgets containing their current value
    # if this value is nullval then it is set to NULL.
    get_mapping <- function(mapping_list, 
                            relevant_widgets, 
                            nullval,
                            example_mapping) {
      res <- mapping_list
      if (input$input_type == 1) { # Example files
        # Get known mapping
        ex <- example_mapping[[input$example_file]]
        res[names(ex)] <- ex
        
      } else if (input$input_type == 2) { # Uploaded files
        # Get the values selected by the user
        
        # Update mapping values for those widgets
        for (i in 1:length(res)) {
          wname <- relevant_widgets[i]
          wval <- input[[wname]]
          
          if (!is.null(wval)) {
            # Replace placeholder with NULL
            if (wval != nullval) {
              res[wname] <- wval
            } else {
              res[wname] <- list(NULL)
            }
          }
          # Else do nothing and just leave the value to NULL
        }
      }
      return(res)
    }
    
# Setup variables ---------------------------------------------------------
    
    # Define roots for ShinyFiles 
    roots <- c("home" = fs::path_home())
    
    # Define placeholder for optional columns
    nullval <- "Not present in data"
    
    # Define dates formats to use un tryFormats
    dates_formats <- c("%Y-%m-%d",
                       "%Y/%m/%d",
                       "%d-%m-%Y",
                       "%d/%m/%Y",
                       "%m-%d-%Y",
                       "%m/%d/%Y")
    datetimes_formats <- c("%Y-%m-%d %T",
                           "%Y/%m/%d %T",
                           "%Y-%m-%d %H:%M",
                           "%Y/%m/%d %H:%M",
                           "%Y-%m-%dT%T",
                           "%Y/%m/%dT%T",
                           "%Y-%m-%dT%H:%M",
                           "%Y/%m/%dT%H:%M")
    
    
# Useful variables from widgets df ----------------------------------------
    
    # Save the complete df
    records_widgets_all <- records_widgets
    cameras_widgets_all <- cameras_widgets
    
    # Get widgets that correspond to columns to choose in the data
    records_widgets <- records_widgets |>
      filter(in_columns == TRUE)
    cameras_widgets <- cameras_widgets |>
      filter(in_columns == TRUE)
    
    # Get time widgets
    time_widgets <- records_widgets |> 
      dplyr::filter(type %in% c("date_time", "timestamp"))
    time_widgets <- time_widgets$widget
    
    # All widgets
    all_records_widgets <- records_widgets$widget
    all_cameras_widgets <- cameras_widgets$widget
    
    # Define columns for which empty is allowed
    allowed_records <- records_widgets |>
      filter(empty_allowed == TRUE)
    allowed_records <- allowed_records$widget
    
    allowed_cameras <- cameras_widgets |>
      filter(empty_allowed == TRUE)
    allowed_cameras <- allowed_cameras$widget
    empty_allowed <- c(allowed_records, 
                       allowed_cameras)
    
    # Define camera columns
    cameras_to_update <- cameras_widgets
    cameras_to_update <- cameras_to_update$widget
    
# Define mappings for example datasets ------------------------------------

    example_mapping_records <- list(mica = get_example_mapping(records_widgets, 
                                                               "mica"),
                                    recordTableSample = get_example_mapping(records_widgets, 
                                                                            "recordTableSample"))
    example_mapping_cameras <- list(mica = get_example_mapping(cameras_widgets, 
                                                               "mica"),
                                    recordTableSample = get_example_mapping(cameras_widgets, 
                                                                            "recordTableSample"))

# Description for example datasets ----------------------------------------

    output$dyntext <- renderText({
      if(input$example_file == "mica") {
        "Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany (camtrapDP format)"
      } else {
        "Sample dataset from the camtrapR package (camtrapR format)"
      }
    })

# Create records widgets ----------------------------------------------------------

## Create automated records ------------------------------------------------
    records_widgets_w <- create_widget_list(records_widgets_all,
                                            width = records_widgets_all$width,
                                            class = records_widgets_all$class,
                                            selectize = TRUE)
    names(records_widgets_w) <- records_widgets_all$widget
    
## Create data/datetime radio buttons --------------------------------------
    
    # Create radio button
    radio <- tagList("datetime_or_timestamp" = column(12, 
                                                      class = "nomargin",
                                                      radioButtons(NS(id, "datetime_or_timestamp"),
                                                                   "Date / time column(s)",
                                                                   choices = c("Date and time" = "date_time",
                                                                               "Timestamp" = "timestamp"), 
                                                                   inline = TRUE)
                                                      )
                     )
    
## Finalize widget list ----------------------------------------------------
    
    # Update timezones widget value
    updateSelectizeInput(session = session,
                         "tz_col",
                         choices = tz_choices,
                         selected = "Etc/GMT",
                         server = TRUE)
    

    #  Append radio
    records_widgets_w <- append(records_widgets_w, 
                                radio,
                                after = 2)
    
    # Add date/datetime conditional panel
    records_widgets_w[["date_col"]] <- conditionalPanel(condition = "input.datetime_or_timestamp == 'date_time'", 
                                                        ns = NS(id),
                                                        records_widgets_w[["date_col"]]
                                                        )
    records_widgets_w[["time_col"]] <- conditionalPanel(condition = "input.datetime_or_timestamp == 'date_time'", 
                                                        ns = NS(id),
                                                        records_widgets_w[["time_col"]]
                                                        )
    records_widgets_w[["timestamp_col"]] <- conditionalPanel(condition = "input.datetime_or_timestamp == 'timestamp'", 
                                                             ns = NS(id),
                                                             records_widgets_w[["timestamp_col"]]
    )

    # Add cameras conditional panel
    camera_records_widget <- records_widgets_all |> 
      filter(type == "cameras")
    camera_records_widget <- camera_records_widget$widget
    
    records_widgets_w[camera_records_widget] <- lapply(records_widgets_w[camera_records_widget],
                                                       function(e) {
                                                         conditionalPanel(condition = "!input.import_cameras && output.records_extension !== 'json'",
                                                                          ns = NS(id),
                                                                          e)}
                                                       )

    # Render UI
    output$records_col <- renderUI(records_widgets_w)
    
# Create cameras widgets --------------------------------------------------
    
    # Create widgets
    cameras_widgets_w <- create_widget_list(cameras_widgets_all,
                                            width = cameras_widgets_all$width,
                                            class = cameras_widgets_all$class,
                                            selectize = TRUE)
    names(cameras_widgets_w) <- cameras_widgets_all$widget
    
    # Render UI
    output$cameras_col <- renderUI(cameras_widgets_w)
    

# Update CRS --------------------------------------------------------------

    observe(
      if (input$input_type == 2) { # We import data
        
        if (input$import_cameras || records_extension() == "json") { # There is no camera file
          lon_col <- input$lon_col_cov
          lat_col <- input$lat_col_cov
          crs_id <- "crs_col_cov"
        } else { # There is a camera file
          lon_col <- input$lon_col
          lat_col <- input$lat_col
          crs_id <- "crs_col"
        }
        
        if(lon_col == nullval & lat_col == nullval) {
          updateSelectizeInput(session = session,
                               crs_id,
                               choices = NULL,
                               options = list(placeholder = "No CRS needed"))
        } else {
          updateSelectizeInput(session = session,
                               crs_id,
                               choices = epsg,
                               selected = "4326",
                               server = TRUE)
        }
      }
      
    ) |> bindEvent(input$lat_col, input$lon_col,
                   input$lat_col_cov, input$lon_col_cov) 
    
    
# Read files --------------------------------------------------------------


## Shiny file listener -----------------------------------------------------

    shinyFileChoose(input, 'records_input', 
                    root = c("home" = fs::path_home()),
                    filetypes = c('csv', 'json'))
    
    shinyFileChoose(input, 'cameras_input', 
                    root = c("home" = fs::path_home()),
                    filetypes = 'csv')
    
## Get imported file extension ---------------------------------------------

    records_extension <- reactive({
      # Get file
      file <- shinyFiles::parseFilePaths(roots,
                                         input$records_input)
      
      # Ensure file is loaded
      file_path <- file$datapath
      validate(need(file_path != '', "Please upload file"))
      
      # Get file extension
      ext <- tools::file_ext(file_path)
      ext
    })
    
    output$records_extension <- reactive({
      records_extension()
    })
    outputOptions(output, 'records_extension', 
                  suspendWhenHidden = FALSE)
    
  
  ## Get raw data ------------------------------------------------------------
    
    dat_raw <- metaReactive2({
      if (input$input_type == 1) { # Example dataset
        if(input$example_file == "mica") {
          
          metaExpr({
            utils::data(mica, package = "camtraptor")
            
            mica
          }, bindToReturn = TRUE)
        } else {
          
          metaExpr({
            utils::data(recordTableSample, package = "camtrapR")
            utils::data(camtraps, package = "camtrapR")
            
            list(data = list(observations = recordTableSample,
                             deployments = camtraps))
          }, bindToReturn = TRUE)
          
        }
      } else if (input$input_type == 2) { # Uploaded dataset
        # Get file
        file <- shinyFiles::parseFilePaths(roots,
                                           input$records_input)
        # Ensure file is loaded before continuing
        req(file)
        
        # Get file_path
        file_path <- unname(file$datapath)

        validate(need(file_path != '', "Please upload a records file"))
        
        # Get input separator values
        sep_records <- input$records_sep
        sep_cameras <- input$cameras_sep
        
        # User wants to import a camera file?
        if (input$import_cameras) {
          # Get file
          camera_file <- shinyFiles::parseFilePaths(roots,
                                                    input$cameras_input)
          
          # Ensure file is loaded before continuing
          req(camera_file)
          
          # Get camera_path
          cameras_path <- unname(camera_file$datapath)
          validate(need(cameras_path != '', "Please upload camera file"))
          
        } else {
          cameras_path <- NULL
        }
        
        # Try to automate separator values
        if (is.null(sep_records)) {
          L <- readLines(file_path, n = 1)
          sep_records <- get_separator(L)
        }
        
        if (!is.null(cameras_path)) {
          if (is.null(sep_cameras)) {
            L <- readLines(cameras_path, n = 1)
            sep_cameras <- get_separator(L)
          }
        }
        
        # Update records separator
        updateRadioButtons(inputId = "records_sep",
                           selected = sep_records)
        
        # Update camera separator
        updateRadioButtons(inputId = "cameras_sep",
                           selected = sep_cameras)
        
        # Read data
        metaExpr({
          read_data(..(file_path), 
                    ..(sep_records),
                    ..(cameras_path),
                    ..(sep_cameras))
        }, bindToReturn = TRUE)
      }
    }, varname = "dat_raw")

# UI settings -------------------------------------------------------------
  
    observe({
      # Set camera import to false if we have a json file
      if (records_extension() == "json") {
        updateCheckboxInput(session = session,
                            "import_cameras", 
                            value = FALSE)
      }
    })
  
# Records columns ---------------------------------------------------------
  
    # Get input data columns
    records_cols <- reactive({
      colnames(dat_raw()$data$observations)
    })
    
    # Get the columns we want to update for the records
    records_to_update <- reactive({
      # Initialize widgets all data
      widget_df <- records_widgets
      
      if (input$datetime_or_timestamp == 'timestamp') {
        # Filter out date/time
        widget_df <- widget_df |> 
          dplyr::filter(type != "date_time")
      } else if (input$datetime_or_timestamp == 'date_time') {
        # Filter out timestamp
        widget_df <- widget_df |> 
          dplyr::filter(type != "timestamp")
      }
      
      if (input$import_cameras || records_extension() == "json") { 
        # User wants to import a camera file
        widget_df <- widget_df |> 
          dplyr::filter(type != "cameras")
      }
      widgets <- widget_df$widget
      widgets
    })
    
    # Default columns mapping for records
    default_records <- reactive({
      # Ensure records table was imported and is available 
      #   (i.e. no example data and data correctly loaded)
      req(input$records_input)
      
      validate(need(!(nullval %in% records_cols()),
                    paste("Please provide a dataframe with no column named",
                          nullval)))
      
      # Prepare regex vector
      regex <- get_named_list(records_widgets, 
                              col = "regex",
                              widget_values = records_to_update())

      # Find default names
      default_names <- find_default_colnames(regex_list = regex,
                                             colnames = records_cols(),
                                             empty_allowed_list = empty_allowed,
                                             empty_placeholder = nullval)
      default_names
    })
    

## Update selectInput ------------------------------------------------------

    # Update selection list and default names in selectInput for records
    observeEvent(input$records_input, {
      if (input$input_type == 2) { # Only update widgets for manual import
        update_selected_columns(widget_list = records_to_update(), 
                                default = default_records(),
                                choices = records_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    
    # Update choices upon separator change
    observeEvent(input$records_sep, {
      if (input$input_type == 2) { # Only update widgets for manual import
        
        update_selected_columns(widget_list = records_to_update(), 
                                default = default_records(),
                                choices = records_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    
    # Update date/time/stamp upon input change
    observeEvent(input$datetime_or_timestamp, {
      if (input$input_type == 2) { # Only update widgets for manual import
        
        # Subset time widgets which are in records to update
        time_widgets <- time_widgets[time_widgets %in% records_to_update()]
        
        update_selected_columns(widget_list = time_widgets, 
                                default = default_records(),
                                choices = records_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    

## Mapping -----------------------------------------------------------------
    
    # Mapping value for records columns
    mapping_records <- reactive({
      
      # Initialize NULL list
      res <- vector(mode = "list",
                    length = nrow(records_widgets))
      names(res) <- records_widgets$widget
      
      res <- get_mapping(mapping_list = res, 
                         relevant_widgets = records_to_update(), 
                         nullval = nullval,
                         example_mapping = example_mapping_records)
      
      # Get the mapping corresponding to cameras in records
      widget_cam <- records_widgets |>
        dplyr::filter(type == "cameras")
      widget_cam <- widget_cam$widget
      res <- res[-which(names(res) %in% widget_cam)]
      
      res  
    })
    
# Cameras columns ------------------------------------------------
    
    # Get input cameras columns
    cameras_cols <- reactive({
      if (!is.null(dat_raw()$data$deployments)) { # Camera file was provided
        res <- colnames(dat_raw()$data$deployments)
      } else { # Camera is in data
        res <- NULL
      }
      res
    })
    
    # Default columns mapping for cameras
    default_cameras <- reactive({
      # Find default names
      if (!is.null(cameras_cols())) {
        # Prepare regex vector
        regex <- get_named_list(cameras_widgets, 
                                col = "regex",
                                widget_values = cameras_to_update)
        # Find default names
        default_names <- find_default_colnames(regex_list = regex,
                                               colnames = cameras_cols(),
                                               empty_allowed_list = empty_allowed,
                                               empty_placeholder = nullval)
      } else {
        default_names <- NULL
      }
      default_names
    })
    

## Update selectInput ------------------------------------------------------

    # Update selection list and default names 
    # in selectInput for cameras
    observe({
      if (input$input_type == 2) { # Only update widgets for manual import
        if (!is.null(dat_raw()$data$deployments)) { # Camera file was provided
          update_selected_columns(widget_list = cameras_to_update, 
                                  default = default_cameras(),
                                  choices = cameras_cols(),
                                  empty_allowed = empty_allowed,
                                  nullval = nullval)
        }
      }
    })
    
    # Update choices upon separator change
    observeEvent(input$cameras_sep, {
      if (input$input_type == 2) { # Only update widgets for manual import
        update_selected_columns(widget_list = cameras_to_update, 
                                default = default_cameras(),
                                choices = cameras_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    

## Mapping -----------------------------------------------------------------
    
    # Mapping value for cameras columns extracted from the camera file
    mapping_cameras <- reactive({
      # Initialize NULL list
      res <- vector(mode = "list",
                    length = length(all_cameras_widgets))
      
      # Names for res are the camera widget names (with "cov")
      names(res) <- all_cameras_widgets
      
      if(!is.null(cameras_cols())) { # user imported a camera file
        widgets_to_look_at <- cameras_to_update
        if (input$input_type != 1) {
          source <- "cameras"
        } else {
          source <- "example"
        }
      } else { # No camera file was imported
        # Names for res are the records widget names
        names(res) <- gsub("_cov$", "", names(res))
        
        # Get the mapping corresponding to cameras in records
        widget_cam <- records_widgets |>
          dplyr::filter(type == "cameras")
        widget_cam <- widget_cam$widget
        # Add cam_col
        widgets_to_look_at <- c(widget_cam, "cam_col")
        if (input$input_type != 1) {
          source <- "records"
        } else {
          source <- "example"
        }
      }
      
      # Look for values in the relevant widgets
      res <- get_mapping(mapping_list = res, 
                         relevant_widgets = widgets_to_look_at, 
                         nullval = nullval,
                         example_mapping = example_mapping_cameras)
      
      # Remove _cov for consistency
      names(res) <- gsub("_cov$", "", names(res))
      
      list(mapping = res,
           source = source)
    })
    

## CRS ---------------------------------------------------------------------
    
    crs <- reactive({
      if (input$input_type == 1) { # Example file
        if(input$example_file == "mica") {
          res <- cameras_widgets_all$mica[cameras_widgets_all$widget == "crs_col_cov"]
        } else {
          res <- cameras_widgets_all$recordTableSample[cameras_widgets_all$widget == "crs_col_cov"]
        }
      } else if (input$input_type == 2) { # Manual import file
        if (input$import_cameras || records_extension() == "json") { # Import a camera file
          res <- input[["crs_col_cov"]]
        } else { # Don't import a camera file
          res <- input[["crs_col"]]
        }
      }
      return(as.numeric(res))
    })
    

# Update latitude/longitude -----------------------------------------------
    
    # Update longitude
    observe({
      if (input$input_type == 2) { # Manual file import
        
        if (input$import_cameras || records_extension() == "json") { # There is a camera file
          lat_col <- input$lat_col_cov
          lon_id <- "lon_col_cov"
        } else { # No camera file
          lat_col <- input$lat_col
          lon_id <- "lon_col"
        }
        
        # Update longitude if latitude is NULL
        if (lat_col == nullval) {
          updateSelectInput(session = session,
                            lon_id, 
                            selected = nullval)
        }
      }
    }) |> bindEvent(input$lat_col, input$lat_col_cov)
    
    # Update latitude
    observe({
      if (input$input_type == 2) { # Manual file import
        
        if (input$import_cameras || records_extension() == "json") { # There is a camera file
          lon_col <- input$lon_col_cov
          lat_id <- "lat_col_cov"
        } else { # No camera file
          lon_col <- input$lon_col
          lat_id <- "lat_col"
        }
        
        # Update latitude if longitude is NULL
        if (lon_col == nullval) {
          updateSelectInput(session = session,
                            lat_id, 
                            selected = nullval)
        }
      }
    }) |> bindEvent(input$lon_col, input$lon_col_cov)


# Timezone and dates formats handling -------------------------------------------------------
    
    tz <- reactive({
      if (input$input_type == 1) {
        if (input$example_file == "mica") {
          tz <- "Etc/GMT"
        } else if (input$example_file == "recordTableSample") {
          tz <- "Etc/GMT-8"
        }
      } else {
        tz <- input$tz_col
      }
      tz
    })
    
    fmt <- reactive({
      if (input$input_type == 1) { # Example files (known date format)
        if (input$example_file == "mica") {
          # No need to cast mica data which comes in the right format already
          fmt_date <- NULL
          fmt_posix_rec <- NULL
          fmt_posix_cam <- NULL
        } else if (input$example_file == "recordTableSample") {
          fmt_date <- "%d/%m/%Y"
          fmt_posix_rec <- "%Y-%m-%d %T"
          fmt_posix_cam <- "%d/%m/%Y"
        }
      } else { # Manual file input
        fmt_date <- dates_formats
        fmt_posix_rec <- datetimes_formats
        fmt_posix_cam <- c(datetimes_formats,
                           dates_formats) # Also add dates formats in case setup and retrieval are POSIX and not dates
      }
      
      list(date = fmt_date,
           posix_cam = fmt_posix_cam,
           posix_rec = fmt_posix_rec)
    })
    
# Clean data --------------------------------------------------------------
    
    dat <- metaReactive2({

      # Ensure data is available
      req(mapping_records())
      req(mapping_cameras()$mapping)

      validate(need(all(unname(unlist(mapping_records())) %in% colnames(dat_raw()$data$observations)),
                    "Wait a minute for the records :)"))
      if( mapping_cameras()$source != "records") {
        # If a file was imported
        validate(need(all(unname(unlist(mapping_cameras()$mapping)) %in% colnames(dat_raw()$data$deployments)),
                      "Wait a minute for the cameras :)"))
      }
      
      # Ensure latitude and longitude are both null or both provided
      lon <- mapping_cameras()$mapping$lon_col
      lat <- mapping_cameras()$mapping$lat_col
      validate(need((!is.null(lon) & !is.null(lat)) | (is.null(lon) & is.null(lat)), 
                    "If one of latitude or longitude is provided, the other must be provided."))
    
      
      if ( mapping_cameras()$source == "records" ) {
        split <- TRUE
      } else {
        split <- FALSE
      }
      
      # Get casting types ---
      # Records
      non_null_rec <- unlist(mapping_records())
      non_null_names <- names(non_null_rec)
      castval_rec <- get_named_list(records_widgets,
                                    col = "cast",
                                    widget_values = non_null_names)
      castval_rec <- castval_rec[non_null_names] # Reorder values
      names(castval_rec) <- unname(non_null_rec) # Rename with table column names
      
      # Cameras
      # /!\ Here we choose to look in the records table,
      # because the cast types should be the same in both tables
      # and mapping_cameras was renamed to remove the _col suffix
      non_null_cam <- unlist(mapping_cameras()$mapping)
      non_null_names <- names(non_null_cam)
      castval_cam <- get_named_list(records_widgets,
                                    col = "cast",
                                    widget_values = non_null_names)
      castval_cam <- castval_cam[non_null_names] # Reorder values
      names(castval_cam) <- unname(non_null_cam) # Rename with table column names
      
      # Add options to casting for dates
      dates_cast <- records_widgets |>
        filter(cast == "as.Date")
      dates_cast <- dates_cast$widget
      # Get column names
      dates_cast <- c(unname(unlist(mapping_records()[dates_cast])),
                      unname(unlist(mapping_cameras()$mapping[dates_cast]))) 
      # Add options to casting for POSIX
      posix_cast <- records_widgets |>
        filter(cast == "as.POSIXct")
      posix_cast <- posix_cast$widget
      posix_cast <- c(unname(unlist(mapping_records()[posix_cast])),
                      unname(unlist(mapping_cameras()$mapping[posix_cast]))) 
      
      # Add a casting type only if relevant
      if (!all(is.null(fmt()$date))) {
        castval_rec <- add_date_options(castval_rec,
                                        formats = fmt()$date,
                                        names_to_add = dates_cast)
        castval_cam <- add_date_options(castval_cam,
                                        formats = fmt()$date,
                                        names_to_add = dates_cast)
      }
      if (!all(is.null(fmt()$posix_rec)) || !is.null(tz()) ) {
        castval_rec <- add_date_options(castval_rec,
                                        formats = fmt()$posix_rec,
                                        tz = tz(),
                                        names_to_add = posix_cast)
      }
      if (!all(is.null(fmt()$posix_cam)) || !is.null(tz()) ) {
        castval_cam <- add_date_options(castval_cam,
                                        formats = fmt()$posix_cam,
                                        tz = tz(),
                                        names_to_add = posix_cast)
      }
      
      metaExpr({
        # Casting types variables ---
        castval_rec <- ..(castval_rec)
        castval_cam <- ..(castval_cam)
        
        # Camera column ---
        cam_col <- ..(unname(mapping_cameras()$mapping[["cam_col"]]))
        rec_col <- ..(unname(mapping_records()[["cam_col"]]))
        
        clean_data(dat = ..(dat_raw()), 
                   cam_type = castval_cam,
                   rec_type = castval_rec,
                   split = ..(split),
                   cam_col_cameras = cam_col,
                   cam_col_records = rec_col,
                   add_rowid = TRUE)
      }, bindToReturn = TRUE)
      
    }, varname = "dat")

# Print code --------------------------------------------------------------
    
    observeEvent(input$code_import, {
      code <- expandChain(dat())
      displayCodeModal(code,
                       title = "Data formatting code")
    })


# File input preview ------------------------------------------------------
    
## Raw data ----------------------------------------------------------------
    output$raw_records <- renderDataTable({
      DT::datatable(dat_raw()$data$observations,
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE))
    })
    
    output$raw_cameras <- renderDataTable({
      DT::datatable(dat_raw()$data$deployments,
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE))
    })
    
    
## Cleaned data ------------------------------------------------------------
    output$records <- renderDataTable({
      DT::datatable(dat()$data$observations,
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE)) |>
        DT::formatStyle(columns = unname(unlist(mapping_records())),
                        backgroundColor = '#F5EE9E')
    })
    
    output$cameras <- renderDataTable({
      DT::datatable(dat()$data$deployments,
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE)) |>
        DT::formatStyle(columns = unname(unlist(mapping_cameras()$mapping)),
                        backgroundColor = '#F5EE9E')
    })
    
    
# Downolad handler --------------------------------------------------------
    
    output$downolad_cleaned_data <- downloadHandler(
      filename = function(){
        paste0("camtrap_data_", Sys.Date(), ".zip")
        
      },
      content = function(file) {
        # definition of content to download
        to_dl <- list(
          # names to use in file names
          names = list(records = "records",
                       cameras = "cameras"),
          # data
          data = list(records = dat()$data$observations,
                      cameras = dat()$data$deployments)
        )
        
        # temp dir for the csv's as we can only create
        # an archive from existent files and not data from R
        twd <- setwd(tempdir())
        on.exit(setwd(twd))
        files <- NULL
        
        # loop on data to download and write individual csv's
        for (nam in c("records", "cameras")) {
          fileName <- paste0(to_dl[["names"]][[nam]], ".csv") # csv file name
          utils::write.csv(to_dl[["data"]][[nam]], fileName) # write csv in temp dir
          files <- c(files, fileName) # store written file name
        }
        
        # create archive from written files
        utils::zip(file, files)
      }
    )

# Return values -----------------------------------------------------------
    return(
      list(camtrap_data = dat,
           mapping_records = reactive(mapping_records()),
           mapping_cameras = reactive(mapping_cameras()$mapping),
           crs = reactive(crs()),
           tz = reactive(tz())
      )
    )
    
  })
}
