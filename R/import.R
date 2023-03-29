importUI <- function(id) {
  # Create namespace variable
  ns <- NS(id)
  tagList(
    box(h2("Data import"),
        width = 12,

# Input type --------------------------------------------------------------
        radioButtons(NS(id, "input_type"),
                     label = h3("Input type"),
                     choices = list("Load example file" = 1,
                                    "Upload file" = 2),
                     inline = TRUE,
                     selected = 1),

# Example files widgets ---------------------------------------------------

        conditionalPanel(condition = "input.input_type == 1", ns = ns,
                         fluidRow(column(4,
                                         selectInput(NS(id, "example_file"), "Dataset",
                                                     choices = c("mica", "recordTableSample"))),
                                  column(8,
                                         style = "margin-top: 25px;", # To align with selectInput
                                         textOutput(NS(id, "dyntext")))
                         )
        ),

# Upload files widgets ----------------------------------------------------

    conditionalPanel(condition = "input.input_type == 2", ns = ns,
                     h4("Records table"),
                     fluidRow(column(4,
                                     shinyFilesButton(NS(id, 'records_input'), 
                                                      style = "margin-bottom: 25px",
                                                      label = 'Choose records table', 
                                                      title = 'Choose file',
                                                      multiple = FALSE),
                                     conditionalPanel(condition = "output.records_extension !== 'json'", ns = ns,
                                                      separator_widget(NS(id, "records")))
                                     ),
                              column(8,
                                     uiOutput(NS(id, "mandatory_records_col")),
                                     radioButtons(NS(id, "datetime_or_timestamp"),
                                                  "Date / time column(s)",
                                                  choices = c("Date and time" = "date_time",
                                                              "Timestamp" = "timestamp"), 
                                                  inline = TRUE),
                                     conditionalPanel(condition = "input.datetime_or_timestamp == 'date_time'", ns = ns,
                                                      uiOutput(NS(id, "datetime_records_col"))
                                                      ),
                                     conditionalPanel(condition = "input.datetime_or_timestamp == 'timestamp'", ns = ns,
                                                      uiOutput(NS(id, "timestamp_records_col"))
                                     ),
                                     conditionalPanel(condition = "!input.import_cameras && output.records_extension !== 'json'",
                                                      ns = ns,
                                                      uiOutput(NS(id, "ui_crs_records_col")),
                                                      uiOutput(NS(id, "lonlat_records_col")),
                                                      uiOutput(NS(id, "other_cov_records_col"))
                                                      ),
                                     uiOutput(NS(id, "optional_records_col"))
                                     ) # End column for records selectInput
                              ), # End fluidRow
                     br(),
                     h4("Cameras table"),
                     conditionalPanel(condition = "output.records_extension !== 'json'",
                                      ns = ns,
                                      checkboxInput(NS(id, "import_cameras"),
                                                    "Import cameras table")
                                      ),
                     conditionalPanel(condition = "input.import_cameras || output.records_extension === 'json'",
                                      ns = ns,
                                      # Display if user wants to import a camera or if it is a json file
                                      fluidRow(column(4,
                                                      conditionalPanel(condition = "output.records_extension !== 'json'",
                                                                       ns = ns,
                                                                       # Display only if not json file
                                                                       shinyFilesButton(NS(id, "cameras_input"), 
                                                                                        style = "margin-bottom: 25px",
                                                                                        label = 'Choose cameras table', 
                                                                                        title = 'Choose file',
                                                                                        multiple = FALSE),
                                                                       separator_widget(NS(id, "cameras")))
                                                      ),
                                                      column(8,
                                                             uiOutput(NS(id, "ui_crs_col")),
                                                             uiOutput(NS(id, "longlat_col")),
                                                             uiOutput(NS(id, "other_cov_col")),
                                                             textInput(NS(id, "setup_retrieval_format"),
                                                                       "Date format")
                                                             ))
                                      ) # conditional cameras table panel
                     ), # conditionalPanel upload file widgets

# File previews -----------------------------------------------------------

                     tabsetPanel(
                       tabPanel("Raw data preview",
                                conditionalPanel(condition = "input.input_type == 1 || input.records_input !== 0",
                                                 ns = ns,
                                                 h4("Records table"),
                                                 dataTableOutput(NS(id, "raw_records")),
                                                 conditionalPanel(condition = "input.input_type == 1 || input.import_cameras || output.records_extension === 'json'",
                                                                  ns = ns,
                                                                  h4("Cameras table"),
                                                                  dataTableOutput(NS(id, "raw_cameras"))
                                                 )
                                )
                       ),
                       tabPanel("Cleaned data preview",
                                conditionalPanel(condition = "input.input_type == 1 || input.records_input !== 0",
                                                 ns = ns,
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
                       )
                     ) # End tabsetPanel
        ) # End box
    ) # End taglist
}


importServer <- function(id) {
  moduleServer(id, function(input, output, session) {


# Server functions --------------------------------------------------------
    
    
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
    
    # Create a list of widgets
    # 
    # @param df A dataframe with columns
    #   widget, label, empty_allowed, type
    # @param ... additional arguments passed to selectInput
    # 
    # @return A list of selectInput
    create_widget_list <- function(df, ...) {
      
      res <- list()
      for (i in 1:nrow(df)){
        wid <- df$widget[i]
        label <- df$label[i]
        if (df$empty_allowed[i]) {
          # Add "optional" in label
          label <- paste(label, "(optional)")
        }
        res[[i]] <- selectInput(NS(id, wid),
                                label, choices = NULL,
                                ...)
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

# CRS formats -------------------------------------------------------------
    epsg_df <-rgdal::make_EPSG()
    
    epsg <- as.list(epsg_df$code)
    names(epsg) <- paste0(epsg_df$note, " (EPSG:", epsg ,")")
    
# Widgets dataframes --------------------------------------------------------
    records_widgets <- data.frame(
      widget = c("spp_col",
                 "cam_col",
                 "date_col",
                 "time_col",
                 "timestamp_col",
                 "crs_col",
                 "lat_col",
                 "lon_col",
                 "setup_col",
                 "retrieval_col",
                 "count_col",
                 "obs_col"),
      label = c("Species",
                "Camera",
                "Date",
                "Time",
                "Timestamp",
                "Coordinates format (CRS)",
                "Latitude",
                "Longitude",
                "Setup date/datetime",
                "Retrieval date/datetime",
                "Count",
                "Observation type"),
      empty_allowed = c(FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        TRUE,
                        TRUE,
                        TRUE,
                        TRUE),
      type = c("records",
               "records",
               "date_time",
               "date_time",
               "timestamp",
               "cameras",
               "cameras",
               "cameras",
               "cameras",
               "cameras",
               "records",
               "records"),
      regex = c("^vernacularNames\\.en$|species", 
                "station|deployment|camera",
                "date", 
                "hour|time(?!stamp)", 
                "timestamp|datetime",
                NA,
                "lat|((^|[^[:alpha:]]+)y([^[:alpha:]]+|$))",
                "lon|((^|[^[:alpha:]]+)x([^[:alpha:]]+|$))",
                "setup|start",
                "retrieval|end",
                "count",
                "observationType"),
      mica = c("vernacularNames.en",
               "deploymentID",
               NA,
               NA,
               "timestamp",
               NA,
               NA,
               NA,
               NA,
               NA,
               "count",
               "observationType"),
      recordTableSample = c("Species",
                            "Station",
                            NA,
                            NA,
                            "DateTimeOriginal",
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA,
                            NA),
      cast =  c("as.character",
                "as.character",
                "parse_date",
                "times",
                "as_datetime",
                "as.character",
                "as.numeric",
                "as.numeric",
                "parse_date",
                "parse_date",
                "as.numeric",
                "as.character"),
      in_columns = c(TRUE,
                     TRUE,
                     TRUE,
                     TRUE,
                     TRUE,
                     FALSE,
                     TRUE,
                     TRUE,
                     TRUE,
                     TRUE,
                     TRUE,
                     TRUE)
      )
    
    cameras_widgets <- records_widgets %>% 
      dplyr::filter(widget == "cam_col" | type == "cameras") %>%
      mutate(widget = paste(widget, "cov", sep = "_"))
    
    # Set default camera columns for mica
    cameras_widgets <- cameras_widgets %>%
      mutate(mica = ifelse(widget == "crs_col_cov", 
                           4326, mica)) %>%
      mutate(mica = ifelse(widget == "lat_col_cov", 
                           "latitude", mica)) %>%
      mutate(mica = ifelse(widget == "lon_col_cov", 
                           "longitude", mica)) %>%
      mutate(mica = ifelse(widget == "setup_col_cov", 
                           "start", mica)) %>%
      mutate(mica = ifelse(widget == "retrieval_col_cov", 
                           "end", mica))
    # Set default camera columns  for recordTableSample
    cameras_widgets <- cameras_widgets %>%
      mutate(recordTableSample = ifelse(widget == "crs_col_cov", 
                                        32650, recordTableSample)) %>%
      mutate(recordTableSample = ifelse(widget == "lat_col_cov", 
                                        "utm_y", recordTableSample)) %>%
      mutate(recordTableSample = ifelse(widget == "lon_col_cov", 
                                        "utm_x", recordTableSample)) %>%
      mutate(recordTableSample = ifelse(widget == "setup_col_cov", 
                                        "Setup_date", recordTableSample)) %>%
      mutate(recordTableSample = ifelse(widget == "retrieval_col_cov", 
                                        "Retrieval_date", recordTableSample))
    
# Setup variables ---------------------------------------------------------
    # Define roots for ShinyFiles 
    roots <- c("home" = fs::path_home())
    
    # Define placeholder for optional columns
    nullval <- "Not present in data"
    
# Useful variables from widgets df ----------------------------------------
    
    # Get CRS spec
    crs_records <- records_widgets %>%
      filter(in_columns == FALSE)
    crs_cameras <- cameras_widgets %>%
      filter(in_columns == FALSE)
    
    # Get widgets that correspond to columns
    records_widgets <- records_widgets %>%
      filter(in_columns == TRUE)
    cameras_widgets <- cameras_widgets %>%
      filter(in_columns == TRUE)
    
    # Get time widgets
    time_widgets <- records_widgets %>% 
      dplyr::filter(type %in% c("date_time", "timestamp")) %>%
      extract2("widget")
    
    # All widgets
    all_records_widgets <- records_widgets$widget
    all_cameras_widgets <- cameras_widgets$widget
    
    # Define columns for which empty is allowed
    allowed_records <- records_widgets %>%
      filter(empty_allowed == TRUE) %>% 
      extract2("widget")
    allowed_cameras <- cameras_widgets %>%
      filter(empty_allowed == TRUE) %>% 
      extract2("widget")
    empty_allowed <- c(allowed_records, 
                       allowed_cameras)
    
    # Define camera columns
    cameras_to_update <- cameras_widgets %>%
      extract2("widget")
    
    # Define example mappings
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
    # Create mandatory selecters
    mandatory <- records_widgets %>%
      dplyr::filter(type == "records") %>%
      dplyr::filter(empty_allowed == FALSE)
    mandatory_widgets <- create_widget_list(mandatory)
    
    # Create timestamp selecter
    timestamp <- records_widgets %>%
      dplyr::filter(type == "timestamp")
    timestamp_widgets <- create_widget_list(timestamp)
    
    # Create date/time selecters
    datetime <- records_widgets %>%
      dplyr::filter(type == "date_time")
    datetime_widgets <- create_widget_list(datetime)
    
    # Crete camera selecters
    cov <- records_widgets %>%
      dplyr::filter(type == "cameras")
    lonlat <- cov %>% 
      dplyr::filter(widget %in% c("lon_col", "lat_col"))
    other_cov <- cov %>% 
      dplyr::filter(!(widget %in% c("lon_col", "lat_col")))
    
    lonlat_widgets_cov <- create_widget_list(lonlat)
    other_cov_widgets_cov <- create_widget_list(other_cov)
    
    # Create CRS widget
    output$ui_crs_records_col <- renderUI({
      selectizeInput(NS(id, crs_records$widget), 
                     crs_records$label,
                     choices = NULL,
                     options = list(placeholder = 'Select a coordinate reference system'))
    })
    updateSelectizeInput(session = session,
                         crs_records$widget, 
                         choices = epsg,
                         selected = "4326",
                         server = TRUE)
    
    # Create optional selecters
    optional <- records_widgets %>%
      dplyr::filter(type == "records") %>%
      dplyr::filter(empty_allowed == TRUE)
    optional_widgets <- create_widget_list(optional)
    
    # Render UI
    output$mandatory_records_col <- renderUI(mandatory_widgets)
    output$datetime_records_col <- renderUI(datetime_widgets)
    output$timestamp_records_col <- renderUI(timestamp_widgets)
    output$lonlat_records_col <- renderUI(lonlat_widgets_cov)
    output$other_cov_records_col <- renderUI(other_cov_widgets_cov)
    output$optional_records_col <- renderUI(optional_widgets)
    

# Create cameras widgets --------------------------------------------------
    lonlat <- cameras_widgets %>% 
      dplyr::filter(widget %in% c("lon_col_cov", "lat_col_cov"))
    other_cov <- cameras_widgets %>% 
      dplyr::filter(!(widget %in% c("lon_col_cov", "lat_col_cov")))
    
    lonlat_widgets <- create_widget_list(lonlat)
    other_cov_widgets <- create_widget_list(other_cov)
    
    # Create CRS widget
    output$ui_crs_col <- renderUI({
      selectizeInput(NS(id, crs_cameras$widget), 
                     crs_cameras$label,
                     choices = NULL,
                     options = list(placeholder = 'Select a coordinate reference system'))
    })
    updateSelectizeInput(session = session,
                         crs_cameras$widget, 
                         choices = epsg,
                         selected = "4326",
                         server = TRUE)
    
    output$longlat_col <- renderUI(lonlat_widgets)
    output$other_cov_col <- renderUI(other_cov_widgets)
    
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
        widget_df <- widget_df %>% 
          dplyr::filter(type != "date_time")
      } else if (input$datetime_or_timestamp == 'date_time') {
        # Filter out timestamp
        widget_df <- widget_df %>% 
          dplyr::filter(type != "timestamp")
      }
      
      if (input$import_cameras) { 
        # User wants to import a camera file
        widget_df <- widget_df %>% 
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
      widget_cam <- records_widgets %>%
        dplyr::filter(type == "cameras") %>%
        extract2("widget")
      
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
        widget_cam <- records_widgets %>%
          dplyr::filter(type == "cameras") %>%
          extract2("widget")
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
          res <- crs_cameras[["mica"]]
        } else {
          res <- crs_cameras[["recordTableSample"]]
        }
      } else if (input$input_type == 2) { # Manual import file
        if (input$import_cameras || records_extension() == "json") { # Import a camera file
          res <- input[[crs_cameras$widget]]
        } else { # Don't import a camera file
          res <- input[[crs_records$widget]]
        }
      }
      return(as.numeric(res))
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
      
      if ( mapping_cameras()$source == "records" ) {
        split <- TRUE
      } else {
        split <- FALSE
      }
      
      # Get casting types ---
      # Records
      
      castval_rec <- get_named_list(records_widgets,
                                    col = "cast",
                                    widget_values = names(mapping_records()))
      
      # Cameras
      # /!\ Here we choose to look in the records table,
      # because the cast types should be the same in both tables
      # and mapping_cameras was renamed to remove the _col suffix
      castval_cam <- get_named_list(records_widgets,
                                    col = "cast",
                                    widget_values = names(mapping_cameras()$mapping))
      
      if (input$setup_retrieval_format != "") {
        # Get casting value for setup
        cast_setup <- castval_cam$setup_col
        cast_setup_new <- list(cast_setup,
                               format = input$setup_retrieval_format)
        
        # Set new value
        castval_cam$setup_col <- cast_setup_new
      }
      
      metaExpr({
        # Get mapping ---
        mapping_cameras <- ..(mapping_cameras()$mapping)
        mapping_records <- ..(mapping_records())
        
        # Casting types variables ---
        castval_rec <- ..(castval_rec)
        castval_cam <- ..(castval_cam)
        
        clean_data(dat = ..(dat_raw()), 
                   mapping_cameras = mapping_cameras, 
                   cam_type = castval_cam,
                   mapping_records = mapping_records,
                   rec_type = castval_rec,
                   split = ..(split))
        
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
                    options = list(scrollX = TRUE)) %>%
        DT::formatStyle(unname(unlist(mapping_records())),
                        backgroundColor = '#F5EE9E')
    })
    
    output$cameras <- renderDataTable({
      DT::datatable(dat()$data$deployments,
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE)) %>%
        DT::formatStyle(unname(unlist(mapping_cameras()$mapping)),
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
      list(camtrap_data = reactive(dat()),
           mapping_records = reactive(mapping_records()),
           mapping_cameras = reactive(mapping_cameras()$mapping),
           crs = reactive(crs())
      )
    )
    
  })
}