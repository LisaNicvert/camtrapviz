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
                                                      uiOutput(NS(id, "cameras_records_col"))
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
                                                                       fileInput(NS(id, "cameras_input"), 
                                                                                 "Cameras table",
                                                                                 accept = ".csv"),
                                                                       separator_widget(NS(id, "cameras")))
                                                      ),
                                                      column(8,
                                                             uiOutput(NS(id, "cameras_col"))
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
                                                 h4("Cameras table"),
                                                 dataTableOutput(NS(id, "raw_cameras"))
                                )
                       ),
                       tabPanel("Cleaned data preview",
                                conditionalPanel(condition = "input.input_type == 1 || input.records_input !== 0",
                                                 ns = ns,
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
    # @param default A named vector, names are the widget names
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
    # 
    # @return A list of selectInput
    create_widget_list <- function(df) {
      
      res <- list()
      for (i in 1:nrow(df)){
        wid <- df$widget[i]
        label <- df$label[i]
        if (df$empty_allowed[i]) {
          # Add "optional" in label
          label <- paste(label, "(optional)")
        }
        res[[i]] <- selectInput(NS(id, wid),
                                label, choices = NULL)
      }
      return(res)
    }

# Widgets dataframes --------------------------------------------------------
    records_widgets <- data.frame(
      widget = c("spp_col",
                 "cam_col",
                 "date_col",
                 "time_col",
                 "timestamp_col",
                 "lat_col",
                 "lon_col",
                 "count_col",
                 "obs_col"),
      label = c("Species",
                "Camera",
                "Date",
                "Time",
                "Timestamp",
                "Latitude",
                "Longitude",
                "Count",
                "Observation type"),
      empty_allowed = c(FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        FALSE,
                        TRUE,
                        TRUE),
      type = c("records",
               "records",
               "datetime",
               "datetime",
               "datetime",
               "cameras",
               "cameras",
               "records",
               "records"))
    
    cameras_widgets <- data.frame(
      widget = c("cam_col_cov",
                  "lat_col_cov",
                  "lon_col_cov"),
      label = c("Camera",
                "Latitude",
                "Longitude"),
      empty_allowed = c(FALSE,
                        FALSE,
                        FALSE),
      type = c("cameras",
               "cameras",
               "cameras"))
    
# Setup variables ---------------------------------------------------------
    # Define roots for ShinyFiles 
    roots <- c("home" = fs::path_home())
    
    # Define placeholder for optional columns
    nullval <- "Not present in data"
    
    # Define columns for which empty is allowed
    empty_allowed <- c("count_col", "obs_col")
    
    # Define camera columns
    cameras_cols_wanted <- c("cam_col_cov",
                             "lat_col_cov",
                             "lon_col_cov")
    
    # Define example data column mappings
    example_mapping_records <- list(mica = c("spp_col" = "vernacularNames.en",
                                             "obs_col" = "observationType",
                                             "cam_col" = "deploymentID",
                                             "timestamp_col" = "timestamp",
                                             "count_col" = "count"),
                                    recordTableSample = c("spp_col" = "Species",
                                                          "cam_col" = "Station",
                                                          "timestamp_col" = "DateTimeOriginal"))
    example_mapping_cameras <- list(mica = c("cam_col_cov" = "deploymentID",
                                             "lat_col_cov" = "latitude",
                                             "lon_col_cov" = "longitude"),
                                    recordTableSample = c("cam_col_cov" = "Station",
                                                          "lat_col_cov" = "utm_y",
                                                          "lon_col_cov" = "utm_x"))
    

# Load example data -------------------------------------------------------

    utils::data(mica, package = "camtraptor")
    utils::data(recordTableSample, package = "camtrapR")
    utils::data(camtraps, package = "camtrapR")
    

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
    
    # Create date/time selecters
    datetime_all <- records_widgets %>%
      dplyr::filter(type == "datetime")
    
    timestamp <- datetime_all %>%
      dplyr::filter(widget == "timestamp_col")
    timestamp_widgets <- create_widget_list(timestamp)
    
    datetime <- datetime_all %>%
      dplyr::filter(widget != "timestamp_col")
    datetime_widgets <- create_widget_list(datetime)
    
    
    # Crete camera selecters
    cov <- records_widgets %>%
      dplyr::filter(type == "cameras")
    cov_widgets <- create_widget_list(cov)
    
    # Create optional selecters
    optional <- records_widgets %>%
      dplyr::filter(type == "records") %>%
      dplyr::filter(empty_allowed == TRUE)
    optional_widgets <- create_widget_list(optional)
    
    # Render UI
    output$mandatory_records_col <- renderUI(mandatory_widgets)
    output$datetime_records_col <- renderUI(datetime_widgets)
    output$timestamp_records_col <- renderUI(timestamp_widgets)
    output$cameras_records_col <- renderUI(cov_widgets)
    output$optional_records_col <- renderUI(optional_widgets)


# Create cameras widgets --------------------------------------------------
    cameras_widgets_col <- create_widget_list(cameras_widgets)
    output$cameras_col <- renderUI(cameras_widgets_col)
    
# Read files --------------------------------------------------------------


## Shiny file listener -----------------------------------------------------

    shinyFileChoose(input, 'records_input', 
                    root = c("home" = fs::path_home()),
                    filetypes = c('csv', 'json'))
  
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
  
    dat_raw <- reactive({
      if (input$input_type == 1) { # Example dataset
        if(input$example_file == "mica") {
          res <- mica
        } else {
          res <- list(data = list(observations = recordTableSample,
                                  deployments = camtraps))
        }
      } else if (input$input_type == 2) { # Uploaded dataset
        # Get file
        file <- shinyFiles::parseFilePaths(roots,
                                           input$records_input)
        # Ensure file is loaded before continuing
        req(file)
        
        # Get file_path
        file_path <- file$datapath
        validate(need(file_path != '', "Please upload a records file"))
        
        # Get separator values
        sep_records <- input$records_sep
        sep_cameras <- input$cameras_sep
        
        # User wants to import a camera file?
        if (input$import_cameras) {
          # Get file
          camera_file <- input$cameras_input
          
          # Ensure file is loaded before continuing
          req(camera_file)
          
          # Get camera_path
          cameras_path <- camera_file$datapath
          validate(need(cameras_path != '', "Please upload camera file"))
        } else {
          cameras_path <- NULL
        }
        
        # Read data
        res_all <- read_data(file_path, 
                             sep_records,
                             cameras_path,
                             sep_cameras)
        
        # Update records separator
        updateRadioButtons(inputId = "records_sep",
                           selected = res_all$sep$sep_records)
        
        # Update camera separator
        updateRadioButtons(inputId = "cameras_sep",
                           selected = res_all$sep$sep_cameras)
        
        # Get data
        res <- res_all$camtrap_data
      }
      return(res)
    })

# UI settings -------------------------------------------------------------
  
    observe({
      # Set camera import to false if we have a json file
      if (records_extension() == "json") {
        updateCheckboxInput(session = session,
                            "import_cameras", 
                            value = FALSE)
      }
    })
  
# Columns mapping (records) ---------------------------------------------------------
  
    # Get input data columns
    records_cols <- reactive({
      colnames(dat_raw()$data$observations)
    })
    
    # Get the columns we want to update for the records
    records_cols_wanted <- reactive({
      if (input$datetime_or_timestamp == 'timestamp') {
        datetime_widgets <- "timestamp_col"
      } else if (input$datetime_or_timestamp == 'date_time') {
        datetime_widgets <- c("date_col", "time_col")
      }
      widget_list <- c("spp_col", "cam_col",
                       datetime_widgets,
                       "count_col", "obs_col")
      
      if (!input$import_cameras) { 
        # User doesn't want to import a camera file
        widget_list <- c(widget_list,
                         "lat_col", "lon_col")
      }
      widget_list
    })
    
    # Default columns mapping for records
    default_records <- reactive({
      # Ensure records table was imported and is available 
      #   (i.e. no example data and data correctly loaded)
      req(input$records_input)
      
      validate(need(!(nullval %in% records_cols()),
                    paste("Please provide a dataframe with no column named",
                          nullval)))
      
      # Find default names
      default_names <- find_default_colnames(records_cols_wanted(),
                                             records_cols(),
                                             empty_allowed,
                                             empty_placeholder = nullval)
      default_names
    })
    

## Update selectInput ------------------------------------------------------

    # Update selection list and default names in selectInput for records
    observeEvent(input$records_input, {
      if (input$input_type == 2) { # Only update widgets for manual import
        update_selected_columns(widget_list = records_cols_wanted(), 
                                default = default_records(),
                                choices = records_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    
    # Update choices upon separator change
    observeEvent(input$records_sep, {
      if (input$input_type == 2) { # Only update widgets for manual import
        update_selected_columns(widget_list = records_cols_wanted(), 
                                default = default_records(),
                                choices = records_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    
    # Update date/time/stamp upon input change
    observeEvent(input$datetime_or_timestamp, {
      if (input$input_type == 2) { # Only update widgets for manual import
        widgets <- records_cols_wanted()
        time_widgets <- widgets[which(widgets %in% c("date_col", "time_col", "timestamp_col"))]
        
        update_selected_columns(widget_list = time_widgets, 
                                default = default_records(),
                                choices = records_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    

## Mapping -----------------------------------------------------------------
    
    # Mapping value for records columns
    mapping_records_all <- reactive({
      if (input$input_type == 1) { # Example files
        # Get known mapping
        res <- example_mapping_records[[input$example_file]]
      } else if (input$input_type == 2) { # Uploaded files
        # Get the values selected by the user
        
        # Get relevant widgets
        widgets <- records_cols_wanted()
        
        # Get selected values for widgets
        res <- vector(mode = "character", 
                      length = length(widgets))
        for(i in 1:length(widgets)) {
          res[i] <- input[[widgets[i]]]
        }
        names(res) <- widgets
      }
      res
    })
    
    mapping_records <- reactive({
      # Get relevant columns (all except "Not in data")
      res <- mapping_records_all()[mapping_records_all() != nullval]
      # Discard lon/lat that are mapped by cameras
      res <- res[which(!names(res) %in% c("lat_col", "lon_col"))]
      
      res
    })
    
# Column mapping (cameras) ------------------------------------------------
    
    # Get input cameras columns
    cameras_cols <- reactive({
      if (!is.null(dat_raw()$data$deployments)) { # Camera file was provided
        res <- colnames(dat_raw()$data$deployments)
      } else { # Camera is in data
        res <- NULL
      }
    })
    
    # Default columns mapping for cameras
    default_cameras <- reactive({
      # Find default names
      if (!is.null(cameras_cols())) {
        default_names <- find_default_colnames(cameras_cols_wanted,
                                               cameras_cols())
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
          update_selected_columns(widget_list = cameras_cols_wanted, 
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
        update_selected_columns(widget_list = cameras_cols_wanted, 
                                default = default_cameras(),
                                choices = cameras_cols(),
                                empty_allowed = empty_allowed,
                                nullval = nullval)
      }
    })
    

## Mapping -----------------------------------------------------------------
    
    # Mapping value for cameras columns
    mapping_cameras <- reactive({
      if (input$input_type == 1) { # Example files
        # Get known mapping
        res <- example_mapping_cameras[[input$example_file]]
      } else if (input$input_type == 2) { # Uploaded files
        # Get the values selected by the user
        if (!is.null(cameras_cols())) { # Camera file was provided
          # Get relevant widgets
          widgets <- cameras_cols_wanted
        } else { # Camera file was not provided
          widgets <- gsub("_cov$", "", cameras_cols_wanted)
        }
        # Get selected values for widgets
        res <- vector(mode = "character", 
                      length = length(widgets))
        for(i in 1:length(widgets)) {
          res[i] <- input[[widgets[i]]]
        }
        names(res) <- widgets
      }
      # Remove the 'cov' if it was in the names
      names(res) <- gsub("_cov$", "", names(res))
      res
    })
    
# Clean data --------------------------------------------------------------
    
    dat <- reactive({
      # Copy raw data
      dat <- dat_raw()
      
      # Records ---
      dat$data$observations <- format_table(dat$data$observations,
                                            mapping_records())
      
      # Cameras ---
      if (is.null(cameras_cols())) { # Camera file was not provided
        # Split data
        cameras <- dat$data$observations %>%
          select(all_of(unname(mapping_cameras())), 
                 everything())
        
        cameras <- cameras %>% distinct()
        dat$data$deployments <- cameras
      }
      
      dat$data$deployments <- format_table(dat$data$deployments,
                                           mapping_cameras())
      
      # Select unique rows for camera table
      # We want rows to be unique across the used camera columns defined in mapping_cameras()
      dat$data$deployments <- dat$data$deployments %>%
        distinct(across(all_of(unname(mapping_cameras()))))
      
      # Both data ---
      # Restrict data to shared cameras
      cam_col_records <- mapping_records()["cam_col"]
      cam_col_cameras <- mapping_cameras()["cam_col"]
      bothcam <- filter_cameras_in_both_tables(dat$data$observations,
                                               dat$data$deployments, 
                                               cam_col_records,
                                               cam_col_cameras)
      
      dat$data$observations <- bothcam$records
      dat$data$deployments <- bothcam$cameras
      
      return(dat)
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
        DT::formatStyle(mapping_records(),
                        backgroundColor = '#F5EE9E')
    })
    
    output$cameras <- renderDataTable({
      DT::datatable(dat()$data$deployments,
                    filter = "none",
                    selection = "none",
                    options = list(scrollX = TRUE)) %>%
        DT::formatStyle(mapping_cameras(),
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
    list(camtrap_data = reactive(dat()),
         mapping_records = reactive(mapping_records()),
         mapping_cameras = reactive(mapping_cameras())
         )
    
  })
}