# Import ------------------------------------------------------------------


## Read data ---------------------------------------------------------------


#' Get separator for a file
#'
#' Detects the separator from one line of a file.
#' 
#' @param line A line from a file
#' @param default The default separator to use in case none work
#'
#' @return The detected separator (looks for comma, semicolon and tab)
get_separator <- function(line, default = ",") {
  
  if(grepl(",", line)) {
    sep <- ","
  } else if(grepl(";", line)) {
    sep <- ";"
  } else if(grepl("\t", line)) {
    sep <- "\t"
  } else {
    sep <- default
  }
  return(sep)
}

#' Get example mapping
#' 
#' Return the mapping for example datasets in a vector form
#'
#' @param df a dataframe with columns col and widget
#' @param col the name of the column to extract example mapping from
#'
#' @return A named character vector, excluding NA values in col
get_example_mapping <- function(df, col) {
  
  res_df <- df %>% 
    dplyr::filter(!is.na(.data[[col]]))
  res <- res_df[[col]]
  names(res) <- res_df$widget
  return(res)
}

#' Get named vector
#'
#' Get the named vector from a dataframe
#'
#' @param df The dataframe. Must have a column named widget
#' and a column named like col.
#' @param col  The column of the dataframe to extract
#' @param widget_values The widgets to get the vector for
#' 
#' 
#' @return A named list containing the values of col, 
#' names are the widget names
get_named_list <- function(df, col, widget_values) {
  
  res_df <- df %>%
    filter(widget %in% widget_values) 
  res <- res_df %>%
    extract2(col)
  res <- as.list(res)
  names(res) <- res_df$widget
  return(res)
}

#' Read data
#'
#' Reads data from a file path (either csv of json file), and 
#' optionnally another csv file with camera data.
#'
#' @param records_path A valid file path for records.
#' @param sep_records separator used for the records.
#' @param sep_cameras separator used for the cameras (defaults to NULL).
#' @param cameras_path A valid file path for cameras (defaults to NULL).
#' @param NA_strings Vector of characters that should be considered NAs
#' after import
#'
#' @return A list with 1 components or a camtrapDP list.
#' If records_path is a json file, returns a captrapDP list.
#' If records_path is a csv file, returns a list with 1 component:
#'    $data: a list with 2 components: 
#'        $observations (records)
#'        $deployments (cameras: if no camera file, is NULL)
#' @export
read_data <- function(records_path,
                      sep_records, 
                      cameras_path = NULL,
                      sep_cameras = NULL,
                      NA_strings = c("NA", "")) {
  
  # Get file extension
  ext <- tools::file_ext(records_path)
  
  if (ext == "csv") { # User uploaded a csv file
    # Read csv
    res_records <- utils::read.csv(records_path, sep = sep_records,
                                   na.strings = NA_strings)
    
    if (!is.null(cameras_path)) { # User wants to import a camera file
      # Read csv
      res_cameras <- utils::read.csv(cameras_path, sep = sep_cameras,
                                     na.strings = NA_strings)
    } else { # User doesn't want to import a camera file
      res_cameras <- NULL
    }
    
    res <- list(data = list(observations = res_records,
                            deployments = res_cameras))
    
  } else if (ext == "json") { #   CamtrapDP format
    res <- camtraptor::read_camtrap_dp(records_path, media = FALSE)
  } else { # Unknown extension
    validate(need(ext == "csv" || ext == "json", 
                  "Please upload a csv file or a json datapackage"))
  }
  return(res)
}

## Default colnames --------------------------------------------------------


#' Find default colname
#'
#' Finds the column name to default to among colnames
#' 
#' @param pattern a regular expression to match against colnames
#' @param colnames a vector of column names
#' @param empty_allowed is empty character allowed for this regex?
#' @param empty_placeholder What is the empty placeholder?
#'
#' @return All matched columns names (if there was at least a match) 
#' or the empty_placeholder (if empty_allowed and no match) or NULL
#' (empty not allowed and no match)
#' 
#' @export
#' 
#' @examples 
#' find_default_colname("species", 
#'                      colnames = c("Species", "cameraID", "DateTime"), 
#'                      empty_allowed = TRUE)
#' find_default_colname("foo", 
#'                      colnames = c("Species", "cameraID", "DateTime"), 
#'                      empty_allowed = FALSE)
#' find_default_colname("foo", 
#'                      colnames = c("Species", "cameraID", "DateTime"), 
#'                      empty_allowed = TRUE)
find_default_colname <- function(pattern, colnames, 
                                 empty_allowed,
                                 empty_placeholder = "Not present in data") {
  
  res <- grep(pattern = pattern, 
              colnames,
              ignore.case = TRUE,
              perl = TRUE,
              value = TRUE)
  
  if (length(res) >= 1) { # At least one match
    res <- res
  } else { # No match
    if (empty_allowed) { # Case empty allowed
      res <- empty_placeholder
    } else { # Case empty not allowed
      res <- NULL
    }
  }
  return(res)
}

#' Find default column names
#'
#' Finds the column names to default to among colnames
#' 
#' @param regex_list a named list with names corresponding to those of 
#' widgets and values are regular expressions used to find those widgets
#' in colnames
#' @param colnames a vector of column names
#' @param empty_allowed_list a vector of allowed widgets that can be set
#' to empty
#' @param empty_placeholder the empty placeholder
#'
#' @return A named list containing either (the first) matched colname,
#' NULL (no match and empty not allowed) 
#' or the empty_placeholder (no match and empty allowed)
#' 
#' @export
#'
#' @examples
#' library(camtraptor)
#' data(mica)
#' colnames <- colnames(mica$data$observations)
#' regex <- c("^vernacularNames\\.en$|species", "station|deployment|camera",
#'            "timestamp|datetime")
#' names(regex) <-  c("spp_col", "cam_col", "timestamp_col")
#' find_default_colnames(regex_list = regex,
#'                       colnames = colnames)
find_default_colnames <- function(regex_list,
                                  colnames,
                                  empty_allowed_list = list(),
                                  empty_placeholder = "Not present in data") {
  
  if ( all(is.null(names(regex_list))) ) {
    stop("regex_list must be named")
  }
  
  # Initialize results
  res <- vector(mode = "list", length = length(regex_list))
  names(res) <- names(regex_list)
  
  for (i in 1:length(regex_list)) { # Iterate through input widgets
    w <- names(regex_list)[i]
    pat <- regex_list[[w]]
    
    # Define empty_allowed boolean
    if (w %in% empty_allowed_list) {
      empty_allowed <- TRUE
    } else {
      empty_allowed <- FALSE
    }
    # Get the default colname for pattern i
    res_i <- find_default_colname(pat, colnames = colnames, 
                                  empty_allowed = empty_allowed,
                                  empty_placeholder = empty_placeholder)
    
    if(!is.null(res_i)) {
      # select first occurrence in case multiple matches
      res_i <- res_i[1]
      # Add result
      res[[i]] <- res_i 
    }
    # If result is NULL do nothing
  }  
  return(res)
}


## Clean data --------------------------------------------------------------

#' Add tryFormats to cast list
#'
#' This function adds a tryFormats attribute to some elements of castvec.
#' 
#' @param castlist A named list of types conversions to perform 
#' @param formats The formats to add to tryFormats
#' @param names_to_add The names of the list for which to add
#' a tryFormats element
#'
#' @return The original castlist where elements names like in names_to_add
#' have a new slot tryFormats = formats.
#' Will not add tryFormats if the casting function is NULL.
add_tryformats <- function(castlist,
                           formats,
                           names_to_add) {
  res <- castlist
  for (n in names_to_add) {
    cast_init <- res[[n]]
    if (!is.null(cast_init)) {
      cast_new <- list(cast_init,
                       tryFormats = formats)
      # Set new value
      res[[n]] <- cast_new
    }
  }
  return(res)
}

#' Prepare cameras
#' 
#' Prepare the camera data for cleaning
#'
#' @param dat The data, a list with at least one component
#' $data 
#'    $deployments
#'    $observations
#' @param mapping_cameras The mapping for camera columns
#' @param split should camera data be extracted from the records?
#'
#' @return The dataset with "pre-cleaned" camera data, i.e.
#'  if split = TRUE, dat$data$deployments is filled with data extracted from the recorde
#'  the records are unique across the mapping column
prepare_cameras <- function(dat, mapping_cameras, split = FALSE) {
  
  # Initialize results
  res <- dat
  
  if (split) { # Manual file input
    # Split data
    cameras <- dat$data$observations %>%
      dplyr::select(all_of(unname(unlist(mapping_cameras))))
    res$data$deployments <- cameras
  }
  
  # Select unique rows for camera table
  # We want rows to be unique across the used camera columns defined in mapping_cameras()$mapping
  res$data$deployments <- res$data$deployments %>%
    distinct(across(all_of(unname(unlist(mapping_cameras)))),
             .keep_all = TRUE)
  
  return(res)
}

#' Cast columns to expected types
#'
#' @param df A dataframe containing the columns specified in col_mapping (values)
#' @param cast_type A named list with the type conversion to perform.
#' Names are the names of the columns to cast in df.
#' Elements of this list can either be characters giving
#' a valid function name to call, or a list with the first element as 
#' the function to call and additional arguments (that can be named)
#' being the arguments to pass to the function.
#'
#' @return the df with casted columns
#' 
#' @export
#' 
#' @examples
#' df <- data.frame(num = 1:10,
#'                  char = letters[1:10])
#' cast <- list(num = "as.character",
#'              char = "as.factor")
#' dfcast <- cast_columns(df, cast)
cast_columns <- function(df, cast_type) {
  
  # Initialize res
  res <- df
  for (i in 1:length(cast_type)) {
    castall <- cast_type[[i]]
    col <- names(cast_type)[i]
    
    if (is.list(castall)) {
      # We assume the first argument is the function
      castfunc <- castall[[1]]
      
      # Other arguments are the options
      rest <- castall[-1]
      args <- c(list(res[[col]]),
                rest) 
    } else {
      # Only one argument is the function
      castfunc <- castall
      
      # Args is only the column
      args <- list(res[[col]])
    }
    
    col <- names(cast_type)[[i]]
    
    res[[col]] <- do.call(castfunc, 
                          args)
    
  }

  return(res)
}

#' Formats a dataframe
#' 
#' Moves columns indicated in mapping to the beginning,
#' casts those columns and removes rows where mapping columns
#' have NA values.
#'
#' @param df The dataframe to clean
#' @param mapping The mapping for columns in the dataframe.
#' @param cast_type A named vector for which values are
#' the functions names to apply for each corresponding column 
#' of the vector name. Where mapping is NULL, the casting will not be
#' performed for those columns and hence it is possible that cast_type
#' contains only the non-null values of mapping.
#'
#' @return The dataframe df with cleaned columns.
#' 
#' @export
#'
#' @examples
#' library(camtraptor)
#' data(mica)
#' mapping <- list("col_spp" = "vernacularNames.en",
#'                 "cam_col" = "deploymentID",
#'                 "timestamp_col" = "timestamp")
#' type <- c("col_spp" = "as.character",
#'           "cam_col" = "as.character",
#'           "timestamp_col" = "as_datetime")
#' format_table(mica$data$observations, mapping, type)
format_table <- function(df, mapping, cast_type) {
  
  # Vector from list (NULL will be discarded)
  vec <- unlist(mapping)
  
  # Check arguments are named
  if (is.null(names(mapping)) || is.null(names(cast_type))) {
    stop("mapping and cast_type must be named")
  }
  # Check all cast_types are in mapping
  if ( !all(names(cast_type) %in% names(mapping)) ) {
    stop("all columns listed in cast_type must be in mapping")
  }
  # Check all non-null mapping are in cast_type
  if ( !all(names(vec) %in% names(cast_type)) ) {
    stop("all non-null columns listed in mapping must be in cast_type")
  }

  res <- df %>%
    select(all_of(unname(vec)),
           everything())
  
  # Cast columns
  # Reorder
  castval <- cast_type[names(vec)]
  names(castval) <- vec
  res <- cast_columns(res,
                      castval)
  
  # Drop NA
  # res <- remove_rows_with_NA(res, vec)
  
  return(res)
}

#' Remove rows with NAs
#'
#' @param df The dataframe to clean
#' @param mapping The mapping for columns in the dataframe.
#'
#' @return The dataframe where rows in mapping contain no NA values
#' (except spp_col which is allowed to have NA values if obs_col is in mapping,
#' when obs_col != 'animal')
#' 
#' @export
#'
#' @examples
#' library(camtraptor)
#' data(mica)
#' df <- mica$data$observations
#' mapping <- c("spp_col" = "vernacularNames.en",
#'              "cam_col" = "deploymentID",
#'              "timestamp_col" = "timestamp",
#'              "obs_col" = "observationType")
#' # Purposedly add NAs to chack rows are removed
#' df$vernacularNames.en[9] <- NA
#' df$timestamp[1] <- NA
#' remove_rows_with_NA(df, mapping)
remove_rows_with_NA <- function(df, mapping) {
  
  if ("obs_col" %in% names(mapping)) {
    # Authorize NA values in spp_col where obs_col is not "species"
    spp_col_name <- mapping["spp_col"]
    obs_col_name <- mapping["obs_col"]
    
    na_check <- mapping[mapping != spp_col_name]
    
    # Drop NA in all columns except spp_col
    res <- df %>%
      drop_na(all_of(unname(na_check)))
    
    obs_col <- res[[obs_col_name]]
    spp_col <- res[[spp_col_name]]
    # Get the NA values of spp_col name where obs_col is 'animal'
    spp_NA <- which(is.na(spp_col) & obs_col == "animal")
    
    if (length(spp_NA) != 0) {
      res <- res[-spp_NA,]
    }
    
  } else {
    # No NAs authorized in the mapping columns
    res <- df %>%
      drop_na(all_of(unname(mapping)))
  }
  return(res)
}

#' Filter data to keep only cameras in both tables
#'
#' Selects rows where cameras are in both tables
#' 
#' @param records Records dataframe
#' @param cameras Cameras dataframe
#' @param cam_col_records Name of the columns with camera values in records
#' @param cam_col_cameras Name of the columns with camera values in cameras
#'
#' @return A list of 2 dataframes with filtered values.
#' 
#' @export
filter_cameras_in_both_tables <- function(records, cameras, 
                                          cam_col_records, 
                                          cam_col_cameras) {
  
  # Get unique camera names
  ucam_records <- unique(records[[cam_col_records]])
  ucam_cameras <- unique(cameras[[cam_col_cameras]])
  
  # Get intersection
  cam_both <- intersect(ucam_records, 
                        ucam_cameras)
  
  # Restrict data to shared cameras
  records <- records %>%
    filter(.data[[cam_col_records]] %in% cam_both)
  
  cameras <- cameras %>%
    filter(.data[[cam_col_cameras]] %in% cam_both)
  
  res <- list(records = records,
              cameras = cameras)
  
  return(res)
}

#' Clean data
#'
#' Cleans raw data by:
#' + splitting data if needed
#' + formatting cameras and records tables
#' + (if only_shared_cameras): selecting the subset of cameras present in both datasets
#'  
#' @param dat The data ti clean
#' @param mapping_cameras The mapping for columns in the cameras dataframe.
#' Names are free except the camera column which must be identified with cam_col.
#' They must only be the same names as in cam_type.
#' @param cam_type A named list with the type conversion to perform.
#' Must be a valid function name to call.
#' Names are the names of the columns to cast in cameras df.
#' @param mapping_records The mapping for columns in the records dataframe.
#' Names are free except the camera column which must be identified with cam_col.
#' They must only be the same names as in rec_type.
#' @param rec_type A named vector with the type conversion to perform.
#' Must be a valid function name to call.
#' Names are the names of the columns to cast in records df.
#' @param split Should the camera data be splitted from the records table?
#' @param only_shared_cameras restrict cleaned data to shared cameras? (ie the
#' cameras that are in data$deployments and in data$observations)
#'
#' @return The cleaned dataset
#' 
#' @export
#' 
#' @examples
#' data(recordTableSample, package = "camtrapR")
#' data(camtraps, package = "camtrapR")
#' dat <- list(data = list(observations = recordTableSample,
#'                         deployments = camtraps))
#' mapping_records <- list(cam_col = "Station",
#' date_col = "Date",
#' time_col = "Time")
#' rec_type <- list(cam_col = "as.character",
#'                  date_col = list("as_date",
#'                                  format = "%Y-%m-%d"),
#'                  time_col = "times")
#' 
#' mapping_cameras <- list(cam_col = "Station",
#'                         setup_col = "Setup_date",
#'                         retrieval_col = "Retrieval_date")
#' cam_type <- list(cam_col = "as.character",
#'                  setup_col = list("as_date",
#'                                   format = "%d/%m/%Y"), 
#'                  retrieval_col = list("as_date",
#'                                       format = "%d/%m/%Y"))
#' dat_clean <- clean_data(dat, 
#'                         mapping_records = mapping_records, rec_type = rec_type,
#'                         mapping_cameras = mapping_cameras, cam_type = cam_type)
clean_data <- function(dat, 
                       mapping_cameras, 
                       cam_type,
                       mapping_records,
                       rec_type,
                       only_shared_cameras = FALSE,
                       split = FALSE) {

  # Prepare cameras ---
  res <- prepare_cameras(dat, 
                         mapping_cameras = mapping_cameras, 
                         split = split)
  
  # Records ---
  res$data$observations <- format_table(res$data$observations,
                                        mapping = mapping_records,
                                        cast_type = rec_type)
  
  # Cameras ---
  res$data$deployments <- format_table(res$data$deployments,
                                       mapping = mapping_cameras,
                                       cast_type = cam_type)
  
  # Both data ---
  if (only_shared_cameras) {
    # Restrict data to shared cameras
    # Get column names ---
    cam_col_records <- mapping_records[["cam_col"]]
    cam_col_cameras <- mapping_cameras[["cam_col"]]
    bothcam <- filter_cameras_in_both_tables(res$data$observations,
                                             res$data$deployments, 
                                             cam_col_records,
                                             cam_col_cameras)
    
    res$data$observations <- bothcam$records
    res$data$deployments <- bothcam$cameras
  }
  
  return(res)
}

# Summary -----------------------------------------------------------------

#' Get cameras not in
#'
#' From a records and a cameras dataframe, determine which cameras are in a file
#' but not in the other
#'
#' @param dfrecords records dataframe
#' @param dfcameras cameras dataframe
#' @param cam_col_records name of the cameras column in the records dataframe
#' @param cam_col_cameras name of the cameras column in the cameras dataframe
#'
#' @return A named list with 2 components
#' not_in_records: cameras from dfcameras that are not in dfrecords
#' not_in_cameras: cameras from dfrecords that are not in dfcameras
#' If the cameras are toe same in both files, the components of the list are 
#' character vectors of length zero.
#'
#' @export
#'
#' @examples
#' dfrecords <- data.frame(camID = letters[2:7])
#' dfcam <- data.frame(cameras = letters[1:5])
#' get_cameras_not_in(dfrecords = dfrecords, 
#'                    dfcameras = dfcam,
#'                    cam_col_records = "camID",
#'                    cam_col_cameras = "cameras")
get_cameras_not_in <- function(dfrecords, 
                               dfcameras,
                               cam_col_records,
                               cam_col_cameras) {
  
  # Get cameras from records and camera file
  records_cameras <- unique(dfrecords[[cam_col_records]])
  deployments_cameras <- dfcameras[[cam_col_cameras]]
  
  # Get not matching list
  not_in_cameras <- records_cameras[!(records_cameras %in% deployments_cameras)]
  not_in_records <- deployments_cameras[!(deployments_cameras %in% records_cameras)]
  
  res <- list("not_in_records" = not_in_records,
              "not_in_cameras" = not_in_cameras)
  return(res)
}

#' Print check cameras
#' 
#' Prints a message to describe cameras status
#'
#' @param cameras character vector of camera names
#' @param type type of message to print: not_in_records or not_in_cameras
#'
#' @return A message describing which cameras are missing where, 
#' the empty string of no cameras are missing
print_check_cameras <- function(cameras, 
                                type = c("not_in_records", "not_in_cameras")) {
  
  if (length(cameras) != 0) {
    if (length(cameras) == 1) {
      if (type == "not_in_records") {
        sentence <- " camera is in the cameras file but not in the records: "
      } else if (type == "not_in_cameras"){
        sentence <- " camera is in the records but not in the cameras file: "
      }
      
    } else {
      if (type == "not_in_records") {
        sentence <- " cameras are in the cameras file but not in the records: "
      } else if (type == "not_in_cameras"){
        sentence <- " cameras are in the records but not in the cameras file: "
      }
    }
    msg <- paste0(length(cameras), sentence,
                  paste(cameras, collapse = ", "))
  } else {
    msg <- ""
  }
  
  return(msg)
}
#' Plot points
#' 
#' Plot occurrences points from a dataframe
#'
#' @param df The dataframe
#' @param camera_col Name of the camera column
#' @param spp_col Name of the species column
#' @param timestamp_col Name of the timestamp column 
#' (can be null if date_col and time_col are provided)
#' @param date_col Name of the date column (can be NULL if timestamp_col is provided)
#' @param time_col Name of the time column (can be NULL if timestamp_col is provided)
#' @param interactive  Make the plot interactive?
#'
#' @details If date_col and time_col are provided along timestamp_col,
#' they will be ignored.
#' 
#' @return A ggplot
#'
#' @export
#'
#' @examples
#' library(camtrapR)
#' data("recordTableSample")
#' plot_points(recordTableSample, 
#'             camera_col = "Station", 
#'             timestamp_col = "DateTimeOriginal", 
#'             spp_col = "Species")
plot_points <- function(df, 
                        camera_col,
                        spp_col,
                        timestamp_col,
                        date_col = NULL,
                        time_col = NULL,
                        interactive = TRUE) {
  
  # Initialize plotting data
  dfp <- df
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if (is.null(date_col) | is.null(time_col)) {
      stop("If timestamp_col is not provided, date_col and time_col must be provided.")
    }
  }
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if("timestamp_col" %in% colnames(dfp)) {
      warning("timestamp_col already exists and this might interfer with plotting")
    }
    # Create a composite timestamp
    dfp$timestamp_col <- paste(as.character(dfp[[date_col]]), 
                               as.character(dfp[[time_col]]))
    
    dfp$timestamp_col <- as.POSIXct(dfp$timestamp_col)
    
    # Change timestamp_col value
    timestamp_col <- "timestamp_col"
  }
  
  if (interactive) {
    gg <- ggplot(dfp, aes(x = .data[[timestamp_col]], 
                          y = .data[[camera_col]],
                          col = .data[[spp_col]],
                          tooltip = paste(.data[[spp_col]], 
                                          .data[[timestamp_col]],
                                          sep = ": "),
                          data_id = .data[[camera_col]]
                          )) +
      geom_point_interactive(show.legend = FALSE)
  } else {
    gg <- ggplot(dfp, aes(x = .data[[timestamp_col]], 
                          y = .data[[camera_col]],
                          col = .data[[spp_col]])) +
      geom_point(show.legend = FALSE)
  }
  
  gg <- gg +
    theme_linedraw() + 
    xlab("Date") +
    ylab("Camera")
  
  return(gg)
}

#' Plot species bars
#'
#' Plot the barplot of species abundance from a dataframe
#'
#' @param df The dataframe
#' @param spp_col Name of the species column
#' @param count_col Name of the count column
#' @param obs_col Name of the observation type column
#' @param interactive Make the plot interactive?
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' library(camtrapR)
#' data("recordTableSample")
#' plot_species_bars(recordTableSample,
#'                   spp_col = "Species")
plot_species_bars <- function(df, 
                              spp_col, 
                              count_col = NULL,
                              obs_col = NULL,
                              interactive = TRUE) {
  
  # Initialize df plot
  dfp <- df
  
  if (!is.null(obs_col)) {
    # Get only the observations of type animal
    dfp <- dfp %>% filter(.data[[obs_col]] == "animal")
  }
  
  # Group by species
  dfp <- dfp %>% group_by(.data[[spp_col]])
  
  if (is.null(count_col)) { # no count column
    dfp <- dfp %>%
      summarise(count = n())
  } else { # count column
    dfp <- dfp %>%
      summarise(count = sum(.data[[count_col]]))
  }
   
  if (interactive) {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], count),
                          y = count,
                          tooltip = paste(.data[[spp_col]], count, 
                                          sep = ": ")
                          )) +
      geom_col_interactive()
  } else {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], count),
                          y = count)) +
      geom_col()
  }
  gg <- gg +
    coord_flip() +
    theme_linedraw() +
    ylab("Count") +
    theme(axis.title.y = element_blank())
  
  return(gg)
}

#' Plot map
#' 
#' Plot a leaflet map of cameras
#' 
#' @param df A dataframe with latitude and longitude columns
#' @param lat_col Name of the latitude column
#' @param lon_col Name of the longitude column
#' @param crs EPSG code for the coordinate reference system
#' @param cam_col Name of the camera name column
#' @param color color for the points

#'
#' @return a leaflet map
#' @export
plot_map <- function(df, 
                     lat_col, lon_col, 
                     crs = NULL,
                     cam_col,
                     color = "black") {
  
  if(!is.null(crs)) { # Specify the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col),
                          crs = as.numeric(crs))
    # Reproject in WGS 84 (a.k.a. EPSG:4326)
    df_sf <- sf::st_transform(df_sf, 4326)
  } else { # Let leaflet choose the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col))
  }
  
  leaflet(df_sf) %>% 
    addTiles() %>% 
    addCircles(data = df_sf,
               label = paste0("Camera: ", df_sf[[cam_col]]),
               layerId = df_sf[[cam_col]],
               popup = df_sf[[cam_col]],
               color = color,
               highlightOptions = highlightOptions(color = "red"))
}

#' Summarize cameras
#' 
#' Summarize camera trap data (records) by creating a start 
#' and an retrieval date.
#' If setup and retrieval date are provided via dfcam,
#' then the dates used in setup and retrieval will be used instead of
#' the first/last picture date.
#' 
#' @param df the dataframe
#' @param cam_col name of the column containing camera IDs
#' @param timestamp_col name of the column containing timestamps 
#' (optional if date_col and time_col are provided)
#' @param date_col name of the column containing date (optional if timestamp is provided)
#' @param time_col name of the column containing hour (optional if timestamp is provided)
#' @param dfcam the dataframe of cameras deployments (optional)
#' @param cam_col_dfcam name of the column containing camera IDs in dfcam (optional if dfcam not provided)
#' @param setup_col name of the column containing setup date/time in dfcam (optional if retrieval_col is provided)
#' @param retrieval_col name of the column containing retrieval date/time in dfcam (optional if setup_col is provided)
#' 
#' @return A summarized dataframe with one row per camera:
#' cam_col (name of the camera column), setup, retrieval,
#' setup_origin ("picture" or "setup_date") and
#' retrieval_origin ("picture" or "retrieval_date")
#' 
#' @export
summarize_cameras <- function(df, cam_col, 
                              timestamp_col,
                              date_col = NULL, time_col = NULL,
                              dfcam = NULL, cam_col_dfcam = NULL,
                              setup_col = NULL, retrieval_col = NULL) {
  
  # --- Check inputs
  # Display message to say that date_col and time_col will
  # not be used
  if (!missing(timestamp_col) && !is.null(timestamp_col)) {
    if (!is.null(date_col) || !is.null(time_col)) {
      message("timestamp_col is provided, so date_col and time_col will be ignored.")
    }
  }
  
  # Stop if not date AND time are specified
  if (missing(timestamp_col) || is.null(timestamp_col)) {
    if (is.null(date_col) || is.null(time_col)) {
      stop("If timestamp_col is not specified or NULL, both date_col and time_col must be provided.")
    }
  }
  
  # Check that some columns are provided when dfcam is provided
  if (!is.null(dfcam)) {
    if (is.null(cam_col_dfcam)) {
      stop("If dfcam is not NULL, then cam_col_dfcam must be provided.")
    }
    if (is.null(setup_col) & is.null(retrieval_col)) {
      stop("If dfcam is not NULL, then setup_col or retrieval_col must be provided.")
    }
  }
  
  # --- Compute first and last picture
  camsum <- df
  
  if (missing(timestamp_col) || is.null(timestamp_col)) {
    # Create timestamp column
    camsum$timestamp <- paste(camsum[[date_col]],
                              camsum[[time_col]])
    camsum$timestamp <- as.POSIXct(camsum$timestamp)
    # Set timestamp_col to 'timestamp'
    timestamp_col <- "timestamp"
  }
  
  # Summarize with timestamp
  camsum <- camsum %>%
    group_by(.data[[cam_col]]) %>%
    summarise(setup = min(.data[[timestamp_col]]),
              retrieval = max(.data[[timestamp_col]]),
              setup_origin = "picture",
              retrieval_origin = "picture")
  
  # --- Use dfcam for setup and retrieval
  # If we have additional info from dfcam
  if (!is.null(dfcam)) {
    
    # Add cameras present in dfcam but not in df
    not_in_camsum <- dfcam[[cam_col_dfcam]][!(dfcam[[cam_col_dfcam]] %in% camsum[[cam_col]])]
    
    if (length(not_in_camsum) != 0) {
      dfbind <- data.frame(not_in_camsum, NA, NA, NA, NA)
      names(dfbind) <- c(cam_col, "setup", "retrieval", "setup_origin", "retrieval_origin")
      camsum <- rbind(camsum, dfbind)
    }
    
    if (!is.null(setup_col)) { # Setup date specified in cameras
      
      setup_df <- dfcam
      
      # Get non-NA setup
      setup_df <- setup_df %>%
        filter(!is.na(.data[[setup_col]]))
      
      # Cast to POSIX
      setup_df[[setup_col]] <- as.POSIXct(setup_df[[setup_col]],
                                          tz = lubridate::tz(camsum$timestamp))
      
      # Get indices to replace in camsum
      # We will replace all these indices because they are not null
      # and we keep the values that are missing in dfcam but were already
      # in camsum
      ind <- match(setup_df[[cam_col_dfcam]], camsum[[cam_col]])
      
      camsum$setup[ind] <- setup_df[[setup_col]]
      camsum$setup_origin[ind] <- "setup"
    }
    if (!is.null(retrieval_col)) { # Retrieval date specified in cameras
      
      retrieval_df <- dfcam
      
      # Get non-NA retrieval
      retrieval_df <- retrieval_df %>%
        filter(!is.na(.data[[retrieval_col]]))
      
      # Get indices to replace in camsum
      # We will replace all these indices because they are not null
      # and we keep the values that are missing in dfcam but were already
      # in camsum
      ind <- match(retrieval_df[[cam_col_dfcam]], camsum[[cam_col]])
      
      camsum$retrieval[ind] <- retrieval_df[[retrieval_col]]
      camsum$retrieval_origin[ind] <- "retrieval"
    }
  }
  
  # --- Compute sampling length
  # Cast to character for cameraOperation
  date_format <- "%Y-%m-%d %H:%M:%S" # Choose standard format to cast to character
  
  # Get rows where setup and retrieval are not NA (can't compute sampling length without
  # setup and retrieval)
  camsum_sampling_time <- camsum %>%
    filter(!is.na(setup)) %>%
    filter(!is.na(retrieval))
  
  # Cast to character
  camsum_sampling_time$setup <- as.character(camsum_sampling_time$setup, 
                                             format = date_format)
  camsum_sampling_time$retrieval <- as.character(camsum_sampling_time$retrieval, 
                                                 format = date_format)
  
  mat <- camtrapR::cameraOperation(camsum_sampling_time,
                                   stationCol = cam_col,
                                   setupCol = "setup",
                                   retrievalCol = "retrieval",
                                   dateFormat = "Ymd HMS",
                                   hasProblems = FALSE) # To modify once the user will be able to add problems
  
  # Get sampling length per camera
  sampling_length <- rowSums(mat, na.rm = TRUE)
  sampling_length <- sampling_length[match(camsum[[cam_col]], names(sampling_length))]
  # (will set the cameras of camsum without sampling_length
  # to NA which is what we want)
  
  # Modify result
  camsum$sampling_length <- unname(sampling_length)
  
  return(camsum)
}
