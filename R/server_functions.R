# Import ------------------------------------------------------------------

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
#' @return A named vector containing the values of col, 
#' names are the widget names
get_named_vector <- function(df, col, widget_values) {
  
  res_df <- df %>%
    filter(widget %in% widget_values) 
  res <- res_df %>%
    extract2(col)
  names(res) <- res_df$widget
  return(res)
}

#' Read csv
#'
#' Reads a csv file from a fileInput widget with the column separator specified in a 
#' radioButtons widget.
#'
#' @param file_path a valid path to a csv file
#' @param column_separator column separator character
#'
#' @return A list with 2 elements: 
#' dat = the dataframe read from the file and
#' sep = the character separator used to read the file
#'
#' @export
read_csv <- function(file_path, column_separator) {
  
  if (is.null(column_separator)) { # Unspecified file separator
    # Default to comma separator
    fsep <- ","
    df <- utils::read.csv(file_path, sep = fsep)
    
    if (ncol(df) == 1) { # Try tab
      fsep <- "\t"
      df <- utils::read.csv(file_path, sep = fsep)
    }
    if (ncol(df) == 1) { # Try semicolon
      fsep <- ";"
      df <- utils::read.csv(file_path, sep = fsep)
    } 
    if (ncol(df) == 1) { # Other character (choice of custom character to implement)
      df <- utils::read.csv(file_path, sep = fsep)
    }
  } else { # File separator is specified
    fsep <- column_separator
    df <- utils::read.csv(file_path, sep = column_separator)
  }
  
  # Warning
  if (ncol(df) == 1) {
    warning("Only one column detected: check file separator")
  }
  
  return(list(dat = df,
              sep = fsep))
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
#'
#' @return A list with 1 or 2 components.
#' If records_path is a json file, returns a list with one component
#' $camtrap_data which contains a captrapDP list.
#' If records_path is a csv file, returns a list with 2 components:
#'  $camtrap_data: a list with one component: 
#'    $data: a list with 2 components: 
#'      $observations (records)
#'       $deployments (cameras: if no camera file, is NULL)
#'  $sep: a list with 2 components (separators used to read files):
#'       $sep_records (for records)
#'       $sep_cameras (for cameras: if no cameras_path is NULL, it is NULL)
#' @export
read_data <- function(records_path,
                      sep_records, 
                      cameras_path = NULL,
                      sep_cameras = NULL) {
  
  # Get file extension
  ext <- tools::file_ext(records_path)
  
  if (ext == "csv") { # User uploaded a csv file
    # Read csv
    res_records <- read_csv(file_path = records_path, 
                            column_separator = sep_records)
    
    if (!is.null(cameras_path)) { # User wants to import a camera file
      # Read csv
      res_cameras <- read_csv(file_path = cameras_path, 
                              column_separator = sep_cameras)
    } else { # User doesn't want to import a camera file
      res_cameras <- list(dat = NULL,
                          sep = NULL)
    }
    
    res <- list(camtrap_data = list(data = list(observations = res_records$dat,
                                                deployments = res_cameras$dat)),
                sep = list(sep_records = res_records$sep,
                           sep_cameras = res_cameras$sep))
    
  } else if (ext == "json") { #   CamtrapDP format
    dat <- camtraptor::read_camtrap_dp(records_path, media = FALSE)
    res <- list(camtrap_data = dat)
  } else { # Unknown extension
    validate(need(ext == "csv" || ext == "json", 
                  "Please upload a csv file or a json datapackage"))
  }
  return(res)
}


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
    pat <- regex_list[w]
    
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

#' Cast columns to expected types
#'
#' @param df A dataframe containing the columns specified in col_mapping (values)
#' @param cast_type A named vector with the type conversion to perform.
#' Must be a valid function name to call.
#' Names are the names of the columns to cast in df.
#'
#' @return the df with casted columns
#' @export
#'
#' @examples
#' df <- data.frame(num = 1:10,
#'                  char = letters[1:10])
#' cast <- c(num = "as.character",
#'           char = "as.factor")
#' dfcast <- cast_columns(df, cast)
cast_columns <- function(df, cast_type) {
  
  # Initialize res
  res <- df
  for (i in 1:length(cast_type)) {
    castfunc <- cast_type[i]
    col <- names(cast_type)[i]
    
    res[[col]] <- do.call(castfunc, 
                          list(res[[col]]))
    
  }
  
  # # Get column codes
  # col_codes <- names(col_mapping)
  # # If _cov present in column names, remove it
  # col_codes <- gsub("_cov$", "", col_codes)
  # 
  # # Rename col_mapping
  # col_mapping_nocov <- col_mapping
  # names(col_mapping_nocov) <- col_codes
  
 
  return(res)
}

#' Cleans a dataframe
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
  res <- remove_rows_with_NA(res, vec)
  
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


# Summary -----------------------------------------------------------------

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
  
  if (is.null(timestamp_col)) { # no timestamp
    if (is.null(date_col) | is.null(time_col)) {
      stop("If timestamp_col is not provided, date_col and time_col must be provided.")
    }
  }
  
  
  if (is.null(timestamp_col)) { # no timestamp
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
                                          sep = ": ")
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
