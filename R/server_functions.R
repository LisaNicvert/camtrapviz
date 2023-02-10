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
#' @param widget_list a list of widgets to choose defaults for
#' (see details to see which widgets are accepted)
#' @param colnames a vector of column names
#' @param empty_allowed_list a vector of allowed widgets that can be set
#' to empty
#' @param empty_placeholder the empty placeholder
#'
#' @return A named list containing either (the first) matched colname,
#' NULL (no match and empty not allowed) 
#' or the empty_placeholder(no match and empty allowed)
#' 
#' @details currently implemented for the following widgets:
#'    spp_col, cam_col, date_col, time_col, timestamp_col,
#'    count_col, lat_col, lon_col
#' @export
#'
#' @examples
#' library(camtraptor)
#' data(mica)
#' colnames <- colnames(mica$data$observations)
#' find_default_colnames(c("spp_col", "cam_col", "timestamp_col"), 
#'                       colnames)
find_default_colnames <- function(widget_list,
                                  colnames,
                                  empty_allowed_list = list(),
                                  empty_placeholder = "Not present in data") {
  
  # Create a list with all possible columns we want
  widget_list_all <- c("spp_col", "cam_col",
                       "date_col", "time_col", "timestamp_col",
                       "count_col", "obs_col","lat_col", "lon_col")

  # Get the corresponding regex
  regex_list_all <- c("^vernacularNames\\.en$|species", "station|deployment|camera",
                      "date", "hour|time(?!stamp)", "timestamp|datetime",
                      "count", "observationType", "lat", "lon")
  
  names(regex_list_all) <- widget_list_all
  # Add covariates (duplicate regex)
  cov <- regex_list_all[grep(pattern = "^cam_col$|^lat_col$|^lon_col$", 
                             names(regex_list_all))]
  names(cov) <- paste(names(cov), "cov", sep = "_")
  regex_list_all <- c(regex_list_all,
                      cov)
  
  # Initialize results
  res <- vector(mode = "character", length = length(widget_list))
  
  for (i in 1:length(widget_list)) { # Iterate through input widgets
    w <- widget_list[i]
    pat <- regex_list_all[w]
    
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
    } else {
      res_i <- list(res_i)
    }
    # Add result
    res[i] <- res_i 
    # Name result
    names(res)[i] <- w
  }  
  return(res)
}

#' Cast columns to expected types
#'
#' @param df A dataframe containing the columns specified in col_mapping (values)
#' @param col_mapping A named character vector: names are toe codes and values are 
#' the corresponding column names
#'
#' @return the df with casted columns
#' @export
#'
#' @examples
#' library(camtraptor)
#' data(mica)
#' mapping <- c("col_spp" = "vernacularNames.en",
#'              "cam_col" = "deploymentID",
#'              "timestamp_col" = "timestamp")
#' cast_columns(mica$data$observations, mapping)
#' mapping <- c("cam_col_cov" = "deploymentID", 
#'              "lat_col_cov" = "latitude", 
#'              "lon_col_cov" = "longitude")
#' cast_columns(mica$data$deployments, mapping)
cast_columns <- function(df, col_mapping) {
  
  # Get column codes
  col_codes <- names(col_mapping)
  # If _cov present in column names, remove it
  col_codes <- gsub("_cov$", "", col_codes)
  
  # Rename col_mapping
  col_mapping_nocov <- col_mapping
  names(col_mapping_nocov) <- col_codes
  
  # Initialize res
  res <- df
  
  # Cast species
  if ("spp_col" %in% col_codes) {
    col_name <- col_mapping_nocov["spp_col"]
    res[[col_name]] <- as.character(res[[col_name]])
  }
  # Cast camera
  if ("cam_col" %in% col_codes) {
    col_name <- col_mapping_nocov["cam_col"]
    res[[col_name]] <- as.character(res[[col_name]])
  }
  # Cast observation type
  if ("obs_col" %in% col_codes) {
    col_name <- col_mapping_nocov["obs_col"]
    res[[col_name]] <- as.character(res[[col_name]])
  }
  # Cast date
  if ("date_col" %in% col_codes) {
    col_name <- col_mapping_nocov["date_col"]
    res[[col_name]] <- as_date(res[[col_name]])
  }
  # Cast time
  if ("time_col" %in% col_codes) {
    col_name <- col_mapping_nocov["time_col"]
    res[[col_name]] <- chron::times(res[[col_name]])
  }
  # Cast datetime
  if ("timestamp_col" %in% col_codes) {
    col_name <- col_mapping_nocov["timestamp_col"]
    res[[col_name]] <-  as_datetime(res[[col_name]])
  }
  # Cast lat
  if ("lat_col" %in% col_codes) {
    col_name <- col_mapping_nocov["lat_col"]
    res[[col_name]] <- as.numeric(res[[col_name]])
  }
  # Cast lon
  if ("lon_col" %in% col_codes) {
    col_name <- col_mapping_nocov["lon_col"]
    res[[col_name]] <- as.numeric(res[[col_name]])
  }
  # Cast count
  if ("count_col" %in% col_codes) {
    col_name <- col_mapping_nocov["count_col"]
    res[[col_name]] <- as.numeric(res[[col_name]])
  }
  return(res)
}

#' Cleans a dataframe
#' 
#' Moves columns indicated in mapping to the beginning,
#' and casts those columns.
#'
#' @param df The dataframe to clean
#' @param mapping The mapping for columns in the dataframe.
#'
#' @return The dataframe df with cleaned columns.
#' 
#' @export
#'
#' @examples
#' library(camtraptor)
#' data(mica)
#' mapping <- c("col_spp" = "vernacularNames.en",
#'              "cam_col" = "deploymentID",
#'              "timestamp_col" = "timestamp")
#' format_table(mica$data$observations, mapping)
format_table <- function(df, mapping) {
  
  res <- df %>%
    dplyr::select(all_of(unname(mapping)), 
                  everything())
  
  # Cast columns
  res <- cast_columns(res,
                      mapping)
  
  # Drop NA
  res <- remove_rows_with_NA(res, mapping)
  
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
