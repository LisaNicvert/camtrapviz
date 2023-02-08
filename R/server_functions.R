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
                       "count_col", "lat_col", "lon_col")

  # Get the corresponding regex
  regex_list_all <- c("^vernacularNames\\.en$|species", "station|deployment|camera",
                      "date", "hour|time(?!stamp)", "timestamp|datetime",
                      "count", "lat", "lon")
  names(regex_list_all) <- widget_list_all
  
  # Initialize results
  res <- vector(mode = "character", length = length(regex_list_all))
  names(res) <- names(regex_list_all)
  
  for (i in 1:length(widget_list)) { # Iterate through imput widgets
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
cast_columns <- function(df, col_mapping) {
  
  col_codes <- names(col_mapping)
  res <- df
  
  # Cast species
  if ("spp_col" %in% col_codes) {
    col_name <- col_mapping["spp_col"]
    res[[col_name]] <- as.character(res[[col_name]])
  }
  # Cast date
  if ("date_col" %in% col_codes) {
    col_name <- col_mapping["date_col"]
    res[[col_name]] <- as_date(res[[col_name]])
  }
  # Cast time
  if ("time_col" %in% col_codes) {
    col_name <- col_mapping["time_col"]
    res[[col_name]] <- chron::times(res[[col_name]])
  }
  # Cast datetime
  if ("timestamp_col" %in% col_codes) {
    col_name <- col_mapping["timestamp_col"]
    res[[col_name]] <-  as_datetime(res[[col_name]])
  }
  # Cast lat
  if ("lat_col" %in% col_codes) {
    col_name <- col_mapping["lat_col"]
    res[[col_name]] <- as.numeric(res[[col_name]])
  }
  # Cast lon
  if ("lon_col" %in% col_codes) {
    col_name <- col_mapping["lon_col"]
    res[[col_name]] <- as.numeric(res[[col_name]])
  }
  # Cast count
  if ("count_col" %in% col_codes) {
    col_name <- col_mapping["count_col"]
    res[[col_name]] <- as.numeric(res[[col_name]])
  }
  return(res)
}
