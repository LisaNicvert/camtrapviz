# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: functions to read data


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
#' Returns a vector of the mapping for example datasets
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
#' Get a vector named with values in column "widget" 
#' from a dataframe
#'
#' @param df The dataframe. Must have a column named widget
#' and a column named like col.
#' @param col  The column of the dataframe to extract
#' @param widget_values The widgets to get the vector for
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
#' optionally from another csv file (with camera data).
#'
#' @param records_path A valid file path for records.
#' @param sep_records separator used for the records.
#' @param sep_cameras separator used for the cameras (defaults to `NULL`).
#' @param cameras_path A valid file path for cameras (defaults to `NULL`).
#' @param NA_strings Vector of characters that should be considered `NA`
#' after import
#'
#' @return A list with one component or a `datapackage` object.
#' + if `records_path` is a json file, returns a `datapackage` object
#' (inheriting list). The data is in the `$data` slot, 
#' which is a list with 2 components: 
#'    + `$deployments` (cameras)
#'    + `$observations` (records)
#' + if `records_path` is a csv file, returns a list with one component
#' named `$data`, which is a list with 2 components: 
#'    + `$deployments` (cameras: if no camera file was provided in `cameras_path`,
#'     it is `NULL`)
#'    + `$observations` (records)
#'    
#' @export
#' @examples
#' \dontrun{
#' # Read only records
#' read_data(records_path = "records.csv", sep_records = ",")
#' # Read records and cameras
#' read_data(records_path = "records.csv", sep_records = ",",
#'           cameras_path = "cameras.csv", sep_cameras = ",")
#' # Read a json file
#' read_data(records_path = "datapackage.json")
#' }
read_data <- function(records_path,
                      sep_records, 
                      cameras_path = NULL,
                      sep_cameras = NULL,
                      NA_strings = c("NA", "")) {
  
  # Get file extension
  ext <- tools::file_ext(records_path)
  
  if (ext == "csv") { # The input is a csv file
    # Read csv
    res_records <- utils::read.csv(records_path, sep = sep_records,
                                   na.strings = NA_strings)
    
    if (!is.null(cameras_path)) { # There is a camera file
      # Read csv
      res_cameras <- utils::read.csv(cameras_path, sep = sep_cameras,
                                     na.strings = NA_strings)
    } else { # There is no camera file
      res_cameras <- NULL
    }
    
    res <- list(data = list(observations = res_records,
                            deployments = res_cameras))
    
  } else if (ext == "json") { # CamtrapDP format
    res <- camtraptor::read_camtrap_dp(records_path, media = FALSE)
  } else { # Unknown extension
    validate(need(ext == "csv" || ext == "json", 
                  "Please upload a csv file or a json datapackage"))
  }
  return(res)
}