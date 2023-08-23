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
#' @noRd
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
#' @noRd
#' @return A named character vector, excluding NA values in col
get_example_mapping <- function(df, col) {
  
  res_df <- df |> 
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
#' @noRd
#' @return A named list containing the values of col, 
#' names are the widget names
get_named_list <- function(df, col, widget_values) {
  
  res_df <- df |>
    filter(widget %in% widget_values) 
  res <- res_df[[col]]
  res <- as.list(res)
  names(res) <- res_df$widget
  return(res)
}

#' Read data
#'
#' Reads data from a file path (either csv of json file), and 
#' optionally from another csv file (with camera data).
#'
#' @param path_rec A valid file path for records.
#' @param sep_rec separator used for the records.
#' @param sep_cam separator used for the cameras (defaults to `NULL`).
#' @param path_cam A valid file path for cameras (defaults to `NULL`).
#' @param NA_strings Vector of characters that should be considered `NA`
#' after import
#' @param add_rowid Should row IDs be added to the observations df?
#' If yes, row names in the form of "ID_xx" are added to the the dataframe.
#'
#' @return A list with one component or a `datapackage` object.
#' + if `path_rec` is a json file, returns a `datapackage` object
#' (inheriting list). The data is in the `$data` slot, 
#' which is a list with 2 components: 
#'    + `$deployments` (cameras)
#'    + `$observations` (records)
#' + if `path_rec` is a csv file, returns a list with one component
#' named `$data`, which is a list with 2 components: 
#'    + `$deployments` (cameras: if no camera file was provided in `path_cam`,
#'     it is `NULL`)
#'    + `$observations` (records)
#'    
#' @export
#' 
#' @seealso [\code{vignette("read-and-clean-data", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/read-and-clean-data.html)
#' 
#' @examples
#' \dontrun{
#' # Read only records
#' read_data(path_rec = "records.csv", sep_rec = ",")
#' # Read records and cameras
#' read_data(path_rec = "records.csv", sep_rec = ",",
#'           path_cam = "cameras.csv", sep_cam = ",")
#' # Read a json file
#' read_data(path_rec = "datapackage.json")
#' }
read_data <- function(path_rec,
                      sep_rec, 
                      path_cam = NULL,
                      sep_cam = NULL,
                      NA_strings = c("NA", ""),
                      add_rowid = FALSE) {
  
  # Get file extension
  ext <- tools::file_ext(path_rec)
  
  if (ext == "csv") { # The input is a csv file
    # Read csv
    res_records <- utils::read.csv(path_rec, sep = sep_rec,
                                   na.strings = NA_strings)
    
    if (!is.null(path_cam)) { # There is a camera file
      # Read csv
      res_cameras <- utils::read.csv(path_cam, sep = sep_cam,
                                     na.strings = NA_strings)
    } else { # There is no camera file
      res_cameras <- NULL
    }
    
    res <- list(data = list(observations = res_records,
                            deployments = res_cameras))
    
  } else if (ext == "json") { # CamtrapDP format
    res <- camtraptor::read_camtrap_dp(path_rec, media = FALSE)
  } else { # Unknown extension
    validate(need(ext == "csv" || ext == "json", 
                  "Please upload a csv file or a json datapackage"))
  }
  
  if (add_rowid) {
    # res$data$observations <- res$data$observations |> 
    #   tibble::rowid_to_column() 
    nrow <- nrow(res$data$observations)
    res$data$observations <- as.data.frame(res$data$observations)
    rownames(res$data$observations) <- paste("ID", 1:nrow,
                                             sep = "_")
  }
  
  return(res)
}
