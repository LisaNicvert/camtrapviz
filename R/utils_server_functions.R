# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: server functions


# Default colnames --------------------------------------------------------

#' Find default colname
#'
#' Finds a column name matching the given pattern among a vector 
#' of column names given in argument.
#' 
#' @param pattern a regular expression to look for in `colnames`
#' @param colnames a vector of column names
#' @param empty_placeholder a character to use as placeholder
#' if there is no match for `pattern` in `columns`.
#' @param empty_allowed logical; is it allowed to return 
#' the character in `empty_placeholder` if there is no match?
#'
#' @return
#' + if there was at least a match: all matched columns names 
#' + if there was no match:
#'    + if `empty_allowed`, returns the empty placeholder
#'    + else, returns `NULL`
#' 
#' @examples 
#' # There is a match
#' camtrapviz:::find_default_colname("species",
#'                                   colnames = c("Species", "cameraID", "DateTime"),
#'                                   empty_allowed = TRUE)
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
#' Finds the column names to default to for each element of the 
#' regular expression list among the column names given in argument.
#' 
#' @param regex_list a named list where values are regular expressions 
#' used to search in `colnames` and names are the same as the names
#' of `empty_allowed_list`. In the Shiny app, this list's names are 
#' the widget names.
#' @param colnames a vector of column names
#' @param empty_allowed_list a list or vector 
#' containing the names of the elements of `regex_list` that can
#' return `NULL` if there is no match
#' @param empty_placeholder  a character to use as placeholder
#' if there is no match for some elements of `regex_list` in `colnames`.
#'
#' @return A list with the same names as `regex_list`. 
#' Each element is the first (or the only) matched column name 
#' for the corresponding regular expression in `regex_list`.
#' If there was no match for one element:
#'    + if `empty_allowed`, returns the empty placeholder
#'    + else, returns `NULL`
#' 
#' @export
#'
#' @examples
#' colnames <- c("speciesName", "CameraID", "Datetime")
#' regex <- c("species", "station|deployment|camera",
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
    
    # Define empty_allowed logical
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


# Summary module -------------------------------------------------------

#' Get cameras not in
#'
#' Using two dataframes in input, determine which cameras 
#' are in one of the tables but not in the other one.
#'
#' @param dfrecords records dataframe
#' @param dfcameras cameras dataframe
#' @param cam_col_records name of the cameras column in the records dataframe
#' @param cam_col_cameras name of the cameras column in the cameras dataframe
#'
#' @return A named list with two components
#' + `$not_in_records`: cameras from `dfcameras` that are not in `dfrecords`.
#' If all cameras from `dfcameras` are in `dfrecords`, this is a character vector 
#' of length zero.
#' + `$not_in_cameras`: cameras from `dfrecords` that are not in `dfcameras`
#' If all cameras from `dfrecords` are in `dfcameras`, this is a character vector 
#' of length zero.
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
#' Display a message to say which cameras are absent from the records or
#' cameras dataframe.
#'
#' @param cameras character vector of camera names
#' @param type type of message to print: allowed values are 
#' `not_in_records` or `not_in_cameras`. Depending on this argument,
#' the sentence will state that input cameras are not in the records 
#' or in the cameras dataframe.
#'
#' @return A message describing which cameras are missing where. 
#' If no cameras are missing (i.e. `cameras` is a vector of length
#' zero), returns the empty string "".
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