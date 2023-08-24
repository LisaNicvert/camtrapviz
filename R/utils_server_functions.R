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
#' @noRd
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
#' @noRd
#' @return A list with the same names as `regex_list`. 
#' Each element is the first (or the only) matched column name 
#' for the corresponding regular expression in `regex_list`.
#' If there was no match for one element:
#'    + if `empty_allowed`, returns the empty placeholder
#'    + else, returns `NULL`
#'
#' @examples
#' colnames <- c("speciesName", "CameraID", "Datetime")
#' regex <- c("species", "station|deployment|camera",
#'            "timestamp|datetime")
#' names(regex) <-  c("spp_col", "cam_col", "datetime_col")
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

#' Get non-shared cameras between two dataframes
#'
#' Using two dataframes in input, determine which cameras 
#' are in one of the tables but not in the other one.
#'
#' @param dfrecords records dataframe
#' @param dfcameras cameras dataframe
#' @param cam_col_dfrec name of the cameras column in the records dataframe
#' @param cam_col_dfcam name of the cameras column in the cameras dataframe
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
#'                    cam_col_dfrec = "camID",
#'                    cam_col_dfcam = "cameras")
get_cameras_not_in <- function(dfrecords, 
                               dfcameras,
                               cam_col_dfrec,
                               cam_col_dfcam) {
  
  # Get cameras from records and camera file
  records_cameras <- unique(dfrecords[[cam_col_dfrec]])
  deployments_cameras <- dfcameras[[cam_col_dfcam]]
  
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
#' @noRd
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


# Plots -------------------------------------------------------------------

#' Get height and width of a plot
#' 
#' Get the height and width of a plot in inches given ncam
#' and daterange.
#' 
#' @param ncam Number of cameras
#' @param daterange Date range
#'
#' @return Named list height and width
#' @noRd
get_hw <- function(ncam, daterange = NULL) {
  
  # Define height
  unith <- ncam/4
  height <- max(5, 
                unith/(1 + exp(-12*unith)))
  if (!is.null(daterange)) {
    # Define width
    unitw <- as.numeric(daterange[2] - daterange[1], "days")/60 # One inch per 2 months
    width <- max(8,
                 unitw/(1 + exp(-24*unitw)))
  } else {
    width <- 8
  }
  
  
  list(height = height,
       width = width)
}


# Check empty or NULL -----------------------------------------------------

#' Check if argument is null or the empty character
#'
#' @param arg The argument to test
#'
#' @return TRUE or FALSE
#' @noRd
empty_or_null <- function(arg) {
  res <- FALSE
  if (is.null(arg)) {
    res <- TRUE
  } else if (arg == "") {
    res <- TRUE
  }
  return(res)
}


# Timezones helpers ------------------------------------------------------------

#' Get best timezone
#'
#' @param custom_tz Custom (user provided) timezone
#' @param data_tz A timezone from the data
#' @param default_tz Default value
#'
#' @return A timezone
#' @noRd
get_tz <- function(custom_tz, data_tz, default_tz = "Etc/GMT") {
  
  if (empty_or_null(custom_tz)) { # no custom tz
      # Try to get timezone from data
      if (empty_or_null(data_tz)) {
        # If empty, set to default
        tz <- default_tz
      } else {
        # Else, set to tz of the data
        tz <- data_tz
      }
  } else {
    tz <- custom_tz
  }
  
  return(tz)
}

#' Add timezone to a vector
#'
#' @param vec The vector
#' @param tz The timezone
#' @param force_tz In case no timezone present, should one be added
#' with force_tz?
#' 
#' @details
#' If vec is not a POSIX, will be converted wih given tz.
#' Else, it depends if vec already has a timezone attribute:
#' + if yes, vec will be converted to the new timezone
#' + else, the new timezone will replace the empty timezone 
#' only if `force_tz` is `TRUE` (else, it will throw an error).
#' 
#' @return The vector with a timezone
#' @noRd
add_tz <- function(vec, tz, force_tz = FALSE) {
  
  # Initialize result
  res <- vec
  
  if (!("POSIXt" %in% class(vec))) {
    # If vec in not a POSIX, simply coerce to POSIX with a given timezone
    res <- as.POSIXct(res, tz = tz)
  } else { # If vec is a POSIX
    # Get vec tz
    tzone_vec <- attr(res, "tzone")
    if (empty_or_null(tzone_vec)) {
      # Override timezone if empty
      if (force_tz) {
        res <- lubridate::force_tz(res, 
                                   tz)
      } else {
        stop("Input POSIX vector has no timezone and it will not be modified unless force_tz is TRUE.")
      }
    } else { # If timezone of vec is not empty
      # Make the conversion
      if (tzone_vec != tz) { # Convert timezone only if not already the good one
        attr(res, "tzone") <- tz
      }
    }
  }
  return(res)
}


# Species -----------------------------------------------------------------

#' Get observation type or species
#'
#' From a dataframe obtained with get_unique_species,
#' separate which items were originally in the species
#' or in the observation type column
#' 
#' @param df df obtained with get_unique_species
#' @param spp_col Name of the column containing species names 
#' (where NAs have been replaced)
#' @param obstype_col Name of the column containing observation types
#'
#' @return A list with two components (can be `NULL`): 
#' `obstype` and `spp`
#' 
#' @noRd
get_obs_spp <- function(df, 
                        spp_col = NULL, 
                        obstype_col = NULL) {
  
  if (!is.null(obstype_col)) {
    # Get species that were originally NA
    NAs <- is.na(df[[paste0(spp_col, "_orig")]])
    
    # Species that were not NA are from the species column
    spp <- df[!NAs, spp_col]
    if(length(spp) == 0) {
      spp <- NULL
    }
    
    # Species that were NA are filtered from the observation type column
    obstype <- df[NAs, spp_col]
    if(length(obstype) == 0) {
      obstype <- NULL
    }
    
  } else {
    # Obs filter is NULL
    obstype <- NULL
    
    # spp filter is all species
    spp <- df[[spp_col]]
  }
  return(list(obstype = obstype,
              spp = spp))
}
