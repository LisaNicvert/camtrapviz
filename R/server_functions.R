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


# Summarize cameras -------------------------------------------------------

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

#' Summarize cameras
#' 
#' Summarize information about cameras activity from camera trap data.
#' 
#' @param df the records dataframe: must contain the camera names and some
#' infortmation on the pictures sampling time (date and time or datetime).
#' @param cam_col name of the column containing the camera ID
#' @param timestamp_col name of the column containing timestamps for the pictures
#' (optional if `date_col` and `time_col` are provided)
#' @param date_col name of the column containing date (optional if `timestamp_col` is provided)
#' @param time_col name of the column containing time (optional if `timestamp_col` is provided)
#' @param dfcam the dataframe of cameras deployments (optional). If it is provided,
#' at least `setup_col` or `retrieval_col` must also be provided.
#' @param cam_col_dfcam name of the column containing camera ID in `dfcam` 
#' If `dfcam` is provided but `cam_col_dfcam` is `NULL`, it will be set to `cam_col`.
#' @param setup_col name of the column containing setup date or datetime 
#' in `dfcam` (optional if `retrieval_col` is provided)
#' @param retrieval_col name of the column containing retrieval date or datetime 
#' in `dfcam` (optional if `setup_col` is provided)
#' 
#' @details In the final dataframe, the start and the end of the sampling are
#' computed as follows for each camera:
#' + if setup and retrieval date are provided in `dfcam`, then these
#' dates are used for the start and the end of the sampling in the summary.
#' + for the cameras for which this information is not provided,
#' it will be replaced with the date of the first or the last picture
#' of the camera.
#' The information on how the start and the end of the sampling 
#' were computed is stored in `setup_origin` and `retrieval_origin`.
#' + if setup and retrieval date are provided via `dfcam`, 
#' `setup_origin` is `setup` and `retrieval_origin` is ``retrieval`.
#' + else, these columns contain `picture`.
#' As this function uses the `cameraOperation` function from 
#' the `camtrapR` package, the camera names may not contain 
#' `Cam` as it is a reserved name in this function.
#' 
#' @return The summary is returned as a dataframe with the following columns:
#' + a column named as `cam_col` containing the camera ID.
#' + `setup` containing the start of the sampling for each camera.
#' + `retrieval` containing the end of the sampling for each camera.
#' + `setup_origin` containing the method used to determine the
#' start of the sampling (`picture` or `setup`)
#' + `retrieval_origin` containing the method used to determine the
#' end of the sampling (`picture` or `retrieval`)
#' + `sampling_length` length of the sampling period in days (computed
#' with the `cameraOperation` function from the `camtrapR` package).
#' 
#' @export
#' 
#' @examples
#' # Create synthetic data
#' records <- data.frame(species = c("pigeon", "mouse", "pigeon", "mouse", "mouse"),
#'                       stamp = as.POSIXct(c("2022-01-01 10:22:01", "2022-03-01 22:12:01",
#'                                            "2022-01-02 11:54:33", "2022-01-12 07:14:38", 
#'                                            "2022-01-22 18:01:34")),
#'                       camera = c("A", "A", "B", "B", "B"))
#' cameras <- data.frame(camera = c("A", "B", "C"),
#'                       setup = as.Date(c(NA, "2021-12-01", "2021-12-01")),
#'                       retrieval = as.Date(c("2022-03-01", "2022-03-01", NA)))
#' # Summarize cameras
#' summarize_cameras(records, 
#'                   cam_col = "camera", timestamp_col = "stamp", 
#'                   dfcam = cameras, 
#'                   setup_col = "setup", retrieval_col = "retrieval")
#' # Since camera A had no setup date, the first picture is used.
#' # For camera B, setup and retrieval are taken from dfcam.
#' # For camera C, as it is present only on dfcam and has no retrieval date,
#' # only a setup date is indicated.
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
    if (is.null(cam_col_dfcam)) { # Set cam_col_dfcam to cam_col
      cam_col_dfcam <- cam_col
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
  camsum <- camsum |>
    group_by(.data[[cam_col]]) |>
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
      setup_df <- setup_df |>
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
      retrieval_df <- retrieval_df |>
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
  camsum_sampling_time <- camsum |>
    filter(!is.na(setup)) |>
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
  # Remove added suffix Camxx
  names(sampling_length) <- gsub(names(sampling_length),
                                 pattern = "Cam\\d+$", replacement = "")
  sampling_length <- sampling_length[match(camsum[[cam_col]], names(sampling_length))]
  # (will set the cameras of camsum without sampling_length
  # to NA which is what we want)
  
  # Modify result
  camsum$sampling_length <- unname(sampling_length)
  
  return(camsum)
}
