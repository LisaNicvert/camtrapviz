# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-25
#
# Script Description: functions to handle camera trap data

#' Get cameras
#' 
#' Get a unique of all cameras present either in one list or in the
#' other.
#'
#' @param cam1 Character vector of camera names.
#' @param cam2 Character vector of camera names.
#' @param NA.last Value of the NA.last argument in `unique` and 
#' `sort` functions
#'
#' @return A vector of unique cameras that are present in both vectors.
#' NAs are kept and placed in the last position. Alphabetical order is used.
#' 
#' @export
#'
#' @examples
#' cam1 <- c("C1", "C2", "C3", NA)
#' cam2 <- c("C2", "C3", "C4")
#' get_cameras(cam1, cam2)
get_cameras <- function(cam1, cam2, NA.last = TRUE) {
  
  cam1r <- unique(cam1, NA.last = NA.last)
  cam2r <- unique(cam2, NA.last = NA.last)
  
  cam <- sort(unique(c(cam1r, cam2r)), na.last = NA.last)
  return(cam)
}


#' Get species count
#'
#' @param df a dataframe
#' @param species_col name of the species column from the dataframe
#' @param obs_col name of the observation type column from the dataframe
#' @param keep_NA count NAs in the species length?
#'
#' @return The number of species. If `obs_col` is provided, will
#' ignore all species with `obs_col` value different from `animal`.
#' If `keep_NA`, will count NA in the total species count.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(obstype = c("animal", "animal", "animal", "animal", "blank"),
#'                  species = c("cat", "cat", "cow", "dog", NA))
#' get_nspecies(df, species_col = "species", obs_col = "obstype")
get_nspecies <- function(df, species_col, obs_col = NULL,
                         keep_NA = FALSE) {
  
  if (!is.null(obs_col)) {
    # Filter to get only animal species
    species <- df[[species_col]][df[[obs_col]] == "animal"]
  } else {
    species <- df[[species_col]]
  }
  
  if (!keep_NA) {
    # Remove Nas
    species <- species[!is.na(species)]
  }
  
  n <- length(unique(species))
  
  return(n)
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
#' `setup_origin` is `setup` and `retrieval_origin` is `retrieval`.
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
#' If `setup` or `retrieval` are `NA`, then `sampling_length` is `NA`
#' and if `setup` and `retrieval` are the same (e.g. unique picture),
#' `sampling_length` is zero.
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
  # and where setup =/retrieval (can't compute sampling length
  # if only one pic)
  camsum_sampling_time <- camsum |>
    filter(!is.na(setup)) |>
    filter(!is.na(retrieval)) |>
    filter(setup != retrieval)
  
  # if we did not filter out everything, recompute sampling_length
  if (nrow(camsum_sampling_time) != 0) {
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
  } else {
    camsum$sampling_length <- NA
  }
  
  # Set sampling length to zero where setup = retrieval
  camsum <- camsum |> 
    mutate(sampling_length = ifelse(setup == retrieval, 0, sampling_length))
  
  # Tibble to df
  camsum <- as.data.frame(camsum)
  return(camsum)
}
