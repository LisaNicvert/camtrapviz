# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-25
#
# Script Description: functions to handle camera trap data


# Cameras -----------------------------------------------------------------

#' Get unique cameras vector
#' 
#' Get a unique vector of all cameras present either in one list or in the
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
#' @param dfcam the dataframe of cameras deployments (optional).
#' @param cam_col_dfcam name of the column containing camera ID in `dfcam` 
#' If `dfcam` is provided but `cam_col_dfcam` is `NULL`, it will be set to `cam_col`.
#' @param setup_col name of the column containing setup date or datetime 
#' in `dfcam` (optional)
#' @param retrieval_col name of the column containing retrieval date or datetime 
#' in `dfcam` (optional)
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
#' `setup_origin` and `retrieval_origin` are `metadata`.
#' + else, these columns contain `picture`.
#' + if `dfcam` is provided but has no setup or retrieval columns, then
#' the cameras will be added but all columns except camera name will be `NA`.
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
  
  if (!is.null(dfcam)) { # if dfcam provided
    if (is.null(cam_col_dfcam)) { # Set cam_col_dfcam to cam_col
      cam_col_dfcam <- cam_col
    }
  }
  
  # --- Compute first and last picture
  camsum <- df
  
  if (missing(timestamp_col) || is.null(timestamp_col)) {
    # Create timestamp column
    camsum$timestamp <- paste(camsum[[date_col]],
                              camsum[[time_col]])
    camsum$timestamp <- as.POSIXct(camsum$timestamp,
                                   tz = "UTC")
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
                                          tz = lubridate::tz(camsum$setup))
      
      # Get indices to replace in camsum
      # We will replace all these indices because they are not null
      # and we keep the values that are missing in dfcam but were already
      # in camsum
      ind <- match(setup_df[[cam_col_dfcam]], camsum[[cam_col]])
      
      camsum$setup[ind] <- setup_df[[setup_col]]
      camsum$setup_origin[ind] <- "metadata"
    }
    if (!is.null(retrieval_col)) { # Retrieval date specified in cameras
      
      retrieval_df <- dfcam
      
      # Get non-NA retrieval
      retrieval_df <- retrieval_df |>
        filter(!is.na(.data[[retrieval_col]]))
      
      # Cast to POSIX
      retrieval_df[[retrieval_col]] <- as.POSIXct(retrieval_df[[retrieval_col]],
                                                  tz = lubridate::tz(camsum$setup))
      
      # Get indices to replace in camsum
      # We will replace all these indices because they are not null
      # and we keep the values that are missing in dfcam but were already
      # in camsum
      ind <- match(retrieval_df[[cam_col_dfcam]], camsum[[cam_col]])
      
      camsum$retrieval[ind] <- retrieval_df[[retrieval_col]]
      camsum$retrieval_origin[ind] <- "metadata"
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
    camsum_sampling_time$setup <- format(camsum_sampling_time$setup, 
                                         format = date_format)
    camsum_sampling_time$retrieval <- format(camsum_sampling_time$retrieval, 
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


# Species -----------------------------------------------------------------

#' Get species from a dataframe
#'
#' This function aims at giving the vector of species names 
#' corresponding to the observations from a dataframe.
#' It is primarily intended for dataframes where observations have a 
#' type, and some non-animal species are written as `NA` but a more 
#' general type is provided (as with the camtrapDP standard).
#' 
#' @param df The dataframe
#' @param spp_col name of the species column from the dataframe
#' @param obs_col name of the observation type column from the dataframe
#' @param animal_code value of `obs_col` coding for animal observations.
#' @param return_df return a dataframe? If `TRUE`, will return a dataframe 
#' (see below); else will return a character vector of species names.
#'
#' @return 
#' species names in the same order as `df`. 
#' If `obs_col` is provided,  `NA` values in `spp_col` are 
#' replaced with the corresponding value in `obs_col` 
#' (if `obs_col` is not `animal_code`). 
#' If `return_df` is `TRUE`, returns a dataframe containing 
#' species and observation type in the same order as `df`.
#' This dataframe has the following columns (type character):
#' + a column named like `spp_col` containing species names
#' (where `NA` values in `spp_col` are replaced as described above).
#' + a column named like `obs_col` containing unique corresponding
#' observations types (if `obs_col` is provided).
#' Else, returns only the character vector containing the values of 
#' `spp_col`.
#' 
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, 
#'                              "cameratrapper", "tourist"),
#'                  type = c("animal", "animal", "animal", "fire", "blank", 
#'                           "human", "human"))
#' # Use the type column
#' get_all_species(df, spp_col = "species", obs_col = "type")
#' # Use the type column but return a vector
#' get_all_species(df, spp_col = "species", return_df = FALSE)
#' 
#' # Don't use the type column
#' get_all_species(df, spp_col = "species")
get_all_species <- function(df,
                            spp_col, obs_col = NULL,
                            animal_code = "animal",
                            return_df = ifelse(is.null(obs_col), FALSE, TRUE)) {
  
  # Convert factor to character
  df[[spp_col]] <- as.character(df[[spp_col]])
  
  if (!is.null(obs_col)) {
    df[[obs_col]] <- as.character(df[[obs_col]])
  }
  
  if (!is.null(obs_col)) {
    # Get columns
    spp_df <- df[, c(spp_col, obs_col)]
    
    # Replace values with not animal ---
    is_non_animal <- spp_df[[obs_col]] != animal_code # Get non-animals
    is_non_animal[is.na(is_non_animal)] <- TRUE # Consider NAs as non-animals
    spp_df[[spp_col]][is_non_animal & is.na(spp_df[[spp_col]])] <- spp_df[[obs_col]][is_non_animal & is.na(spp_df[[spp_col]])]
  } else {
    # Df with a unique column
    spp_df <- data.frame(col = df[[spp_col]])
    colnames(spp_df) <- spp_col
  }
  
  if (!return_df) { # Return only spp_col column
    res <- spp_df[[spp_col]]
  } else {
    res <- spp_df
  }
  
  return(res)
}

#' Get unique species from a dataframe
#'
#' This function aims at giving the unique species names 
#' corresponding from a dataframe.
#' It is primarily intended for dataframes where observations have a 
#' type, and some non-animal species are written as `NA` but a more 
#' general type is provided (as with the camtrapDP standard).
#' 
#' @param df The dataframe
#' @param spp_col name of the species column from the dataframe
#' @param obs_col name of the observation type column from the dataframe
#' @param animal_code value of `obs_col` coding for animal observations.
#' @param return_df return a dataframe? If `TRUE`, will return a dataframe 
#' (see below); else will return a character vector of unique 
#' species names.
#' @param reorder Reorder the results? This will arrange values by 
#' alphabetical order. If `obs_col` is provided, non-animal species will
#' be arranged last.
#'
#' @return 
#' unique species names. 
#' If `obs_col` is provided,  `NA` values in `spp_col` are 
#' replaced with the corresponding value in `obs_col` 
#' (if `obs_col` is not `animal_code`). 
#' If `return_df` is `TRUE`, returns a dataframe containing 
#' unique species and observation type.
#' This dataframe has the following columns (type character):
#' + a column named like `spp_col` containing species names
#' (where `NA` values in `spp_col` are replaced as described above).
#' + a column named like `obs_col` containing unique corresponding
#' observations types (if `obs_col` is provided).
#' + if `obs_col` is not `NULL`, the rows of the dataframe are named 
#' as `spp_type` where `spp` is the species value and 
#' `type` is the observation type value. Else, rows are named as `spp`.
#' Else, returns only the unique values of 
#' `spp_col`.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, 
#'                              "cameratrapper", "tourist"),
#'                  type = c("animal", "animal", "animal", "fire", "blank", 
#'                           "human", "human"))
#' # Use the type column
#' get_unique_species(df, spp_col = "species", obs_col = "type",
#'                    reorder = TRUE)
#' # Use the type column but return a vector
#' get_unique_species(df, spp_col = "species", return_df = FALSE,
#'                    reorder = TRUE)
#' 
#' # Don't use the type column
#' get_unique_species(df, spp_col = "species",
#'                    reorder = TRUE)
get_unique_species <- function(df,
                        spp_col, obs_col = NULL,
                        animal_code = "animal",
                        return_df = ifelse(is.null(obs_col), FALSE, TRUE),
                        reorder = FALSE) {
  
  # Get a dataframe of non-unique species names
  spp_df_all <- get_all_species(df = df,
                                spp_col = spp_col, 
                                obs_col = obs_col,
                                animal_code = animal_code,
                                return_df = TRUE)
  
  if (!is.null(obs_col)) {
    # Get (unique) species ---
    spp_df <- spp_df_all |>
      distinct(.keep_all = TRUE)
    
    # Arrange with non-animal last ---
    if (reorder) {
      # Get factor levels
      levels <- sort(unique(spp_df[[obs_col]], na.last = TRUE), na.last = TRUE)
      levels <- c(animal_code, levels[levels != animal_code])
      
      spp_df[[obs_col]] <- factor(spp_df[[obs_col]], 
                                  levels = levels)
      spp_df <- spp_df |> dplyr::arrange(.data[[obs_col]],
                                         .data[[spp_col]])
      # Convert back to character
      spp_df[[obs_col]] <- as.character(spp_df[[obs_col]])
    }
  } else { # obs_col not provided
    spp_df <- spp_df_all[spp_col] |>
      distinct(.keep_all = TRUE)
    if (reorder) {
      spp_df <- spp_df |> dplyr::arrange(.data[[spp_col]])
    }
  }
  
  if (return_df) {
    # Add ID
    res <- as.data.frame(spp_df) # Convert tibble to df
    
    if (!is.null(obs_col)) {
      rownames(res) <- paste(res[[spp_col]], res[[obs_col]], sep = "_")
    } else {
      rownames(res) <- res[[spp_col]]
    }
    
  } else {
    res <- spp_df[[spp_col]]
  }
  
  return(res)
}


#' Get species count from a dataframe
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

#' Summarize species
#'
#' Summarize species information from a data table
#'
#' @param df the observation dataframe to summarize
#' @param spp_col Name of the species column
#' @param obs_col Name of the observation type column (optional)
#' @param count_col Name of the column containing species count (optional)
#' @param cam_col Name of the column containing camera codes (optional if
#' `by_cam` is `FALSE`)
#' @param ncam Number of cameras to take into account when computing
#' the proportion of cameras the species was ween on. If `NULL`, 
#' defaults to the number of cameras present in the `df` (not needed
#' if `cam_col` is not provided).
#' @param NA_count_placeholder Value with which to replace NAs present
#' in the column containing counts. Defaults to NA (i.e. values are not
#' replaced).
#' @param by_cam Should the values be summarized by camera? If yes, 
#' there will be one row per cameras-species  
#' @param keep_all_camera_levels If there is a camera on which no species were
#' seen, should it be present in the output? Not needed
#' if `cam_col` is not provided.
#'
#' @return A table summarizing species information with the following columns:
#' + Species (named like `spp_col`): species identity 
#' (same as the `spp_col` input column)
#' + Observation type (present only if `obs_col` is not `NULL` and named like 
#' `obs_col`): observation type (same as the `obs_col` input column)
#' + `sightings`: number of rows where the species was photographed.
#' + `individuals`: count of individuals observed on all
#' pictures (using the input `count_col` column).
#' If `count_col` is `NULL`, it contains the same values as `sightings`.
#' If there are NAs in the input  `count_col`, they will propagate in `individuals`
#' (unless a value is specified in `NA_count_placeholder`).
#' 
#' If `by_cam` is `FALSE`, the following rows are also present:
#' + `n_cameras` (present only if `cam_col` is not `NULL`) : the number of cameras the species was seen on.
#' + `prop_cam` (present only if `cam_col` is not `NULL`): the proportion of cameras the species was seen on.
#' If `ncam` is provided, then it uses `ncam` as the total number of cameras.
#' 
#' If `by_cam` is `TRUE`, the following rows are also present:
#' + `sightings_prop`: the proportion of sightings represented by the species at the camera.
#' + `individuals_prop`: the proportion of individuans represented by the species at the camera.
#' 
#' Finally, if `keep_all_camera_levels` is `TRUE`, a final column named
#' `empty` is added to indicate which cameras were empty (have no data).
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("zebra", "cat", "cat", "cow", NA, NA),
#'                  type = c("animal", "animal", "animal", "animal", "human", "blank"),
#'                  camera = c("C1", "C1", "C2", "C3", "C3", "C4"),
#'                  count = c(1, 1, 3, 50, 1, NA))
#' # Summarize species across all cameras
#' summarize_species(df, 
#'                   spp_col = "species", cam_col = "camera",
#'                   obs_col = "type",
#'                   count_col = "count",
#'                   NA_count_placeholder = 1)
#' # Summarize per species and cameras
#' summarize_species(df, 
#'                   spp_col = "species", cam_col = "camera",
#'                   obs_col = "type",
#'                   count_col = "count",
#'                   by_cam = TRUE,
#'                   NA_count_placeholder = 1)
summarize_species <- function(df, 
                              spp_col, 
                              cam_col = NULL,
                              obs_col = NULL,
                              count_col = NULL,
                              ncam = NULL,
                              by_cam = FALSE,
                              keep_all_camera_levels = FALSE,
                              NA_count_placeholder = NA) {
  
  # Check data input ---
  # Set and check ncam
  if (!is.null(cam_col)) {
    ncam_df <- length(unique(df[[cam_col]]))
    if (is.null(ncam)) {
      # Set ncam to the number of cameras in the data
      ncam <- ncam_df
    } else {
      if (ncam < ncam_df) {
        warning("ncam is smaller than the number of cameras in df: are you sure it's what you want?")
      }
    }
  }
  
  # Check that cam_col is provided if by_cam is TRUE
  if (by_cam) {
    if (is.null(cam_col)) {
      stop("cam_col must be provided when by_cam is TRUE.")
    }
  }
  
  
  # Replace NAs in count ---
  if (!is.null(count_col)) {
    NAs_in_count <- length(df[[count_col]][is.na(df[[count_col]])])
    
    if (NAs_in_count != 0) {
      if (is.na(NA_count_placeholder)) {
        warning("There are NAs in the count column; if you want to replace them with a value, use NA_count_placeholder.")
      } else {
        df[[count_col]][is.na(df[[count_col]])] <- NA_count_placeholder
      }
    }
  }
  
  # Group data ---
  if (!is.null(obs_col)) { # obs_col provided
    if (by_cam) {
      # Group by camera
      res <- df |> 
        group_by(.data[[spp_col]], .data[[obs_col]],
                 .data[[cam_col]])
    } else {
      # Don't group by camera
      res <- df |> 
        group_by(.data[[spp_col]], .data[[obs_col]])
    }
    
  } else {
    if (by_cam) {
      # Group by camera
      res <- df |> 
        group_by(.data[[spp_col]], .data[[cam_col]])
    } else {
      # Don't group by camera
      res <- df |> 
        group_by(.data[[spp_col]])
    }
  }
  
  # Summarize ---
  if (is.null(cam_col) || by_cam) {
    # Just add n sightings and individuals
    res <- res |> 
      summarise(sightings = n(),
                individuals = ifelse(is.null(count_col), 
                                       n(), # Simply count rows
                                       sum(.data[[count_col]]) # else sum count col
                ),
                .groups = "drop"
      )
    if (by_cam) {
      # Add the proportion of species seen on the camera
      res <- res |> 
        group_by(.data[[cam_col]]) |> 
        mutate(across(c("sightings", "individuals"), 
                      ~ .x/sum(.x),
                      .names = "{.col}_prop")
               ) |>
        dplyr::ungroup()
    }
    
  } else {
    # If cam_col is provided but we don't group by camera,
    # Add the number of cameras the species was seen on
    res <- res |> 
      summarise(sightings = n(),
                individuals = ifelse(is.null(count_col), 
                                     n(), # Simply count rows
                                     sum(.data[[count_col]]) # else sum count col
                ),
                n_cameras = length(unique(.data[[cam_col]])),
                .groups = "drop"
      )
    # Add prop_cam
    res$prop_cam <- res$n_cameras/ncam
  }
  
  
  if(!is.null(cam_col)) {
    # If keep_all_camera_levels, add unused levels
    if(keep_all_camera_levels & is.factor(df[[cam_col]])) {
      # Create df with missing factors
      cam_levels <- levels(as.factor(df[[cam_col]]))
      to_add <- cam_levels[!(cam_levels %in% res[[cam_col]])]
      
      # Add missing cameras
      if (length(to_add) != 0) {
        to_add <- factor(to_add, levels = cam_levels)
        
        missing_cams <- data.frame(cam = to_add, empty = TRUE)
        colnames(missing_cams)[1] <- cam_col
        
        res <- res |> 
          mutate(empty = FALSE) |>
          dplyr::bind_rows(missing_cams)
      }
    }
  }
  
  # Arrange table
  if (!by_cam) {
    if (is.null(obs_col)) {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       na.last = TRUE)
    } else {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       .data[[obs_col]],
                       na.last = TRUE)
    }
  } else {
    if (is.null(obs_col)) {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       .data[[cam_col]],
                       na.last = TRUE)
    } else {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       .data[[obs_col]],
                       .data[[cam_col]],
                       na.last = TRUE)
    }
  }
  
  # Convert tibble to dataframe
  res <- as.data.frame(res)
  
  return(res)
  
}


# Filter data -------------------------------------------------------------

#' Filter camera trap data
#' 
#' Allows to filter camera trap data observations and
#' cameras metadata based on species, cameras and dates.
#'
#' @param dat The data to filter. It can be either a list with one component `$data`
#' or a `datapackage` object (inheriting list). Either way, the data 
#' are in the `$data` slot with two components: 
#' + `$deployments` (cameras table)
#' + `$observations` (records table)
#' @param spp_filter Species to filter from the data
#' @param spp_col Name of the species column
#' @param obs_filter Observation types to filter from the data
#' @param obs_col Name of the observation column
#' @param cam_filter Cameras to filter from the data
#' @param cam_col_rec Name of the cameras column in records table (`dat$data$observations`).
#' @param cam_col_cam Name of the cameras column in cameras table (`dat$data$deployments`).
#' Defaults to the same value as `cam_col_rec`.
#' @param daterange Date range to filter on for the data (will filter 
#' observations' times so that `times >= daterange[1]` and 
#' `times <= daterange[2]`). Can be either a Date or a POSIX.
#' @param timestamp_col Name of the datetime column (must be coercible to POSIX).
#' It is not needed if `date_col` and `time_col` are provided.
#' @param date_col Name of the date column. It is not needed if `timestamp_col` 
#' is provided.
#' @param time_col Name of the time column. It is not needed if `timestamp_col` 
#' is provided.
#' @param cameras_as_factor Transform cameras as factors?
#' @param tz Timezone for the data bounds. If not provided, will try to 
#' find the timezone in `daterange` (if it is a POSIX), then in
#' `timestamp_col` (if provided), and finally if no timezone is 
#' present it will default to UTC (Etc/GMT).
#' For the filtering step, if needed datetimes in `timestamp_col` can be 
#' converted to `tz` but the output data will not be affected.
#' @param custom_col name of a custom column in to filter values in 
#' `dat$data$observations`.
#' @param custom_filter values to filter out in the custom column `custom_col`.
#' 
#' @details
#' For the `spp_filter`, `cam_filter`, `daterange` and `custom_col` values: 
#' if they are `NULL`, data are not filtered on that condition.
#' Also note that e.g. if all species are in `spp_filter`, then
#' all species will be filtered out.
#' 
#' @return The filtered data. Species and dates remove data only in `dat$data$observations`,
#' but cameras also remove cameras from `dat$data$deployments`.
#' 
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' recordTableSample$DateTimeOriginal <- as.POSIXct(recordTableSample$DateTimeOriginal)
#' data("camtraps", package = "camtrapR")
#' dat <- list(data = list(observations = recordTableSample,
#'                         deployments = camtraps))
#' # Filter out data for species PBE and VTA, camera Station A and keep
#' # only data from 2009-05-01 to 2009-05-15.
#' filter_data(dat, 
#'             spp_col = "Species", 
#'             spp_filter = c("PBE", "VTA"),
#'             cam_col_rec = "Station", 
#'             cam_filter = "StationA",
#'             daterange = as.Date(c("2009-05-01", "2009-05-15")),
#'             timestamp_col = "DateTimeOriginal")
filter_data <- function(dat,
                        spp_filter = NULL,
                        spp_col = NULL,
                        obs_filter = NULL,
                        obs_col = NULL,
                        cam_filter = NULL,
                        cam_col_rec = NULL,
                        cam_col_cam = cam_col_rec,
                        daterange = NULL,
                        custom_col = NULL,
                        custom_filter = NULL,
                        timestamp_col = NULL,
                        time_col = NULL,
                        date_col = NULL,
                        tz = NULL,
                        cameras_as_factor = FALSE
                        ) {
  
  rec <- dat$data$observations
  
  if (!is.null(daterange)) {
    if (is.null(timestamp_col)) { # no timestamp
      if (is.null(date_col) | is.null(time_col)) {
        stop("If timestamp_col is not provided, date_col and time_col must be provided.")
      }
      if( !(date_col %in% colnames(rec))) {
        stop("date_col must be a column of dat$data$observations.")
      }
      if( !(time_col %in% colnames(rec))) {
        stop("time_col must be a column of dat$data$observations.")
      }
    } else { # If provided, timestamp must be in df
      if( !(timestamp_col %in% colnames(rec))) {
        stop("timestamp_col must be a column of dat$data$observations.")
      }
    }
  }
  
  if (!is.null(custom_filter)) {
    if (is.null(custom_col)) {
      stop("If custom_filter is provided, custom_col cannot be NULL.")
    }
  }
  
  res <- dat
  
  # Filter species  ---
  if (!is.null(spp_filter)) {
    # User wants to filter out species
    res$data$observations <- res$data$observations |>
      dplyr::filter(!(.data[[spp_col]] %in% spp_filter))
  }
  
  # Filter observations ---
  if (!is.null(obs_filter)) {
    # User wants to filter out observations
    res$data$observations <- res$data$observations |>
      dplyr::filter(!(.data[[obs_col]] %in% obs_filter))
  } 
  
  
  # Filter cameras ---
  if (!is.null(cam_filter)) {
    res$data$observations <- res$data$observations |>
      dplyr::filter(!(.data[[cam_col_rec]] %in% cam_filter))
    res$data$deployments <- res$data$deployments |>
      dplyr::filter(!(.data[[cam_col_cam]] %in% cam_filter))
  }
  
  
  # Filter daterange ---
  if (!is.null(daterange)) {
    
    # Set the timezone ---
    default_tz <- "Etc/GMT"
    if (!is.null(timestamp_col)) {
      data_tz <- attr(res$data$observations[[timestamp_col]], "tzone")
    } else {
      data_tz <- NULL
    }
    tz <- get_tz(custom_tz = tz, 
                 data_tz = data_tz, 
                 default_tz = "Etc/GMT")
    
    if (is.null(timestamp_col)) { # no timestamp_col column
      # Create a composite timestamp with custom timezone
      timestamp_vec <- paste(as.character(res$data$observations[[date_col]]), 
                             as.character(res$data$observations[[time_col]]))
      
      timestamp_vec <- as.POSIXct(timestamp_vec,
                                  tz = tz)
    } else { # timestamp_col not NULL
      timestamp_vec <- add_tz(res$data$observations[[timestamp_col]],
                              tz = tz,
                              force_tz = TRUE)
    }
    
    # Convert daterange to POSIX with appropriate timezone
    daterange_filter <- add_tz(daterange,
                               tz = tz,
                               force_tz = TRUE)
    
    # Filter dates
    sel <- dplyr::between(timestamp_vec, 
                          daterange_filter[1],
                          daterange_filter[2])
    res$data$observations <- res$data$observations[sel, ]
    
  }
  
  # Custom filter ---
  if (!is.null(custom_col)) {
    res$data$observations <- res$data$observations |>
      dplyr::filter(!(.data[[custom_col]] %in% custom_filter))
  }
  
  # Cameras to factor ---
  if (cameras_as_factor) {
    cameras_list <- get_cameras(res$data$observations[[cam_col_rec]],
                                res$data$deployments[[cam_col_cam]])
    
    res$data$observations[[cam_col_rec]] <- factor(res$data$observations[[cam_col_rec]],
                                                   levels = cameras_list)
    res$data$deployments[[cam_col_cam]] <- factor(res$data$deployments[[cam_col_cam]],
                                                  levels = cameras_list)
  }
  
  return(res)
}


# Diversity ---------------------------------------------------------------

#' Get diversity indices
#' 
#' From a summary table of camera/species, return richness, Shannon
#' and Simpson diversity indices.
#'
#' @param count_df Dataframe summarizing counts per species. Can be 
#' computed using the records dataframe with the `summarize_species`
#' function.
#' @param spp_col Name of the column containing species names
#' @param cam_col Name of the column containing cameras names
#' @param count_col Name of the column containing species counts
#' @param prop_col Name of the column containing species proportions
#'
#' @return A dataframe with one row per camera summarizing diversity indices.
#' 
#' @details Computes the richness, Shannon and Simpson indices.
#' See [vignette](https://lisanicvert.github.io/camtrapviz/articles/diversity.html)
#' (or run \code{vignette("diversity", package = "camtrapviz")}) 
#' for details on the formulas of the diversity indices.
#' 
#' @export
#'
#' @examples
#' countdf <- data.frame(camera = c("C1", "C1", "C1", "C2",
#'                                  "C3", "C3", "C3"),
#'                       species = c("cat", "cow", "rabbit",
#'                                   "cat", "cat", "cow", "rabbit"),
#'                       individuals = c(30, 30, 30, 30, 88, 1, 1),
#'                       individuals_prop = c(1/3, 1/3, 1/3, 1, 88/90, 1/90, 1/90))
#' get_diversity_indices(countdf, 
#'                       spp_col = "species", cam_col = "camera")
get_diversity_indices <- function(count_df, spp_col, cam_col,
                                  count_col = "individuals",
                                  prop_col = "individuals_prop") {
  
  if ("empty" %in% colnames(count_df)) {
    diversity_df <- count_df |>
      group_by(.data[[cam_col]]) |>
      summarise(richness = ifelse(all(empty),
                                  NA, n()),
                shannon = ifelse(all(empty),
                                 NA, -sum(.data[[prop_col]]*log(.data[[prop_col]]))),
                simpson = ifelse(all(empty),
                                 NA, sum(.data[[count_col]]*(.data[[count_col]]-1))/(sum(.data[[count_col]])*(sum(.data[[count_col]])-1))),
                .groups = "drop")
  } else {
  diversity_df <- count_df |>
    group_by(.data[[cam_col]]) |>
    summarise(richness = n(),
              shannon = -sum((.data[[prop_col]]*log(.data[[prop_col]]))),
              simpson = sum(.data[[count_col]]*(.data[[count_col]]-1))/(sum(.data[[count_col]])*(sum(.data[[count_col]])-1)),
              .groups = "drop")
  }
  
  diversity_df <- as.data.frame(diversity_df)
  return(diversity_df)
}


# Circular density distribution -------------------------------------------

#' Time to radians
#'
#' Convert a time of day to circular data.
#' 
#' @param time The time of the day (must be an object of class
#' `times` from the `chron` package)
#' @param circular Return an object of class circular? 
#' @param units The unit to use (subset of values used in in `circular::circular`)
#'
#' @return the times converted to the desired unit. 
#' If `circular` is `TRUE`, returns an object of class `circular`.
#'
#' @export
#'
#' @examples
#' time_to_circular(chron::times(c("00:00:00", "12:00:00", "08:33:44")))
time_to_circular <- function(time, 
                             units = c("radians", "hours"),
                             circular = FALSE) {
  
  if (!("times" %in% class(time))) {
    stop("time must be of class 'times'. You can convert it using chron::times(time)")
  }
  
  if (length(units) > 1) {
    units <- units[1]
  }
  
  if (units == "radians") {
    coef <- 2*pi
  } else if (units == "hours") {
    coef <- 24
  }
  
  timenum <- as.numeric(time)*coef
  
  if (circular) {
    timenum <- circular::circular(timenum, 
                                  units = units)
  }
  
  return(timenum)
}

#' Fit a von Mises distribution
#'
#' @param time The time of the day (must be an object of class
#' `times` from the `chron` package)
#' @param k Number of mixture components (number of modes
#' in the final distribution)
#'
#' @return A mixture model of von Mises distributions of class
#' `movMF`.
#' 
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' recordTableSample <- recordTableSample[recordTableSample$Species == "PBE", ]
#' recordTableSample$Time <- chron::times(recordTableSample$Time)
#' fit_vonMises(recordTableSample$Time, k = 3)
fit_vonMises <- function(time, k) {
  
  if (length(units) > 1) {
    units <- units[1]
  }
  
  circular <- time_to_circular(time, units = "radians")
  
  # Convert angular coordinates to cartesian coordinates 
  # on the unit circle
  x <- cos(circular)
  y <- sin(circular)
  
  mod <- movMF::movMF(cbind(x, y), k)
  
  return(mod)
}

#' von Mises density
#'
#' Get the density of a von Mises mixture model.
#' 
#' @param mod The model
#' @param unit Unit to use in the return dataframe. Can be either `radians`
#' or `hours`.
#' @param x The vector on which to fit. Must be of type `circular`.
#'
#' @return A dataframe with 2 columns:
#' + `density`: value of the density probability function
#' + `x`: corresponding value of the variable 
#' (using the unit specified in `unit`).
#'
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' recordTableSample <- recordTableSample[recordTableSample$Species == "PBE", ]
#' recordTableSample$Time <- chron::times(recordTableSample$Time)
#' mod <- fit_vonMises(recordTableSample$Time, k = 3)
#' dt <- vonMises_density(mod)
#' # Visual check
#' hist(as.numeric(recordTableSample$Time)*2*pi, 
#'      freq = FALSE, breaks = seq(0, 2*pi, by = pi/8))
#' lines(as.numeric(dt$x), dt$density, type = "l")
vonMises_density <- function(mod, 
                             x = circular::circular(seq(0, 2*pi, by = 0.01),
                                                    units = "radians"),
                             unit = c("hour", "radians")) {
  
  # Get model coefficients ---
  modcoef <- coef(mod)
  # Get proportions
  alpha <- modcoef$alpha
  # Get means
  mu <- apply(modcoef$theta, 1, 
              function(x) atan2(y = x[2], x = x[1]))
  # Get kappa
  kappa <- sqrt(rowSums(modcoef$theta^2)) 
  
  # Get density ---
  # Helper function for one density
  vm_density <- function(x, alpha, mu, kappa) {
    alpha*circular::dvonmises(x, 
                              mu = circular::circular(mu, units = "radians"),
                              kappa = kappa)
  }
  
  # Get density for each component of the mixture
  densities <- lapply(1:length(alpha),
                      function(i) vm_density(x, alpha[i], mu[i], kappa[i]))
  densities <- do.call("rbind", densities)
  
  # Get final density
  density <- colSums(densities)
  
  # Format return as a df ---
  if (length(unit) > 1) {
    unit <- unit[1]
  }
  
  if (unit == "hour") {
    coef <- (2*pi)/24
    # Convert time and density
    x <- x * 1/coef
    density <- density * coef
  }
  
  res <- data.frame(density = density,
                    x = x)
  
  return(res)
  
}


# Helpers -----------------------------------------------------------------
#' Reorder values
#'
#' @param vec A named vector to reorder
#' @param names The order of the names
#' @param keep_all_names If some values of `names` are not in
#' the names of `vec`, should they be kept?
#'
#' @noRd
#' @return The reordered vector `vec` with only names in `names`.
#' Some values can be `NA` if `keep_all_names` is `TRUE`.
reorder_named_values <- function(vec, names, 
                                 keep_all_names = TRUE) {
  
  names_vec <- names(vec)
  
  if (!keep_all_names) {
    names_sel <- names[names %in% names_vec]
  }
  else {
    names_sel <- names
  }
  res <- vec[names_sel]
  names(res) <- names_sel
  return(res)
}

