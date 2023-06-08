# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-25
#
# Script Description: functions to handle camera trap data


# Cameras -----------------------------------------------------------------

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

#' Get unique species from a dataframe
#'
#' @param df The dataframe
#' @param spp_col name of the species column from the dataframe
#' @param obs_col name of the observation type column from the dataframe
#' @param animal_code value of `obs_col` coding for animal observations.
#' @param return_df renturn a dataframe? If `TRUE`, will return a dataframe 
#' (see below); else will return a character vector of species names.
#'
#' @return If `return_df` is `TRUE`, returns a dataframe containing 
#' unique species and their characteristics. Else, returns only the character
#' vector containing to the column `spp_col` of this dataframe.
#' + a column named like `spp_col` containing unique species names.
#' + if `obs_col` is not `NULL`: a column named like `obs_col` containing corresponding
#' observation type
#' The rownames of this table are of the form ID_number and are a unique ID for
#' the species.
#' 
#' @details
#' When `obs_col` is provided, the values of the `spp_col` column that are 
#' `NA` when `obs_col` corresponds to a non-animal species (which is the 
#' case in the `camtrapDP` standard) are replaced with the value from 
#' `obs_col`.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, 
#'                              "cameratrapper", "tourist"),
#'                  type = c("animal", "animal", "animal", "fire", "blank", 
#'                           "human", "human"))
#' get_species(df, spp_col = "species", obs_col = "type")
get_species <- function(df,
                        spp_col, obs_col = NULL,
                        animal_code = "animal",
                        return_df = TRUE) {
  
  if (!is.null(obs_col)) {
    # Get (unique) species ---
    spp_df <- df[c(obs_col, spp_col)] |>
      distinct(.keep_all = TRUE)
    
    # Replace values with not animal ---
    is_non_animal <- spp_df[[obs_col]] != animal_code # Get non-animals
    is_non_animal[is.na(is_non_animal)] <- TRUE # Consider NAs as non-animals
    spp_df[[spp_col]][is_non_animal & is.na(spp_df[[spp_col]])] <- spp_df[[obs_col]][is_non_animal & is.na(spp_df[[spp_col]])]
    
    # Arrange with non-animal last ---
    # Get factor levels
    levels <- sort(unique(spp_df[[obs_col]], na.last = TRUE), na.last = TRUE)
    levels <- c(animal_code, levels[levels != animal_code])
    
    spp_df[[obs_col]] <- factor(spp_df[[obs_col]], 
                                levels = levels)
    spp_df <- spp_df |> dplyr::arrange(.data[[obs_col]],
                                       .data[[spp_col]])
    # Convert back to character
    spp_df[[obs_col]] <- as.character(spp_df[[obs_col]])
    
    # Reorder columns
    spp_df <- spp_df[, c(spp_col, obs_col)]
  } else {
    spp_df <- df[spp_col] |>
      distinct(.keep_all = TRUE)
    spp_df <- spp_df |> dplyr::arrange(.data[[spp_col]])
  }
  
  if (return_df) {
    # Add ID
    res <- as.data.frame(spp_df)

    rownames(res) <- paste("ID", 1:nrow(res), sep = "_")
  } else {
    res <- spp_df[[spp_col]]
  }
  
  return(res)
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

#' Summarize species
#'
#' Summarize species information from a data table
#'
#' @param df the observation dataframe to summarize
#' @param species_col Name of the species column
#' @param obs_col Name of the observation type column (optional)
#' @param count_col Name of the column containing species count (optional)
#' @param cam_col Name of the column containing camera codes (optional)
#' @param ncam Number of cameras to take into account when computing
#' the proportion of cameras the species was ween on. If `NULL`, 
#' defaults to the number of cameras present in the `df`.
#' @param NA_count_placeholder Value with which to replace NAs present
#' in the column containing counts. Defaults to NA (i.e. values are not
#' replaced)
#'
#' @return A table summarizing species information with the following columns:
#' + Species (named like `species_col`): species identity 
#' (same as the `species_col` input column)
#' + Observation type (present only if `obs_col` is not `NULL` and named like 
#' `obs_col`): observation type (same as the `obs_col` input column)
#' + `n_sightings`: number of rows where the species was photographed.
#' + `n_individuals`: count of individuals observed on all
#' pictures (using the input `count_col` column).
#' If `count_col` is `NULL`, it contains the same values as `n_sightings`.
#' If there are NAs in the input  `count_col`, they will propagate in `n_individuals`
#' (unless a value is specified in `NA_count_placeholder`).
#' + `n_cameras` (present only if `cam_col` is not `NULL`) : the number of cameras the species was seen on.
#' + `prop_cam` (present only if `cam_col` is not `NULL`): the proportion of cameras the species was seen on.
#' If `ncam` is provided, then it uses `ncam` as the total number of cameras.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("zebra", "cat", "cat", "cow", NA, NA),
#'                  type = c("animal", "animal", "animal", "animal", "human", "blank"),
#'                  camera = c("C1", "C1", "C2", "C3", "C3", "C4"),
#'                  count = c(1, 1, 3, 50, 1, NA))
#' res <- summarize_species(df, 
#'                          species_col = "species", cam_col = "camera",
#'                          obs_col = "type",
#'                          count_col = "count",
#'                          NA_count_placeholder = 1)
summarize_species <- function(df, 
                              species_col, 
                              cam_col = NULL,
                              obs_col = NULL,
                              count_col = NULL,
                              ncam = NULL,
                              NA_count_placeholder = NA) {
  
  # Set and check ncam ---
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
    res <- df |> 
      group_by(.data[[species_col]], .data[[obs_col]])
  } else {
    res <- df |> 
      group_by(.data[[species_col]])
  }
  
  # Summarize ---
  if (!is.null(cam_col)) {
    res <- res |> 
      summarise(n_sightings = n(),
                n_individuals = ifelse(is.null(count_col), 
                                       n(), # Simply count rows
                                       sum(.data[[count_col]]) # else sum count col
                ),
                n_cameras = length(unique(.data[[cam_col]]))
      )
    # Add prop_cam
    res$prop_cam <- res$n_cameras/ncam
  } else {
    res <- res |> 
      summarise(n_sightings = n(),
                n_individuals = ifelse(is.null(count_col), 
                                       n(), # Simply count rows
                                       sum(.data[[count_col]]) # else sum count col
                )
      )
  }
  
  
  # Convert tibble to dataframe
  res <- as.data.frame(res)
  
  return(res)
  
}


# Diversity ---------------------------------------------------------------

#' Get diversity table
#' 
#' Compute a table with various diversity indices from a species
#' occurrence dataframe.
#'
#' @param df The dataframe
#' @param cam_col Name of the column containing cameras names
#' @param spp_col Name of the column containing species names
#' @param keep_all_levels Should we keep all camera levels even 
#' those that have no rows in `df`? (Checked only if `cam_col` is a factor)
#' @param count_col Name of the column containing species counts (optional)
#'
#' @return A dataframe with one row per camera-species combination.
#' For each camera-species, we have the count (column `count`) 
#' and proportion (column `prop`) of individuals of each species seen at this camera.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", NA, NA),
#'                  type = c("animal", "animal", "animal", "animal", "animal", "human", "blank"),
#'                  camera = c("C1", "C1", "C1", "C1", "C2", "C2", "C3"),
#'                  count = c(1, 1, 1, 50, 3, 1, NA))
#' get_diversity_table(df,
#'                     cam_col = "camera",
#'                     spp_col = "species",
#'                     count_col = "count")
get_diversity_table <- function(df, cam_col, spp_col, 
                                count_col = NULL, 
                                keep_all_levels = TRUE) {
  
  # One row per camera and species
  res <- df |>
    group_by(.data[[cam_col]], 
             .data[[spp_col]]) |>
    summarise(count = ifelse(is.null(count_col),
                             n(), sum(.data[[count_col]])),
              .groups = "drop") |> 
    group_by(.data[[cam_col]]) |> 
    mutate(prop = count/sum(count)) |>
    dplyr::ungroup()
  
  # Add unused levels
  if(is.factor(df[[cam_col]]) & keep_all_levels) {
    # Create df with missing factors
    cam_levels <- levels(df[[cam_col]])
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
    # Reorder
    res <- res |> 
      dplyr::arrange(.data[[cam_col]])
  }

  res <- as.data.frame(res)
  return(res)
  
}

#' Get diversity indices
#' 
#' From a summary table of camera/species, return richness, Shannon
#' and Simpson diversity indices.
#'
#' @param count_df Dataframe summarizing counts per species. Can be 
#' computed using the records dataframe with the `get_diversity_table`
#' function.
#' @param spp_col Name of the column containing species names
#' @param cam_col Name of the column containing cameras names
#' @param count_col Name of the column containing species counts
#' @param prop_col Name of the column containing species proportions
#'
#' @return A dataframe with one row per camera summarizing diversity indices.
#' 
#' @details
#' Richness is computed as the number of the different species seen at the
#' camera.
#' 
#' Shannon index is computed as: 
#' \deqn{-\sum p_i ln(p_i)}
#' Where p_i represents the proportion of the abundance of species i at
#' a camera over the the total number of individuals of all species 
#' seen at this camera.
#' It ranges between 0 and +infinity, zero indicating the 
#' lowest diversity.
#' 
#' Simpson index is computed as: 
#' \deqn{(\sum n_i (n_i - 1))/(N (N-1))}
#' Where n_i represents the abundance of species i at
#' a camera over the total number of individuals of all species 
#' seen at this camera.
#' It ranges between 0 and 1, one indicating the lowest diversity.
#' 
#' @export
#'
#' @examples
#' countdf <- data.frame(camera = c("C1", "C1", "C1", "C2",
#'                                  "C3", "C3", "C3"),
#'                       species = c("cat", "cow", "rabbit",
#'                                   "cat", "cat", "cow", "rabbit"),
#'                       count = c(30, 30, 30, 30, 88, 1, 1),
#'                       prop = c(1/3, 1/3, 1/3, 1, 88/90, 1/90, 1/90))
#'get_diversity_indices(countdf, 
#'                      spp_col = "species", cam_col = "camera")
get_diversity_indices <- function(count_df, spp_col, cam_col,
                                  count_col = "count",
                                  prop_col = "prop") {
  
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

#' Fit a von Mises distribution.
#'
#' @param time The time of the day (must be an object of class
#' `times` from the `chron` package)
#' @param k Number of mixxture components (number of modes
#' in the final ditribution)
#'
#' @return A mixture moden of von Mises distributions of class
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

