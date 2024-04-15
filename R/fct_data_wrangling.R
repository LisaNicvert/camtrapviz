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
#' @param dfrec the records dataframe: must contain the camera names and some
#' infortmation on the pictures sampling time (date and time or datetime).
#' @param cam_col name of the column containing the camera ID
#' @param datetime_col name of the column containing timestamps for the pictures
#' (optional if `date_col` and `time_col` are provided)
#' @param date_col name of the column containing date (optional if `datetime_col` is provided)
#' @param time_col name of the column containing time (optional if `datetime_col` is provided)
#' @param dfcam the dataframe of cameras deployments (optional).
#' @param cam_col_dfcam name of the column containing camera ID in `dfcam` 
#' If `dfcam` is provided but `cam_col_dfcam` is `NULL`, it will be set to `cam_col`.
#' @param setup_col name of the column containing setup date or datetime 
#' in `dfcam` (optional)
#' @param retrieval_col name of the column containing retrieval date or datetime 
#' in `dfcam` (optional)
#' @param spp_col name of the species column (optional). If present, 
#' the summarry will include the number of species seen on each camera.
#' @param obstype_col name of the observation type column
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
#' + `sampling_length` length of the sampling period in days (computed
#' with the `cameraOperation` function from the `camtrapR` package).
#' If `setup` or `retrieval` are `NA`, then `sampling_length` is `NA`
#' and if `setup` and `retrieval` are the same (e.g. unique picture),
#' `sampling_length` is zero.
#' + `pictures`: the number of pictures taken with this camera (it
#' is zero if the camera is only in `dfcam` but not in `dfrec`).
#' + `species` (present only if `spp_col` is provided): number of species 
#' caught on camera. If `obstype_col` is provided, species
#' marked as `NA` in `spp_col` but which have different values
#' in `obs_type` are counted as different species.
#' + `setup` containing the start of the sampling for each camera.
#' + `retrieval` containing the end of the sampling for each camera.
#' + `setup_origin` containing the method used to determine the
#' start of the sampling (`picture` or `setup`)
#' + `retrieval_origin` containing the method used to determine the
#' end of the sampling (`picture` or `retrieval`)
#' 
#' @export
#' 
#' @seealso [\code{vignette("summarize", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/summarize.html),
#' [summarize_species()]
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
#'                   cam_col = "camera", datetime_col = "stamp", 
#'                   dfcam = cameras, 
#'                   setup_col = "setup", retrieval_col = "retrieval",
#'                   spp_col = "species")
#' # Since camera A had no setup date, the first picture is used.
#' # For camera B, setup and retrieval are taken from dfcam.
#' # For camera C, as it is present only on dfcam and has no retrieval date,
#' # only a setup date is indicated.
summarize_cameras <- function(dfrec, cam_col, 
                              datetime_col = NULL,
                              date_col = NULL, time_col = NULL,
                              spp_col = NULL, obstype_col = NULL,
                              dfcam = NULL, 
                              cam_col_dfcam = ifelse(is.null(dfcam), NULL, cam_col),
                              setup_col = NULL, retrieval_col = NULL) {
  
  # --- Check inputs
  
  # Check datetime or date + time
  check_datetime_cols(dfrec,
                      datetime_col = datetime_col,
                      date_col = date_col,
                      time_col = time_col)
  
  # Check column names
  check_column_names(dfrec, cam_col, ignore_null = FALSE)
  check_column_names(dfrec, spp_col)
  check_column_names(dfrec, obstype_col)
  
  check_column_names(dfcam, cam_col_dfcam, ignore_null = FALSE)
  check_column_names(dfcam, setup_col)
  check_column_names(dfcam, retrieval_col)
  
  # --- Compute first and last picture
  camsum <- dfrec
  
  if (is.null(datetime_col)) {
    # Create timestamp column
    camsum$timestamp <- paste(camsum[[date_col]],
                              camsum[[time_col]])
    camsum$timestamp <- as.POSIXct(camsum$timestamp,
                                   tz = "UTC")
    # Set datetime_col to 'timestamp'
    datetime_col <- "timestamp"
  }
  
  # Summarize with timestamp
  if (!is.null(spp_col)) {
    # We want to add species count to the summary
    
    # Handle NA species
    if (!is.null(obstype_col)) {
      # Replace species with species or type
      camsum[[spp_col]] = get_all_species(camsum, 
                                          spp_col = spp_col,
                                          obstype_col = obstype_col,
                                          return_df = FALSE)
    }
    
    camsum <- camsum |>
      group_by(.data[[cam_col]]) |>
      summarise(pictures = n(),
                species = length(unique(.data[[spp_col]])),
                setup = min(.data[[datetime_col]]),
                retrieval = max(.data[[datetime_col]]),
                setup_origin = "picture",
                retrieval_origin = "picture")
  } else {
    # No species count
    camsum <- camsum |>
      group_by(.data[[cam_col]]) |>
      summarise(pictures = n(),
                setup = min(.data[[datetime_col]]),
                retrieval = max(.data[[datetime_col]]),
                setup_origin = "picture",
                retrieval_origin = "picture")
  }
  
  
  # --- Use dfcam for setup and retrieval
  # If we have additional info from dfcam
  if (!is.null(dfcam)) {
    
    # Add cameras present in dfcam but not in dfrec
    not_in_camsum <- dfcam[[cam_col_dfcam]][!(dfcam[[cam_col_dfcam]] %in% camsum[[cam_col]])]
    
    if (length(not_in_camsum) != 0) {
      if (is.null(spp_col)) {
        dfbind <- data.frame(not_in_camsum, 0, NA, NA, NA, NA)
        names(dfbind) <- c(cam_col, "pictures", "setup", "retrieval", "setup_origin", "retrieval_origin")
      } else {
        dfbind <- data.frame(not_in_camsum, 0, NA, NA, NA, NA, NA)
        names(dfbind) <- c(cam_col, "pictures", "species", "setup", "retrieval", "setup_origin", "retrieval_origin")
      }
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
    # tibble to dfrec
    camsum_sampling_time <- as.data.frame(camsum_sampling_time)
    
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
    camsum <- camsum |> 
      mutate(sampling_length = unname(sampling_length), 
             .before = setup)
    # camsum$sampling_length <- unname(sampling_length)
  } else {
    camsum <- camsum |> 
      mutate(sampling_length = NA, 
             .before = setup)
    # camsum$sampling_length <- NA
  }
  
  # Set sampling length to zero where setup = retrieval
  camsum <- camsum |> 
    mutate(sampling_length = ifelse(setup == retrieval, 0, sampling_length))
  
  # Tibble to dfrec
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
#' @param obstype_col name of the observation type column from the dataframe
#' @param animal_code value of `obstype_col` coding for animal observations.
#' @param return_df return a dataframe? If `TRUE`, will return a dataframe 
#' (see below); else will return a character vector of species names.
#'
#' @return 
#' species names in the same order as `df`. 
#' If `obstype_col` is provided,  `NA` values in `spp_col` are 
#' replaced with the corresponding value in `obstype_col` 
#' (if `obstype_col` is not `animal_code`). 
#' If `return_df` is `TRUE`, returns a dataframe containing 
#' species and observation type in the same order as `df`.
#' This dataframe has the following columns (type character):
#' + a column named like `spp_col` containing species names
#' (where `NA` values in `spp_col` are replaced as described above).
#' + if `obstype_col` is provided: a column named like `obstype_col` 
#' containing corresponding observations types.
#' + if `obstype_col` is provided: a column named like `spp_col` 
#' with a suffix `_orig` which indicates the original value of 
#' `spp_col` (before it was maybe replaced with `NA`).
#' Else, returns only the character vector containing the values of 
#' `spp_col`.
#' 
#' 
#' @seealso [get_unique_species()]
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, 
#'                              "cameratrapper", "tourist"),
#'                  type = c("animal", "animal", "animal", "fire", "blank", 
#'                           "human", "human"))
#' # Use the type column
#' get_all_species(df, spp_col = "species", obstype_col = "type")
#' # Use the type column but return a vector
#' get_all_species(df, spp_col = "species", return_df = FALSE)
#' 
#' # Don't use the type column
#' get_all_species(df, spp_col = "species")
get_all_species <- function(df,
                            spp_col, obstype_col = NULL,
                            animal_code = "animal",
                            return_df = ifelse(is.null(obstype_col), FALSE, TRUE)) {
  # Check columns ---
  check_column_names(df, spp_col, ignore_null = FALSE)
  check_column_names(df, obstype_col)
  
  # Convert factor to character
  df[[spp_col]] <- as.character(df[[spp_col]])
  
  if (!is.null(obstype_col)) {
    df[[obstype_col]] <- as.character(df[[obstype_col]])
  }
  
  if (!is.null(obstype_col)) {
    # Get columns
    spp_df <- df[, c(spp_col, obstype_col)]
    
    # Store original value
    spp_col_orig <- spp_df[[spp_col]]
    
    # Replace values with not animal ---
    is_non_animal <- spp_df[[obstype_col]] != animal_code # Get non-animals
    is_non_animal[is.na(is_non_animal)] <- TRUE # Consider NAs obstype as non-animals
    spp_df[[spp_col]][is_non_animal & is.na(spp_df[[spp_col]])] <- spp_df[[obstype_col]][is_non_animal & is.na(spp_df[[spp_col]])]
    
    # Add original value
    spp_df[[paste0(spp_col, "_orig")]] <- spp_col_orig
    
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
#' @param obstype_col name of the observation type column from the dataframe
#' @param animal_code value of `obstype_col` coding for animal observations.
#' @param return_df return a dataframe? If `TRUE`, will return a dataframe 
#' (see below); else will return a character vector of unique 
#' species names.
#' @param reorder Reorder the results? This will arrange values by 
#' alphabetical order. If `obstype_col` is provided, non-animal species will
#' be arranged last.
#' @param add_ID Add an ID column?
#'
#' @return 
#' unique species names. 
#' If `obstype_col` is provided,  `NA` values in `spp_col` are 
#' replaced with the corresponding value in `obstype_col` 
#' (if `obstype_col` is not `animal_code`). 
#' If `return_df` is `TRUE`, returns a dataframe containing 
#' unique species and observation type.
#' This dataframe has the following columns (type character):
#' + If `add_ID` is `TRUE`: a column `ID` to uniquely identify each 
#' species/observation combination (IDs are numbers). 
#' `type` is the observation type value. Else, IDs are of the form `spp`.
#' + a column named like `spp_col` containing species names
#' (where `NA` values in `spp_col` are replaced as described above).
#' + if `obstype_col` is provided: a column named like `obstype_col` 
#' containing corresponding observations types.
#' + if `obstype_col` is provided: a column named like `spp_col` 
#' with a suffix `_orig` which indicates the original value of 
#' `spp_col` (before it was maybe replaced with `NA`).
#' If `return_df` is `FALSE`, returns only the unique values of 
#' `spp_col`.
#' @export
#' 
#' @seealso [get_all_species()]
#'
#' @examples
#' df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, 
#'                              "cameratrapper", "tourist"),
#'                  type = c("animal", "animal", "animal", "fire", "blank", 
#'                           "human", "human"))
#' # Use the type column
#' get_unique_species(df, spp_col = "species", obstype_col = "type",
#'                    reorder = TRUE)
#' # Use the type column but return a vector
#' get_unique_species(df, spp_col = "species", return_df = FALSE,
#'                    reorder = TRUE)
#' 
#' # Don't use the type column
#' get_unique_species(df, spp_col = "species",
#'                    reorder = TRUE)
get_unique_species <- function(df,
                        spp_col, obstype_col = NULL,
                        animal_code = "animal",
                        return_df = ifelse(is.null(obstype_col), FALSE, TRUE),
                        reorder = FALSE,
                        add_ID = FALSE) {
  
  # Don't check column names because it is done in get_all_species

  # Get a dataframe of non-unique species names
  spp_df_all <- get_all_species(df = df,
                                spp_col = spp_col, 
                                obstype_col = obstype_col,
                                animal_code = animal_code,
                                return_df = TRUE)
  
  if (!is.null(obstype_col)) {
    # Get (unique) species ---
    spp_df <- spp_df_all |>
      distinct(.keep_all = TRUE)
    
    # Arrange with non-animal last ---
    if (reorder) {
      # Get factor levels
      levels <- sort(unique(spp_df[[obstype_col]], na.last = TRUE), na.last = TRUE)
      levels <- c(animal_code, levels[levels != animal_code])
      
      spp_df[[obstype_col]] <- factor(spp_df[[obstype_col]], 
                                  levels = levels)
      spp_df <- spp_df |> dplyr::arrange(.data[[obstype_col]],
                                         .data[[spp_col]])
      # Convert back to character
      spp_df[[obstype_col]] <- as.character(spp_df[[obstype_col]])
    }
  } else { # obstype_col not provided
    spp_df <- spp_df_all[spp_col] |>
      distinct(.keep_all = TRUE)
    if (reorder) {
      spp_df <- spp_df |> dplyr::arrange(.data[[spp_col]])
    }
  }
  
  if (return_df) {
    # Add ID
    if (add_ID) {
      spp_df <- spp_df |> 
        mutate(ID = paste("ID", 1:nrow(spp_df), sep = "_"),
               .before = 1)
    }
    res <- as.data.frame(spp_df) # Convert tibble to df
  } else {
    res <- spp_df[[spp_col]]
  }
  
  return(res)
}


#' Get species count from a dataframe
#'
#' @param df a dataframe
#' @param spp_col name of the species column from the dataframe
#' @param obstype_col name of the observation type column from the dataframe
#' @param keep_NA count NAs in the species length?
#'
#' @return The number of species. If `obstype_col` is provided, will
#' ignore all species with `obstype_col` value different from `animal`.
#' If `keep_NA`, will count NA in the total species count.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(obstype = c("animal", "animal", "animal", "animal", "blank"),
#'                  species = c("cat", "cat", "cow", "dog", NA))
#' get_nspecies(df, spp_col = "species", obstype_col = "obstype")
get_nspecies <- function(df, spp_col, obstype_col = NULL,
                         keep_NA = FALSE) {
  
  # Check column names ---
  check_column_names(df, spp_col, ignore_null = FALSE)
  check_column_names(df, obstype_col)
  
  if (!is.null(obstype_col)) {
    # Filter to get only animal species
    species <- df[[spp_col]][df[[obstype_col]] == "animal"]
  } else {
    species <- df[[spp_col]]
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
#' @param obstype_col Name of the observation type column (optional)
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
#' @param dfcam Dataframe containing information about the cameras
#' sampling length. If it is provided, then `cam_col_dfcam` and
#' `duration_col_dfcam` must be in its column names.
#' @param cam_col_dfcam Column name containing cameras names in `dfcam`
#' @param duration_col_dfcam Column name containing sampling duration
#' in `dfcam`
#'
#' @return A table summarizing species information with the following columns:
#' + Species (named like `spp_col`): species identity 
#' (same as the `spp_col` input column)
#' + Observation type (present only if `obstype_col` is not `NULL` and named like 
#' `obstype_col`): observation type (same as the `obstype_col` input column)
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
#' If `by_cam` is `TRUE` and `dfcam` is provided, 
#' the following rows are also present:
#' + `sightings_RAI`: relative abundance index for species' sightings 
#' at each camera. It is computed as the number of sightings over the sampling duration
#' (it represents the number of sightings per time unit).
#' + `individuals_RAI`: the same as `sightings_RAI`, but computed as the 
#' number of individuals over the sampling duration.
#' + Sampling duration (named like `duration_col_dfcam`): sampling
#' duration for each camera
#' 
#' Finally, if `keep_all_camera_levels` is `TRUE`, a final column named
#' `empty` is added to indicate which cameras were empty (have no data).
#' 
#' @export
#'
#' @seealso [\code{vignette("summarize", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/summarize.html),
#' [summarize_cameras()]
#' 
#' @examples
#' df <- data.frame(species = c("zebra", "cat", "cat", "cow", NA, NA),
#'                  type = c("animal", "animal", "animal", "animal", "human", "blank"),
#'                  camera = c("C1", "C1", "C2", "C3", "C3", "C4"),
#'                  count = c(1, 1, 3, 50, 1, NA))
#' # Summarize species across all cameras
#' summarize_species(df, 
#'                   spp_col = "species", cam_col = "camera",
#'                   obstype_col = "type",
#'                   count_col = "count",
#'                   NA_count_placeholder = 1)
#' # Summarize per species and cameras
#' summarize_species(df, 
#'                   spp_col = "species", cam_col = "camera",
#'                   obstype_col = "type",
#'                   count_col = "count",
#'                   by_cam = TRUE,
#'                   NA_count_placeholder = 1)
#' # Add camera sampling length to get the RAI
#' cam_sampling <- data.frame(camera = c("C1", "C2", "C3", "C4"),
#'                            sampling_duration = c(100, 1, 10, 10))
#' summarize_species(df, 
#'                   spp_col = "species", cam_col = "camera",
#'                   obstype_col = "type",
#'                   count_col = "count",
#'                   by_cam = TRUE,
#'                   dfcam = cam_sampling,
#'                   duration_col_dfcam = "sampling_duration",
#'                   NA_count_placeholder = 1)
summarize_species <- function(df, 
                              spp_col, 
                              cam_col = NULL,
                              obstype_col = NULL,
                              count_col = NULL,
                              ncam = NULL,
                              by_cam = FALSE,
                              keep_all_camera_levels = FALSE,
                              dfcam = NULL,
                              cam_col_dfcam = ifelse(is.null(dfcam), NULL, cam_col),
                              duration_col_dfcam = ifelse(is.null(dfcam), NULL, "sampling_length"),
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
  
  # Check column names
  check_column_names(df, spp_col, 
                     ignore_null = FALSE)
  if (by_cam) {
    check_column_names(df, cam_col,
                       ignore_null = FALSE)
  }
  
  check_column_names(df, obstype_col)
  check_column_names(df, count_col)
  
  check_column_names(dfcam, cam_col_dfcam)
  check_column_names(dfcam, duration_col_dfcam)
  
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
  if (!is.null(obstype_col)) { # obstype_col provided
    if (by_cam) {
      # Group by camera
      res <- df |> 
        group_by(.data[[spp_col]], .data[[obstype_col]],
                 .data[[cam_col]])
    } else {
      # Don't group by camera
      res <- df |> 
        group_by(.data[[spp_col]], .data[[obstype_col]])
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
  
  if (by_cam & !is.null(dfcam)) { # Add RAI
    # Get only interesting columns from dfcam
    cam_duration <- dfcam |>
      dplyr::select(.data[[cam_col_dfcam]],
                    .data[[duration_col_dfcam]])
    
    # Get the correspondence between camera column in records and in cameras
    by <- cam_col_dfcam
    names(by) <- cam_col
    
    # Merge cameras table and duration
    res <- res |>
      dplyr::left_join(cam_duration,
                       by = by)
    
    # Compute RAI
    res <- res |>
      mutate(across(c("sightings", "individuals"), 
                    ~ .x/.data[[duration_col_dfcam]],
                    .names = "{.col}_RAI"),
             .before = duration_col_dfcam)
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
    if (is.null(obstype_col)) {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       na.last = TRUE)
    } else {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       .data[[obstype_col]],
                       na.last = TRUE)
    }
  } else {
    if (is.null(obstype_col)) {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       .data[[cam_col]],
                       na.last = TRUE)
    } else {
      res <- res |> 
        dplyr::arrange(.data[[spp_col]],
                       .data[[obstype_col]],
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
#' Filter camera trap data observations and
#' cameras metadata based on species, cameras and dates.
#' Values will be filtered out by default, but if `filter_out`
#' is `FALSE` they will be kept instead.
#'
#' @param dat The data to filter. It can be either a list with one component `$data`
#' or a `datapackage` object (inheriting list). Either way, the data 
#' are in the `$data` slot with two components: 
#' + `$deployments` (cameras table)
#' + `$observations` (records table)
#' @param spp_filter Species to filter from the data
#' @param spp_col Name of the species column (required if `spp_filter` is not `NULL`)
#' @param obstype_filter Observation types to filter from the data
#' @param obstype_col Name of the observation column  (required if `obstype_filter` is not `NULL`)
#' @param cam_filter Cameras to filter from the data
#' @param cam_col_rec Name of the cameras column in records table (`dat$data$observations`).
#' Required if `cam_filter` is not `NULL`.
#' @param cam_col_cam Name of the cameras column in cameras table (`dat$data$deployments`).
#' Defaults to the same value as `cam_col_rec`, and is required if `cam_filter` is not `NULL`.
#' @param daterange Date range to filter on for the data (will filter 
#' observations' times so that `times >= daterange[1]` and 
#' `times <= daterange[2]`). Can be either a Date or a POSIX.
#' @param datetime_col Name of the datetime column (must be coercible to POSIX).
#' It is not needed if `date_col` and `time_col` are provided, but else
#' it is required if `daterange` is not `NULL`.
#' @param date_col Name of the date column. It is not needed if `datetime_col` 
#' is provided, but else it is required if `daterange` is not `NULL`.
#' @param time_col Name of the time column. It is not needed if `datetime_col` 
#' is provided, but else it is required if `daterange` is not `NULL`.
#' @param custom_filter values to filter out in the custom column `custom_col`.
#' @param custom_col name of a custom column in to filter values in 
#' `dat$data$observations` (required if `custom_filter` is not `NULL`).
#' @param cam_as_factor Transform cameras as factors?
#' @param tz Timezone for the data bounds. If not provided, will try to 
#' find the timezone in `daterange` (if it is a POSIX), then in
#' `datetime_col` (if provided), and finally if no timezone is 
#' present it will default to UTC (Etc/GMT).
#' For the filtering step, if needed datetimes in `datetime_col` can be 
#' converted to `tz` but the output data will not be affected.
#' @param filter_out Filter out (`TRUE`) or keep values?
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
#' @seealso [\code{vignette("filter-data", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/filter-data.html),
#' [filter_cameras_in_both_tables()]
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
#'             datetime_col = "DateTimeOriginal")
filter_data <- function(dat,
                        spp_filter = NULL,
                        spp_col = NULL,
                        obstype_filter = NULL,
                        obstype_col = NULL,
                        cam_filter = NULL,
                        cam_col_rec = NULL,
                        cam_col_cam = cam_col_rec,
                        daterange = NULL,
                        datetime_col = NULL,
                        time_col = NULL,
                        date_col = NULL,
                        custom_filter = NULL,
                        custom_col = NULL,
                        tz = NULL,
                        cam_as_factor = FALSE,
                        filter_out = TRUE
                        ) {
  
  rec <- dat$data$observations
  
  res <- dat
  
  # Filter species  ---
  if (!is.null(spp_filter)) {
    
    check_column_names(dat$data$observations, spp_col, 
                       ignore_null = FALSE,
                       dfname = "dat$data$observations")
    
    if (filter_out) {
      # User wants to filter out species
      res$data$observations <- res$data$observations |>
        dplyr::filter(!(.data[[spp_col]] %in% spp_filter))
    } else {
      # User wants to keep species
      res$data$observations <- res$data$observations |>
        dplyr::filter(.data[[spp_col]] %in% spp_filter)
    }
  }
  
  # Filter observations ---
  if (!is.null(obstype_filter)) {
    
    check_column_names(dat$data$observations, obstype_col, 
                       ignore_null = FALSE,
                       dfname = "dat$data$observations")
    
    if (filter_out) {
      # User wants to filter out observations
      res$data$observations <- res$data$observations |>
        dplyr::filter(!(.data[[obstype_col]] %in% obstype_filter))
    } else {
      # User wants to keep observations
      res$data$observations <- res$data$observations |>
        dplyr::filter(.data[[obstype_col]] %in% obstype_filter)
    }
  } 
  
  
  # Filter cameras ---
  if (!is.null(cam_filter)) {
    
    check_column_names(dat$data$observations, cam_col_rec, 
                       ignore_null = FALSE,
                       dfname = "dat$data$observations")
    check_column_names(dat$data$deployments, cam_col_cam, 
                       ignore_null = FALSE,
                       dfname = "dat$data$deployments")
    
    if (filter_out) {
      # User wants to filter out cameras
      res$data$observations <- res$data$observations |>
        dplyr::filter(!(.data[[cam_col_rec]] %in% cam_filter))
      res$data$deployments <- res$data$deployments |>
        dplyr::filter(!(.data[[cam_col_cam]] %in% cam_filter))
    } else {
      # User wants to keep cameras
      res$data$observations <- res$data$observations |>
        dplyr::filter(.data[[cam_col_rec]] %in% cam_filter)
      res$data$deployments <- res$data$deployments |>
        dplyr::filter(.data[[cam_col_cam]] %in% cam_filter)
    }
    
  }
  
  
  # Filter daterange ---
  if (!is.null(daterange)) {
    
    check_datetime_cols(dat$data$observations,
                        datetime_col = datetime_col,
                        date_col = date_col,
                        time_col = time_col, 
                        dfname = "dat$data$observations")
    
    # Set the timezone ---
    default_tz <- "Etc/GMT"
    if (!is.null(datetime_col)) {
      data_tz <- attr(res$data$observations[[datetime_col]], "tzone")
    } else {
      data_tz <- NULL
    }
    tz <- get_tz(custom_tz = tz, 
                 data_tz = data_tz, 
                 default_tz = "Etc/GMT")
    
    if (is.null(datetime_col)) { # no datetime_col column
      # Create a composite timestamp with custom timezone
      timestamp_vec <- paste(as.character(res$data$observations[[date_col]]), 
                             as.character(res$data$observations[[time_col]]))
      
      timestamp_vec <- as.POSIXct(timestamp_vec,
                                  tz = tz)
    } else { # datetime_col not NULL
      timestamp_vec <- add_tz(res$data$observations[[datetime_col]],
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
  if (!is.null(custom_filter)) {
    
    check_column_names(dat$data$observations, custom_col,
                       ignore_null = FALSE,
                       dfname = "dat$data$observations")
    
    if (filter_out) {
      # User wants to filter out a custom column
      res$data$observations <- res$data$observations |>
        dplyr::filter(!(.data[[custom_col]] %in% custom_filter))
    } else {
      # User wants to keep a custom column
      res$data$observations <- res$data$observations |>
        dplyr::filter(.data[[custom_col]] %in% custom_filter)
    }
  }
  
  # Cameras to factor ---
  if (cam_as_factor) {
    cam_vec <- get_cameras(res$data$observations[[cam_col_rec]],
                                res$data$deployments[[cam_col_cam]])
    
    res$data$observations[[cam_col_rec]] <- factor(res$data$observations[[cam_col_rec]],
                                                   levels = cam_vec)
    res$data$deployments[[cam_col_cam]] <- factor(res$data$deployments[[cam_col_cam]],
                                                  levels = cam_vec)
  }
  
  return(res)
}


# Diversity ---------------------------------------------------------------

#' Get diversity indices
#' 
#' From a summary table of camera/species, return richness, Shannon
#' and Simpson diversity indices.
#'
#' @param dfcount Dataframe summarizing counts per species. Can be 
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
#' See [\code{vignette("diversity", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/diversity.html)
#' for details on the formulas of the diversity indices.
#' 
#' @export
#' 
#' @seealso [\code{vignette("diversity", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/diversity.html),
#' [summarize_species()]
#' @examples
#' countdf <- data.frame(camera = c("C1", "C1", "C1", "C2",
#'                                  "C3", "C3", "C3"),
#'                       species = c("cat", "cow", "rabbit",
#'                                   "cat", "cat", "cow", "rabbit"),
#'                       individuals = c(30, 30, 30, 30, 88, 1, 1),
#'                       individuals_prop = c(1/3, 1/3, 1/3, 1, 88/90, 1/90, 1/90))
#' get_diversity_indices(countdf, 
#'                       spp_col = "species", cam_col = "camera")
get_diversity_indices <- function(dfcount, spp_col, cam_col,
                                  count_col = "individuals",
                                  prop_col = "individuals_prop") {
  
  # Check column names ---
  check_column_names(dfcount, spp_col, ignore_null = FALSE)
  check_column_names(dfcount, cam_col, ignore_null = FALSE)
  check_column_names(dfcount, count_col, ignore_null = FALSE)
  check_column_names(dfcount, prop_col, ignore_null = FALSE)
  
  if ("empty" %in% colnames(dfcount)) {
    diversity_df <- dfcount |>
      group_by(.data[[cam_col]]) |>
      summarise(richness = ifelse(all(empty),
                                  NA, n()),
                shannon = ifelse(all(empty),
                                 NA, -sum(.data[[prop_col]]*log(.data[[prop_col]]))),
                simpson = ifelse(all(empty),
                                 NA, sum(.data[[count_col]]*(.data[[count_col]]-1))/(sum(.data[[count_col]])*(sum(.data[[count_col]])-1))),
                .groups = "drop")
  } else {
  diversity_df <- dfcount |>
    group_by(.data[[cam_col]]) |>
    summarise(richness = n(),
              shannon = -sum((.data[[prop_col]]*log(.data[[prop_col]]))),
              simpson = sum(.data[[count_col]]*(.data[[count_col]]-1))/(sum(.data[[count_col]])*(sum(.data[[count_col]])-1)),
              .groups = "drop")
  }
  
  diversity_df <- as.data.frame(diversity_df)
  return(diversity_df)
}


# Circular density distribution (not used anymore) -------------------------------------------

#' Time to radians
#'
#' Convert a time of day to circular data.
#' 
#' @param time The time of the day (must be an object of class
#' `times` from the `chron` package)
#' @param circular Return an object of class circular
#' @param units The unit to use (subset of values used in in `circular::circular`)
#'
#' @return the times converted to the desired unit. 
#' If `circular` is `TRUE`, returns an object of class `circular`.
#'
#' @noRd
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
#' @noRd
#'
#' @seealso [\code{vignette("activity-patterns", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/activity-patterns.html)
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

#' Check column names
#' 
#' Checks that a character is in the column names of a df
#' and else returns an error.
#'
#' @param df The dataframe. If `NULL`, the function does nothing.
#' @param col Character (column name to check).
#' @param dfname Name of the dataframe argument to display in the error message
#' @param colname Name of the column argument to display in the error message
#' @param ignore_null In case `col` is NULL, should the function still 
#' raise an error?
#'
#' @return Either an error or nothing.
#' @noRd
check_column_names <- function(df, col,
                               dfname = substitute(df), 
                               colname = substitute(col),
                               ignore_null = TRUE) {
  
  if (!is.null(df)) {
    # Initialize stopping
    stopping <- FALSE
    
    if (is.null(col) ) {
      # Handle special case where col is NULL
      
      if (!ignore_null) { # If an error should be raised even if col is NULL
        stopping <- TRUE
        colval <- "NULL"
      }
    } else if (!(col %in% colnames(df))) {
      # Check if col in colnames
      stopping <- TRUE
      colval <- col
    }
    
    if (stopping) {
      # Raise error
      stop(colname,
           " (value: ", colval, ")",
           " is not in the column names of ", dfname)
    }
  }
  
}


check_datetime_cols <- function(df, 
                                datetime_col,
                                date_col, time_col,
                                dfname = substitute(df), 
                                datetime_colname = substitute(datetime_col),
                                date_colname = substitute(date_col),
                                time_colname = substitute(time_col)) {
  if (!is.null(datetime_col)) { 
    # If datetime not NULL, use datetime
    if (!is.null(date_col) || !is.null(time_col)) {
      # If date and time are provided they will be ignored
      message("datetime_col is provided, so date_col and time_col will be ignored.")
    }
    check_column_names(df, datetime_col,
                       dfname = dfname, colname = datetime_colname)
  } else {
    # If datetime NULL, check date + time
    if (is.null(date_col) || is.null(time_col)) {
      # If either one is NULL, stop
      stop("If datetime_col is not specified or NULL, both date_col and time_col must be provided.")
    } else {
      # If both are not NULL, check their names
      check_column_names(df, date_col, 
                         dfname = dfname, colname = date_colname)
      check_column_names(df, time_col,
                         dfname = dfname, colname = time_colname)
    }
  }
}
