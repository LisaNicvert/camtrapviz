# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: functions to format data


# Cast --------------------------------------------------------------------

#' Cast columns
#' 
#' Converts columns to a given format.
#'
#' @param df A dataframe containing the columns specified in 
#' the names of `cast_type`.
#' @param cast_type A named list containing the name of the 
#' function to cast between types.
#' The list's names are the names of the columns to cast in `df`.
#' Elements of this list can be:
#' + a character giving a valid function name to call
#' + a list with the first element being the function to call (character)
#' and additional arguments to the function call (that can be named as 
#' the names of the functions' arguments).
#'
#' @return the original dataframe with the specified columns
#' casted with the type indicated in `cast_type`.
#' 
#' @export
#' 
#' @examples
#' df <- data.frame(num = 1:10,
#'                  char = letters[1:10],
#'                  date = rep("12/24/2020", 10))
#' cast <- list(num = "as.character",
#'              char = "as.factor",
#'              date = list("as.Date", 
#'                          format = "%m/%d/%Y"))
#' dfcast <- cast_columns(df, cast)
cast_columns <- function(df, cast_type) {
  
  # Initialize res
  res <- df
  for (i in 1:length(cast_type)) {
    castall <- cast_type[[i]]
    col <- names(cast_type)[i]
    
    if (is.list(castall)) {
      # We assume the first argument is the function
      castfunc <- castall[[1]]
      
      # Other arguments are the options
      rest <- castall[-1]
      args <- c(list(res[[col]]),
                rest) 
    } else {
      # Only one argument is the function
      castfunc <- castall
      
      # Args is only the column
      args <- list(res[[col]])
    }
    
    col <- names(cast_type)[[i]]
    
    res[[col]] <- do.call(castfunc, 
                          args)
    
  }
  
  return(res)
}

#' Add tryFormats
#'
#' Adds a `tryFormats` element to the specified elements of 
#' `castlist`.
#' 
#' @param castlist A named list of types conversions to perform 
#' @param formats The formats to add to `tryFormats`
#' @param names_to_add The names of the elements of `castlist` 
#' for which to add a `tryFormats` element
#'
#' @return The original `castlist` where the specified elements
#' have a new slot `tryFormats` which contains the `formats` vector.
#' If the value of `castlist` to modify is `NULL`, will not add 
#' `tryFormats` slot.
add_tryformats <- function(castlist,
                           formats,
                           names_to_add) {
  res <- castlist
  for (n in names_to_add) {
    cast_init <- res[[n]]
    if (!is.null(cast_init)) {
      cast_new <- list(cast_init,
                       tryFormats = formats)
      # Set new value
      res[[n]] <- cast_new
    }
  }
  return(res)
}


# Prepare data ------------------------------------------------------------


#' Prepare cameras
#' 
#' Prepare the camera data for cleaning
#'
#' @param dat The data. It can be either a list with one component `$data`
#' or a `datapackage` object (inheriting list). Either way, the data 
#' are in the `$data` slot with two components: 
#' + `$deployments` (cameras table)
#' + `$observations` (records table)
#' @param mapping_cameras The mapping list for columns in the cameras table 
#' (`dat$data$deployments`). The names of the list indicate the
#' the column type and the values indicate the relevant column names
#' to consider.
#' @param split should camera data be extracted from the records?
#' If yes, a cameras table will be created and replace the value
#' of `dat$data$deployments`.
#'
#' @return The dataset with "pre-cleaned" cameras data:
#' + the cameras table is filtered to keep unique rows across 
#' the columns indicated in `mapping_cameras`. This allows to filter 
#' for instance between duplicated cameras names.
#' + if `split` is `TRUE`, `dat$data$deployments` is replaced with 
#' data extracted from `dat$data$observations` 
#' (only the columns indicated in `mapping_cameras`).
prepare_cameras <- function(dat, mapping_cameras, split = FALSE) {
  
  # Initialize results
  res <- dat
  
  if (split) { # Manual file input
    # Split data
    cameras <- dat$data$observations %>%
      dplyr::select(all_of(unname(unlist(mapping_cameras))))
    res$data$deployments <- cameras
  }
  
  # Select unique rows for camera table
  # We want rows to be unique across the used camera columns 
  # defined in mapping_cameras
  res$data$deployments <- res$data$deployments %>%
    distinct(across(all_of(unname(unlist(mapping_cameras)))),
             .keep_all = TRUE)
  
  return(res)
}



#' Format table
#' 
#' Casts columns as indicated in `cast_type` and 
#' moves columns indicated in `mapping` to the beginning
#' of the table.
#'
#' @param df The dataframe to format
#' @param mapping The mapping list for columns in the dataframe.
#' Names are the column types (common between `mapping` and
#' `cast_type`). Values are the corresponding column names
#' in the dataframe. 
#' @param cast_type A named list containing the name of the 
#' function to cast between types.
#' The list's names are the names of the columns column types 
#' (common between `mapping` and `cast_type`). 
#' Elements of this list can be:
#' + a character giving a valid function name to call
#' + a list with the first element being the function to call (character)
#' and additional arguments to the function call (that can be named as 
#' the names of the functions' arguments).
#'
#' @return The dataframe df with reordered and casted columns.
#' 
#' @export
#'
#' @examples
#' # Create synthetic dataset
#' df <- data.frame(camera = c("A", "B", "C"),
#'                  lat = c("20.12", "20.22", "22.34"),
#'                  lon = c("33.44", "33.45", "33.42"),
#'                  setup = c("2022-01-01", "2022-01-01", "2022-01-02"))
#' mapping <- list(cam_col = "camera",
#'                 lat_col = "lat",
#'                 lon_col = "lon",
#'                 setup_col = "setup")
#' type <- list(cam_col = "as.character",
#'              lat_col = "as.numeric",
#'              lon_col = "as.numeric",
#'              setup_col = list("as.Date",
#'                               format = "%Y-%m-%d"))
#' # Format table
#' format_table(df, mapping, type)
format_table <- function(df, mapping, cast_type) {
  
  # Vector from list (NULL will be discarded)
  vec <- unlist(mapping)
  
  # Check arguments are named
  if (is.null(names(mapping)) || is.null(names(cast_type))) {
    stop("mapping and cast_type must be named")
  }
  # Check all cast_types are in mapping
  if ( !all(names(cast_type) %in% names(mapping)) ) {
    stop("all columns listed in cast_type must be in mapping")
  }
  # Check all non-null mapping are in cast_type
  if ( !all(names(vec) %in% names(cast_type)) ) {
    stop("all non-null columns listed in mapping must be in cast_type")
  }
  
  res <- df %>%
    select(all_of(unname(vec)),
           everything())
  
  # Cast columns
  # Reorder
  castval <- cast_type[names(vec)]
  names(castval) <- vec
  res <- cast_columns(res,
                      castval)
  
  # Drop NA
  # res <- remove_rows_with_NA(res, vec)
  
  return(res)
}

#' Filter cameras in both tables
#'
#' Filter data to keep only rows where cameras are in both tables
#' 
#' @param records Records dataframe
#' @param cameras Cameras dataframe
#' @param cam_col_records Name of the column with cameras names in records
#' @param cam_col_cameras Name of the column with cameras names in cameras.
#' If `NULL` will be assumed to be the same as `cam_col_records`. 
#'
#' @return A list of two dataframes with filtered values:
#' + `$records` is the records dataframe
#' + `$cameras` is the cameras dataframe
#' 
#' @export
#' 
#' @examples
#' records <- data.frame(species = c("pigeon", "mouse", "pigeon", "mouse"),
#'                       stamp = Sys.time() + seq(60, length.out = 4, by = 60),
#'                       camera = c("A", "B", "C", "E"))
#' cameras <- data.frame(camera = c("A", "B", "C", "D"), 
#'                       lat = c(20.12, 20.22, 22.34, 21.35),
#'                       lon = c(33.44, 33.45, 33.42, 33.53))
#' filter_cameras_in_both_tables(records, cameras, 
#'                               cam_col_records = "camera")
filter_cameras_in_both_tables <- function(records, cameras, 
                                          cam_col_records, 
                                          cam_col_cameras = NULL) {
  
  # Set cam_col_cameras
  if (is.null(cam_col_cameras)) {
    cam_col_cameras <- cam_col_records
  }
  
  # Get unique camera names
  ucam_records <- unique(records[[cam_col_records]])
  ucam_cameras <- unique(cameras[[cam_col_cameras]])
  
  # Get intersection
  cam_both <- intersect(ucam_records, 
                        ucam_cameras)
  
  # Restrict data to shared cameras
  records <- records %>%
    filter(.data[[cam_col_records]] %in% cam_both)
  
  cameras <- cameras %>%
    filter(.data[[cam_col_cameras]] %in% cam_both)
  
  res <- list(records = records,
              cameras = cameras)
  
  return(res)
}


# Final function ----------------------------------------------------------

#' Clean data
#'
#' Cleans data by:
#' + splitting records data in records and cameras (if needed)
#' + formatting cameras and records tables: casting columns and
#' moving the selected columns to the beginning
#' + if `only_shared_cameras` is `TRUE`: selecting the subset of 
#' cameras present in both records and cameras tables
#'  
#' @param dat The data to clean. It can be either a list with one component `$data`
#' or a `datapackage` object (inheriting list). Either way, the data 
#' are in the `$data` slot with two components: 
#' + `$deployments` (cameras table)
#' + `$observations` (records table)
#' @param mapping_cameras The mapping list for columns in the cameras table.
#' Names are the column types (common between `mapping_cameras` and `cam_type`). 
#' Values are the corresponding column names in `dat$data$deployments`. 
#' Names are free except the camera column which must be identified with `cam_col`.
#' @param cam_type A named list containing the name of the 
#' function to cast between types for the cameras table.
#' The list's names are the names of `mapping_cameras` 
#' corresponding to the columns to cast in `dat$data$deployments`.
#' For details on the content of this list, see the documentation of 
#' the `cast_columns` function.
#' @param mapping_records The mapping list for columns in the cameras table.
#' Names are the column types (common between `mapping_records` and `rec_type`). 
#' Values are the corresponding column names in `dat$data$observations`. 
#' Names are free except the camera column which must be identified with `cam_col`.
#' @param rec_type A named list containing the name of the 
#' function to cast between types for the records table.
#' The list's names are the names of `mapping_records` 
#' corresponding to the columns to cast in `dat$data$observations`. 
#' For details on the content of this list, see the documentation of 
#' the `cast_columns` function.
#' @param split Logical; should the camera data be extracted from the 
#' records table by splitting the data?
#' @param only_shared_cameras Logical; restrict final data to shared cameras
#' that are in `dat$data$deployments` and in `dat$data$observations`?
#'
#' @return An object of the same type as the original input,
#' but where `dat$data$deployments` and `dat$data$observations` have been
#' cleaned as described above.
#' 
#' @export
#' 
#' @examples
#' # Create synthetic dataset
#' records <- data.frame(foo = 1:6,
#'                       species = c("pigeon", "mouse", "pigeon", "mouse", "mouse", "pigeon"),
#'                       date = c("2022-01-01", "2022-03-01", 
#'                                "2022-01-02", "2022-01-12", "2022-01-22",
#'                                "2022-01-03"),
#'                       time = c("10:22:01", "22:12:01",
#'                                "11:54:33", "07:14:38", "18:01:34", 
#'                                "12:11:34"),
#'                       camera = c("A", "A", "B", "B", "B", "C"))
#' cameras <- data.frame(camera = c("A", "B", "C"),
#'                       lat = c("20.12", "20.22", "22.34"),
#'                       lon = c("33.44", "33.45", "33.42"))
#' dat <- list(data = list(observations = records,
#'                         deployments = cameras))
#' mapping_records <- list(species_col = "species",
#'                         date_col = "date",
#'                         time_col = "time",
#'                         cam_col = "camera")
#' rec_type <- list(species_col = "as.character",
#'                  date_col = list("as_date",
#'                                  format = "%Y-%m-%d"),
#'                  time_col = "times",
#'                  cam_col = "as.character")
#' mapping_cameras <- list(cam_col = "camera",
#'                         lat_col = "lat",
#'                         lon_col = "lon")
#' cam_type <- list(cam_col = "as.character",
#'                  lat_col = "as.numeric",
#'                  lon_col = "as.numeric")
#' # Clean data converts columns to the appropriate types 
#' # and reorders columns
#' clean_data(dat,
#'            mapping_records = mapping_records, rec_type = rec_type,
#'            mapping_cameras = mapping_cameras, cam_type = cam_type)
clean_data <- function(dat, 
                       mapping_cameras, 
                       cam_type,
                       mapping_records,
                       rec_type,
                       only_shared_cameras = FALSE,
                       split = FALSE) {
  
  # Prepare cameras ---
  res <- prepare_cameras(dat, 
                         mapping_cameras = mapping_cameras, 
                         split = split)
  
  # Records ---
  res$data$observations <- format_table(res$data$observations,
                                        mapping = mapping_records,
                                        cast_type = rec_type)
  
  # Cameras ---
  res$data$deployments <- format_table(res$data$deployments,
                                       mapping = mapping_cameras,
                                       cast_type = cam_type)
  
  # Both data ---
  if (only_shared_cameras) {
    # Restrict data to shared cameras
    # Get column names ---
    cam_col_records <- mapping_records[["cam_col"]]
    cam_col_cameras <- mapping_cameras[["cam_col"]]
    bothcam <- filter_cameras_in_both_tables(res$data$observations,
                                             res$data$deployments, 
                                             cam_col_records,
                                             cam_col_cameras)
    
    res$data$observations <- bothcam$records
    res$data$deployments <- bothcam$cameras
  }
  
  return(res)
}


# Other -------------------------------------------------------------------


#' Remove rows with NA
#'
#' @param df The dataframe for which to remove NA
#' @param mapping The mapping list for columns in the dataframe.
#' Names are the column types. They are free, except the
#' species column and the observation type column, which
#' must be coded with respectively `obs_col` and `spp_col`
#' (if they exist). If `obs_col` is present in the names,
#' then `spp_col` is assumed to be present too.
#' Values are the corresponding column names in the dataframe. 
#' 
#' @return The filtered dataframe, so that rows in `mapping` contain no 
#' `NA` values. `NA` are allowed only in `spp_col` if `obs_col` 
#' is in mapping is not `animal`.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(species = c("pigeon", "mouse", NA, "pigeon"), 
#'                  type = c("animal", "animal", "blank", "animal"),
#'                  stamp = Sys.time() + seq(60, length.out = 4, by = 60), 
#'                  camera = c("A", "B", "C", NA))
#' mapping <- list(spp_col = "species",
#'                 obs_col = "type",
#'                 timestamp_col = "stamp",
#'                 cam_col = "camera")
#' remove_rows_with_NA(df, mapping)
remove_rows_with_NA <- function(df, mapping) {
  
  if ("obs_col" %in% names(mapping)) {
    # Authorize NA values in spp_col where obs_col is not "species"
    spp_col_name <- mapping[["spp_col"]]
    obs_col_name <- mapping[["obs_col"]]
    
    na_check <- mapping[mapping != spp_col_name]
    na_check <- unlist(na_check)
    
    # Drop NA in all columns except spp_col
    res <- df %>%
      tidyr::drop_na(all_of(unname(na_check)))
    
    obs_col <- res[[obs_col_name]]
    spp_col <- res[[spp_col_name]]
    # Get the NA values of spp_col name where obs_col is 'animal'
    spp_NA <- which(is.na(spp_col) & obs_col == "animal")
    
    if (length(spp_NA) != 0) {
      res <- res[-spp_NA,]
    }
  } else { # No obs_col
    # No NAs authorized in the mapping columns
    res <- df %>%
      tidyr::drop_na(all_of(unname(mapping)))
  }
  
  return(res)
}