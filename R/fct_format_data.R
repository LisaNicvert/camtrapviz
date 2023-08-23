# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: functions to format data


# Cast --------------------------------------------------------------------

#' Cast columns types
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
    
    res[[col]] <- tryCatch(do.call(castfunc, args),
                           error = function(e) {
                             e
                             return(NA)
                           })
    
  }
  
  return(res)
}

#' Add date options
#'
#' Adds a `format`, `tryFormats` and/or `tz` 
#' element to the specified elements of `castlist`.
#' 
#' @param castlist A named list of types conversions to perform.
#' @param formats The formats to add. If it is of length one, then
#' it is added to the name `format`, else if is added to the name 
#' `tryFormats`.
#' @param names_to_add The names of the elements of `castlist` 
#' for which to add the parameters.
#'
#' @noRd
#' @return The original `castlist` where the specified elements
#' have a new slot `format` or `tryFormats` and/or `tz`.
#' If the value of `castlist` to modify is `NULL`, will not add 
#' any options.
add_date_options <- function(castlist,
                             formats = NULL,
                             tz = NULL,
                             names_to_add) {
  res <- castlist
  for (n in names_to_add) {
    res_cast <- res[[n]]
    if (!is.null(res_cast)) { # modify only if the element of the list is not null
      if (!is.null(formats)) { # Add formats if not null
        if ("list" %in% class(res_cast)) { # if the element is a list
          if (length(formats) == 1) { # Name of the argument is format
            res_cast <- c(res_cast,
                          format = formats)
          } else { # Name of the argument is tryFormats
            res_cast <- c(res_cast,
                          tryFormats = formats)
          }
        } else {
          if (length(formats) == 1) { # Name of the argument is format
            res_cast <- list(res_cast,
                             format = formats)
          } else { # Name of the argument is tryFormats
            res_cast <- list(res_cast,
                             tryFormats = formats)
          }
        }
      }
      if (!is.null(tz)) { # Add tz if not null
        if ("list" %in% class(res_cast)) {
          res_cast <- c(res_cast,
                        tz = tz)
        } else {
          res_cast <- list(res_cast,
                           tz = tz)
        }
      }
      # Set new value
      res[[n]] <- res_cast
    }
  }
  return(res)
}


# Prepare data ------------------------------------------------------------


#' Split records data
#' 
#' Splits the records data to create a cameras dataframe.
#'
#' @param dat The data. It can be either a list with one component `$data`
#' or a `datapackage` object (inheriting list). Either way, the data 
#' are in the `$data` slot with two components: 
#' + `$deployments` (cameras table)
#' + `$observations` (records table)
#' @param cam_cols A list or vector of columns names that indicate 
#' columns that should be moved to `dat$data$deployments`.
#' Defaults to a unique column (`deploymentID`).
#' @param cam_col name of the camera column. This column must
#' be in `cam_cols` and will be kept in 
#' `dat$data$observations` after the split.
#' Defaults to `deploymentID`.
#'
#' @noRd
#' 
#' @return The data with camera columns moved to `dat$data$deployments`.
#' `dat$data$deployments` is replaced with data extracted from 
#' columns indicated in `cam_cols` from `dat$data$observations` 
#' (keeping only unique rows across columns in `cam_cols`).
#' These columns are then removed from `dat$data$observations`.
#' (except `cam_name`)
split_records_cameras <- function(dat, 
                                  cam_cols = "deploymentID", 
                                  cam_col = "deploymentID") {
  
  # Initialize results
  res <- dat
  
  # Check cam_col is in cam_cols
  if(!(cam_col %in% cam_cols)) {
    stop("cam_col should be in cam_cols.")
  }
  
  # Split data
  cameras <- dat$data$observations |>
    dplyr::select(all_of(unname(unlist(cam_cols)))) |> 
    distinct()
  
  res$data$deployments <- cameras
    
  # Remove camera columns
  to_remove <- cam_cols[cam_cols != cam_col]
  res$data$observations <- dat$data$observations |>
    dplyr::select(-all_of(unname(unlist(to_remove))))
  
  return(res)
}


#' Filter cameras in both tables
#'
#' Filter data to keep only rows where cameras are in both tables
#' 
#' @param dfrec Records dataframe
#' @param dfcam Cameras dataframe
#' @param cam_col_dfrec Name of the column with cameras names in `dfrec`
#' @param cam_col_dfcam Name of the column with cameras names in `dfcam`
#' If `NULL` will be assumed to be the same as `cam_col_dfrec`. 
#'
#' @return A list of two dataframes with filtered values:
#' + `$records` is the records dataframe
#' + `$cameras` is the cameras dataframe
#' 
#' @export
#' 
#' @examples
#' dfrec <- data.frame(species = c("pigeon", "mouse", "pigeon", "mouse"),
#'                       stamp = Sys.time() + seq(60, length.out = 4, by = 60),
#'                       camera = c("A", "B", "C", "E"))
#' dfcam <- data.frame(camera = c("A", "B", "C", "D"), 
#'                       lat = c(20.12, 20.22, 22.34, 21.35),
#'                       lon = c(33.44, 33.45, 33.42, 33.53))
#' filter_cameras_in_both_tables(dfrec, dfcam, 
#'                               cam_col_dfrec = "camera")
filter_cameras_in_both_tables <- function(dfrec, dfcam, 
                                          cam_col_dfrec, 
                                          cam_col_dfcam = NULL) {
  
  # Set cam_col_dfcam
  if (is.null(cam_col_dfcam)) {
    cam_col_dfcam <- cam_col_dfrec
  }
  
  # Get unique camera names
  ucam_records <- unique(dfrec[[cam_col_dfrec]])
  ucam_cameras <- unique(dfcam[[cam_col_dfcam]])
  
  # Get intersection
  cam_both <- intersect(ucam_records, 
                        ucam_cameras)
  
  # Restrict data to shared cameras
  dfrec <- dfrec |>
    filter(.data[[cam_col_dfrec]] %in% cam_both)
  
  dfcam <- dfcam |>
    filter(.data[[cam_col_dfcam]] %in% cam_both)
  
  res <- list(records = dfrec,
              cameras = dfcam)
  
  return(res)
}


# Final function ----------------------------------------------------------

#' Clean data
#'
#' Cleans data by:
#' + splitting records data in records and cameras (if needed)
#' + formatting cameras and records tables: casting specified columns 
#' and moving them to the beginning
#' + if `only_shared_cam` is `TRUE`: selecting the subset of 
#' cameras present in both records and cameras tables
#'  
#' @param dat The data to clean. It can be either a list with one component `$data`
#' or a `datapackage` object (inheriting list). Either way, the data 
#' are in the `$data` slot with two components: 
#' + `$deployments` (cameras table)
#' + `$observations` (records table)
#' @param rec_type A named list containing the name of the 
#' function to cast between types for the records table.
#' In case `split = TRUE` and some columns that will be moved
#' to the cameras table must be converted, they should be in this list.
#' If `NULL`, the records table will not be modified or its columns 
#' reordered.
#' The list's names are the names of the columns to cast 
#' in `dat$data$observations`. 
#' For details on the content of this list, see the documentation of 
#' the `cast_columns` function.
#' @param cam_type A named list containing the name of the 
#' function to cast between types for the cameras table. It is used
#' only if `split = FALSE`.
#' If `NULL`, the cameras table will not be modified or its columns 
#' reordered.
#' The list's names are the names of the columns to cast 
#' in `dat$data$deployments`.
#' For details on the content of this list, see the documentation of 
#' the `cast_columns` function.
#' @param split Logical; should the camera data be extracted from the 
#' records table by splitting the data?
#' @param only_shared_cam Logical; restrict final data to shared cameras
#' that are in `dat$data$deployments` and in `dat$data$observations`?
#' @param cam_col_dfrec Name of the column with cameras names in 
#' records (needed only if `split` or `only_shared_cam` is `TRUE`)
#' @param cam_col_dfcam Name of the column with cameras names in 
#' cameras (needed only if `only_shared_cam` is `TRUE`).
#' If `NULL` will be assumed to be the same as `cam_col_dfrec`. 
#' @param add_rowid Should row IDs be added to the observations df?
#' If yes, row names in the form of "ID_xx" are added to the the dataframe.
#' @param cam_cols A character vector of the columns in `dfrec` that should
#' be moved to the `dat$data$deployments` dataframe if `split = TRUE`.
#' 
#' @return An object of the same type as the original input,
#' but where `dat$data$deployments` and `dat$data$observations` have been
#' cleaned as described above.
#' 
#' @export
#' 
#' @seealso [\code{vignette("read-and-clean-data", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/read-and-clean-data.html)
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
#'                         
#' # Clean data
#' rec_type <- list(date = list("as.Date",
#'                                  format = "%Y-%m-%d"),
#'                  time = "times")
#' cam_type <- list(lat = "as.numeric",
#'                  lon = "as.numeric")
#' 
#' # Clean data converts columns to the appropriate types 
#' # and reorders columns
#' clean_data(dat,
#'            rec_type = rec_type,
#'            cam_type = cam_type)
clean_data <- function(dat, 
                       cam_type = NULL,
                       rec_type = NULL,
                       only_shared_cam = FALSE,
                       cam_col_dfrec = NULL,
                       cam_col_dfcam = NULL,
                       split = FALSE,
                       cam_cols = ifelse(split, cam_col_dfrec, NULL),
                       add_rowid = FALSE) {
  
  # Initialize data ---
  res <- dat 
  
  # Records ---
  # Cast
  if (!is.null(rec_type)) {
    res$data$observations <- cast_columns(res$data$observations,
                                          cast_type = rec_type)
    
    # Reorder
    res$data$observations <- res$data$observations |>
      select(all_of(names(rec_type)),
             everything())
  }
  
  # Split cameras ---
  if (split) { 
    # We need to extract relevant columns from dfrec
    res <- split_records_cameras(res, 
                                 cam_cols = cam_cols, 
                                 cam_col = cam_col_dfrec)
  } else {
    # Cameras ---
    if (!is.null(cam_type)) {
      # Cast
      res$data$deployments <- cast_columns(res$data$deployments,
                                           cast_type = cam_type)
      
      # Reorder
      res$data$deployments <- res$data$deployments |>
        select(all_of(names(cam_type)),
               everything())
    }
  }
  
  # Both data ---
  if (only_shared_cam) {
    # Restrict data to shared cameras
    bothcam <- filter_cameras_in_both_tables(res$data$observations,
                                             res$data$deployments, 
                                             cam_col_dfrec,
                                             cam_col_dfcam)
    
    res$data$observations <- bothcam$records
    res$data$deployments <- bothcam$cameras
  }
  
  if (add_rowid) {
    nrow <- nrow(res$data$observations)
    res$data$observations <- as.data.frame(res$data$observations)
    rownames(res$data$observations) <- paste("ID", 1:nrow,
                                             sep = "_")
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
#' must be coded with respectively `obstype_col` and `spp_col`
#' (if they exist). If `obstype_col` is present in the names,
#' then `spp_col` is assumed to be present too.
#' Values are the corresponding column names in the dataframe. 
#' 
#' @return The filtered dataframe, so that rows in `mapping` contain no 
#' `NA` values. `NA` are allowed only in `spp_col` if `obstype_col` 
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
#'                 obstype_col = "type",
#'                 datetime_col = "stamp",
#'                 cam_col = "camera")
#' remove_rows_with_NA(df, mapping)
remove_rows_with_NA <- function(df, mapping) {
  
  if ("obstype_col" %in% names(mapping)) {
    # Authorize NA values in spp_col where obstype_col is not "species"
    spp_col_name <- mapping[["spp_col"]]
    obstype_col_name <- mapping[["obstype_col"]]
    
    na_check <- mapping[mapping != spp_col_name]
    na_check <- unlist(na_check)
    
    # Drop NA in all columns except spp_col
    res <- df |>
      tidyr::drop_na(all_of(unname(na_check)))
    
    obstype_col <- res[[obstype_col_name]]
    spp_col <- res[[spp_col_name]]
    # Get the NA values of spp_col name where obstype_col is 'animal'
    spp_NA <- which(is.na(spp_col) & obstype_col == "animal")
    
    if (length(spp_NA) != 0) {
      res <- res[-spp_NA,]
    }
  } else { # No obstype_col
    # No NAs authorized in the mapping columns
    res <- df |>
      tidyr::drop_na(all_of(unname(mapping)))
  }
  
  return(res)
}
