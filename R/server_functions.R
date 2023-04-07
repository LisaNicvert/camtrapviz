# Import ------------------------------------------------------------------


## Read data ---------------------------------------------------------------


#' Get separator for a file
#'
#' Detects the separator from one line of a file.
#' 
#' @param line A line from a file
#' @param default The default separator to use in case none work
#'
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
#' @return A named character vector, excluding NA values in col
get_example_mapping <- function(df, col) {
  
  res_df <- df %>% 
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
#' @return A named list containing the values of col, 
#' names are the widget names
get_named_list <- function(df, col, widget_values) {
  
  res_df <- df %>%
    filter(widget %in% widget_values) 
  res <- res_df %>%
    extract2(col)
  res <- as.list(res)
  names(res) <- res_df$widget
  return(res)
}

#' Read data
#'
#' Reads data from a file path (either csv of json file), and 
#' optionally from another csv file (with camera data).
#'
#' @param records_path A valid file path for records.
#' @param sep_records separator used for the records.
#' @param sep_cameras separator used for the cameras (defaults to `NULL`).
#' @param cameras_path A valid file path for cameras (defaults to `NULL`).
#' @param NA_strings Vector of characters that should be considered `NA`
#' after import
#'
#' @return A list with one component or a `datapackage` object.
#' + if `records_path` is a json file, returns a `datapackage` object
#' (inheriting list). The data is in the `$data` slot, 
#' which is a list with 2 components: 
#'    + `$deployments` (cameras)
#'    + `$observations` (records)
#' + if `records_path` is a csv file, returns a list with one component
#' named `$data`, which is a list with 2 components: 
#'    + `$deployments` (cameras: if no camera file was provided in `cameras_path`,
#'     it is `NULL`)
#'    + `$observations` (records)
#'    
#' @export
#' @examples
#' \dontrun{
#' # Read only records
#' read_data(records_path = "records.csv", sep_records = ",")
#' # Read records and cameras
#' read_data(records_path = "records.csv", sep_records = ",",
#'           cameras_path = "cameras.csv", sep_cameras = ",")
#' # Read a json file
#' read_data(records_path = "datapackage.json")
#' }
read_data <- function(records_path,
                      sep_records, 
                      cameras_path = NULL,
                      sep_cameras = NULL,
                      NA_strings = c("NA", "")) {
  
  # Get file extension
  ext <- tools::file_ext(records_path)
  
  if (ext == "csv") { # The input is a csv file
    # Read csv
    res_records <- utils::read.csv(records_path, sep = sep_records,
                                   na.strings = NA_strings)
    
    if (!is.null(cameras_path)) { # There is a camera file
      # Read csv
      res_cameras <- utils::read.csv(cameras_path, sep = sep_cameras,
                                     na.strings = NA_strings)
    } else { # There is no camera file
      res_cameras <- NULL
    }
    
    res <- list(data = list(observations = res_records,
                            deployments = res_cameras))
    
  } else if (ext == "json") { # CamtrapDP format
    res <- camtraptor::read_camtrap_dp(records_path, media = FALSE)
  } else { # Unknown extension
    validate(need(ext == "csv" || ext == "json", 
                  "Please upload a csv file or a json datapackage"))
  }
  return(res)
}

## Default colnames --------------------------------------------------------


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


## Clean data --------------------------------------------------------------

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

# Summary -----------------------------------------------------------------

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
  camsum <- camsum %>%
    group_by(.data[[cam_col]]) %>%
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
      setup_df <- setup_df %>%
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
      retrieval_df <- retrieval_df %>%
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
  camsum_sampling_time <- camsum %>%
    filter(!is.na(setup)) %>%
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


# Plotting functions ------------------------------------------------------


#' Plot points
#' 
#' Plot species occurrences at cameras as points
#'
#' @param df The dataframe
#' @param camera_col Name of the camera column
#' @param spp_col Name of the species column
#' @param timestamp_col Name of the timestamp column. If it
#' is a datetime, it must be of class POSIXct. It can be `NULL` 
#' if `date_col` and `time_col` are provided.
#' @param date_col Name of the date column. It is assumed
#' to be of class `Date` (else, results are not guaranteed). 
#' It can be `NULL` if `timestamp_col` is provided.
#' @param time_col Name of the time column. It is assumed
#' to be of class `times` (else, results are not guaranteed). 
#' It can be `NULL` if `timestamp_col` is provided.
#' @param interactive Logical; make the plot interactive with `ggiraph`?
#'
#' @details If `date_col` and `time_col` are provided along with
#' `timestamp_col`, they will be ignored.
#' 
#' @return A `ggplot` object representing time on the x-axis
#' and cameras on the y-axis. Colors of the points correspond to 
#' different species.
#'
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' recordTableSample$DateTimeOriginal <- as.POSIXct(recordTableSample$DateTimeOriginal)
#' plot_points(recordTableSample, 
#'             camera_col = "Station", 
#'             timestamp_col = "DateTimeOriginal", 
#'             spp_col = "Species")
plot_points <- function(df, 
                        camera_col,
                        spp_col,
                        timestamp_col,
                        date_col = NULL,
                        time_col = NULL,
                        interactive = TRUE) {
  
  # Initialize plotting data
  dfp <- df
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if (is.null(date_col) | is.null(time_col)) {
      stop("If timestamp_col is not provided, date_col and time_col must be provided.")
    }
  }
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if("timestamp_col" %in% colnames(dfp)) {
      warning("timestamp_col already exists and this might interfer with plotting")
    }
    # Create a composite timestamp
    dfp$timestamp_col <- paste(as.character(dfp[[date_col]]), 
                               as.character(dfp[[time_col]]))
    
    dfp$timestamp_col <- as.POSIXct(dfp$timestamp_col)
    
    # Change timestamp_col value
    timestamp_col <- "timestamp_col"
  }
  
  if (interactive) {
    gg <- ggplot(dfp, aes(x = .data[[timestamp_col]], 
                          y = .data[[camera_col]],
                          col = .data[[spp_col]],
                          tooltip = paste(.data[[spp_col]], 
                                          .data[[timestamp_col]],
                                          sep = ": "),
                          data_id = .data[[camera_col]]
                          )) +
      geom_point_interactive(show.legend = FALSE)
  } else {
    gg <- ggplot(dfp, aes(x = .data[[timestamp_col]], 
                          y = .data[[camera_col]],
                          col = .data[[spp_col]])) +
      geom_point(show.legend = FALSE)
  }
  
  gg <- gg +
    theme_linedraw() + 
    xlab("Date") +
    ylab("Camera")
  
  return(gg)
}

#' Plot species bars
#'
#' Plot the barplot of species abundance
#'
#' @param df The dataframe
#' @param spp_col Name of the species column
#' @param count_col Name of the count column (optional). If missing,
#' it will be assumed to be 1 for all observations.
#' @param obs_col Name of the observation type column (optional).
#' If it is present, the function will plot only the observations
#' for which `obs_col` is "animal". 
#' @param interactive Logical; make the plot interactive with `ggiraph`?
#'
#' @return A `ggplot` object representing horizontal bars of species
#' count. The x-axis is the observed number of individuals and the y-axis
#' are the different species.
#' 
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' plot_species_bars(recordTableSample,
#'                   spp_col = "Species")
plot_species_bars <- function(df, 
                              spp_col, 
                              count_col = NULL,
                              obs_col = NULL,
                              interactive = TRUE) {
  
  # Initialize df plot
  dfp <- df
  
  if (!is.null(obs_col)) {
    # Get only the observations of type animal
    dfp <- dfp %>% filter(.data[[obs_col]] == "animal")
  }
  
  # Group by species
  dfp <- dfp %>% group_by(.data[[spp_col]])
  
  if (is.null(count_col)) { # no count column
    dfp <- dfp %>%
      summarise(count = n())
  } else { # count column
    dfp <- dfp %>%
      summarise(count = sum(.data[[count_col]]))
  }
   
  if (interactive) {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], count),
                          y = count,
                          tooltip = paste(.data[[spp_col]], count, 
                                          sep = ": ")
                          )) +
      geom_col_interactive()
  } else {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], count),
                          y = count)) +
      geom_col()
  }
  gg <- gg +
    coord_flip() +
    theme_linedraw() +
    ylab("Count") +
    theme(axis.title.y = element_blank())
  
  return(gg)
}

#' Plot map
#' 
#' Plot a leaflet map representing cameras' coordinates as points.
#' 
#' @param df A dataframe containing cameras information
#' @param lat_col Name of the latitude (or the projected y-coordinate) 
#' column
#' @param lon_col Name of the longitude (or the projected y-coordinate) 
#' column
#' @param crs EPSG code for the coordinate reference system (CRS)
#' Defaults to EPSG:4326, which is the code for WGS84 standard.
#' @param cam_col Name of the camera name column
#' @param color color for the points
#'
#' @return a `leaflet` map representing cameras as points.
#' If the CRS of the input data is different from EPSG:4326 (WGS84), 
#' data are re-projected using WGS84.
#' When hovering over a camera, it becomes red and its name is shown.
#' When clicking on a camera, a popup displaying the camera name appears.
#' 
#' @export
#' 
#' @examples
#' data(camtraps, package = "camtrapR")
#' plot_map(camtraps,
#'          lat_col = "utm_y", 
#'          lon_col = "utm_x",
#'          crs = 32650, # Here we use the EPSG code for UTM zone 50N
#'          cam_col = "Station")
plot_map <- function(df, 
                     lat_col, lon_col, 
                     crs = 4326,
                     cam_col,
                     color = "black") {
  
  if(!is.null(crs)) { # Specify the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col),
                          crs = as.numeric(crs))
    # Reproject in WGS84 (a.k.a. EPSG:4326)
    df_sf <- sf::st_transform(df_sf, 4326)
  } else { # Let leaflet choose the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col))
  }
  
  leaflet(df_sf) %>% 
    addTiles() %>% 
    addCircles(data = df_sf,
               label = paste0("Camera: ", df_sf[[cam_col]]),
               layerId = df_sf[[cam_col]],
               popup = df_sf[[cam_col]],
               color = color,
               highlightOptions = highlightOptions(color = "red"))
}

