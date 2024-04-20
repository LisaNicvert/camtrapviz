# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: tests format data functions

library(testthat)
library(chron)

# Cast --------------------------------------------------------------------

test_that("Cast columns errors", {
  df <- data.frame(num = 1:10,
                   char = letters[1:10])
  
  # Not a list
  cast <- c("as.character", "as.factor", "as.numeric")
  expect_error(cast_columns(df, cast), 
               "cast_type must be a list",
               fixed = TRUE)
  
  # List has no names
  cast <- list("as.character", "as.factor", "as.numeric")
  expect_error(cast_columns(df, cast), 
               "All elements of cast_type must be named",
               fixed = TRUE)
  
  # List has not all names
  names(cast)[1] <- "a"
  expect_error(cast_columns(df, cast), 
               "All elements of cast_type must be named",
               fixed = TRUE)
  
  names(cast)[1] <- ""
  expect_error(cast_columns(df, cast), 
               "All elements of cast_type must be named",
               fixed = TRUE)
  
  # List has names not in df colnames
  cast <- list(num = "as.character",
               char = "as.factor",
               foo = "as.factor")
  expect_error(cast_columns(df, cast), 
               "names(cast)[3] (value: foo) is not in the column names of df",
               fixed = TRUE)
})


test_that("Cast columns", {
  df <- data.frame(num = 1:10,
                   char = letters[1:10])
  cast <- list(num = "as.character",
               char = "as.factor")
  
  dfcast <- cast_columns(df, cast)
  
  expect_equal(class(dfcast$num), "character")
  expect_equal(class(dfcast$char), "factor")
  
  # Test cast date with option
  df <- data.frame(num = 1:10,
                   date = as.character(rep(Sys.Date(), 10)))
  cast <- list(num = "as.character",
               date = list("as_date",
                           "format" = "%Y-%m-%d"))
  
  dfcast <- cast_columns(df, cast)
  
  expect_equal(class(dfcast$num), "character")
  expect_equal(class(dfcast$date), "Date")
})

test_that("add_date_options", {
  # Normal case
  cast <- list(num = "as.character",
               date = "as.Date",
               date2 = "as.Date")
  
  fmt <- c("%Y-%m-%d", "%d-%m-%Y")
  res <- add_date_options(cast, 
                          formats = fmt, 
                          names_to_add = c("date", "date2"))
  expected <- cast
  expected$date <- list("as.Date",
                        tryFormats = fmt)
  expected$date2 <- list("as.Date",
                         tryFormats = fmt)
  
  expect_equal(res, expected)
  
  # NULL
  cast <- list(num = "as.character",
               date = NULL)
  
  fmt <- c("%Y-%m-%d", "%d-%m-%Y")
  res <- add_date_options(cast, 
                          formats = fmt, 
                          names_to_add = "date")
  expected <- cast
  expect_equal(res, expected)
  
  # Size one case
  cast <- list(num = "as.character",
               date = "as.Date",
               date2 = "as.Date")
  
  fmt <- c("%Y-%m-%d")
  res <- add_date_options(cast, 
                          formats = fmt, 
                          names_to_add = c("date", "date2"))
  
  expected <- cast
  expected$date <- list("as.Date",
                        format = fmt)
  expected$date2 <- list("as.Date",
                         format = fmt)
  
  expect_equal(res, expected)
  
  # With timezone
  cast <- list(num = "as.character",
               date = "as.Date",
               date2 = "as.Date")
  
  fmt <- c("%Y-%m-%d", "%d-%m-%Y")
  tz <- "GMT"
  res <- add_date_options(cast, 
                          formats = fmt, 
                          tz = tz,
                          names_to_add = c("date", "date2"))
  expected <- cast
  expected$date <- list("as.Date",
                        tryFormats = fmt,
                        tz = tz)
  expected$date2 <- list("as.Date",
                         tryFormats = fmt,
                         tz = tz)
  expect_equal(res, expected)
  
  # With timezone only
  cast <- list(num = "as.character",
               date = "as.Date",
               date2 = "as.Date")
  
  tz <- "GMT"
  res <- add_date_options(cast, 
                          tz = tz,
                          names_to_add = c("date", "date2"))
  expected <- cast
  expected$date <- list("as.Date",
                        tz = tz)
  expected$date2 <- list("as.Date",
                         tz = tz)
  expect_equal(res, expected)
})


# Split records cameras ---------------------------------------------------------
test_that("split_records_cameras", {
  # Prepare data
  dat <- list(data = list(deployments = NULL,
                          observations = kga_all))
  
  # Test with list
  cam_cols <- list(cam_col = "cameraID",
                   loc_col = "locationID",
                   elev_col = "elevation")
  
  dat_clean <- split_records_cameras(dat = dat, 
                                     cam_cols = cam_cols,
                                     cam_col = "cameraID")
  expect_equal(ncol(dat_clean$data$observations), ncol(dat$data$observations) - length(cam_cols) + 1)
  expect_equal(ncol(dat_clean$data$deployments), length(cam_cols))
  expect_equal(colnames(dat_clean$data$deployments), unname(unlist(cam_cols)))
  expect_equal(nrow(dat_clean$data$deployments), length(unique(kga_all$cameraID)))
  
  # Test with vector
  cam_cols <- c("cameraID", loc_col = "locationID", elev_col = "elevation")
  
  dat_clean <- split_records_cameras(dat = dat, 
                                     cam_cols = cam_cols,
                                     cam_col = "cameraID")
  expect_equal(ncol(dat_clean$data$observations), ncol(dat$data$observations) - length(cam_cols) + 1)
  expect_equal(ncol(dat_clean$data$deployments), length(cam_cols))
  expect_equal(colnames(dat_clean$data$deployments), unname(unlist(cam_cols)))
  expect_equal(nrow(dat_clean$data$deployments), length(unique(kga_all$cameraID)))
  
})

test_that("split_records_cameras (error)", {
  obs <- data.frame(cam = paste0("cam", LETTERS[1:5]),
                    date = rep(Sys.time(), 5),
                    lat = 1:5,
                    lon = 11:15)
  
  dat <- list(data = list(deployments = NULL,
                          observations = obs))
  
  cam_cols <- c("cam", "lat", "lon")
  
  # cam_cols not in colnames dat$data$observations
  expect_error(split_records_cameras(dat = dat, 
                                     cam_cols = c("lon", "xx", "lat")), 
               "cam_cols[2] (value: xx) is not in the column names of dat$data$observations", 
               fixed = TRUE)
  
  
  # cam_col not in cam_cols
  expect_error(split_records_cameras(dat = dat, 
                                     cam_cols = cam_cols,
                                     cam_col = "CAM"), 
               "cam_col should be in cam_cols", 
               fixed = TRUE)
  
})



# Get cameras in both table -----------------------------------------------

test_that("Cameras in both table", {
  dfrec <- data.frame(species = c("pigeon", "mouse", "pigeon", "mouse"),
                        stamp = Sys.time() + seq(60, length.out = 4, by = 60),
                        camera = c("A", "B", "C", "E"))
  dfcam <- data.frame(camera = c("A", "B", "C", "D"),
                      lat = c(20.12, 20.22, 22.34, 21.35),
                      lon = c(33.44, 33.45, 33.42, 33.53))
  
  # Errors
  expect_error(filter_cameras_in_both_tables(dfrec, dfcam,
                                             cam_col_dfrec = "xxx"),
               "cam_col_dfrec (value: xxx) is not in the column names of dfrec",
               fixed = TRUE)
  expect_error(filter_cameras_in_both_tables(dfrec, dfcam,
                                             cam_col_dfrec = "camera",
                                             cam_col_dfcam = "xxx"),
               "cam_col_dfcam (value: xxx) is not in the column names of dfcam",
               fixed = TRUE)
  
  # No error
  res <- filter_cameras_in_both_tables(dfrec, dfcam,
                                       cam_col_dfrec = "camera")
  
  ucam <- c("A", "B", "C")
  expected_dfrec <- dfrec |> 
    dplyr::filter(camera %in% ucam)
  expected_dfcam <- dfcam |> 
    dplyr::filter(camera %in% ucam)
  
  expected <- list(records = expected_dfrec,
                   cameras = expected_dfcam)
  
  expect_equal(res, expected)
})

# Final function ----------------------------------------------------------
test_that("Clean data (errors)", {
  # Prepare data
  dat <- list(data = list(deployments = camtraps,
                          observations = recordTableSample))
  
  # Error in cast_rec
  cast_rec <- list(Species = "as.character",
                   foo = "as.character")
  
  expect_error(clean_data(dat = dat,
                          rec_type = cast_rec,
                          split = FALSE),
               "names(rec_type)[2] (value: foo) is not in the column names of dat$data$observations",
               fixed = TRUE)
  
  # Error in cast_cam
  cast_cam <- list(utm_x = "as.numeric",
                   foo = "as.character")
  
  expect_error(clean_data(dat = dat,
                          cam_type = cast_cam,
                          split = FALSE),
               "names(cam_type)[2] (value: foo) is not in the column names of dat$data$deployments",
               fixed = TRUE)
  
  # Error in cam_cols (split)
  expect_error(clean_data(dat = dat,
                          split = TRUE,
                          cam_cols = c("Station", "foo")),
               "cam_cols[2] (value: foo) is not in the column names of dat$data$observations",
               fixed = TRUE)
  
  # Error in cam_col_dfrec (only_shared_cam)
  expect_error(clean_data(dat = dat,
                          cam_col_dfrec = "foo",
                          only_shared_cam = TRUE),
               "cam_col_dfrec (value: foo) is not in the column names of dfrec",
               fixed = TRUE)
               
  
  # Error in cam_col_dfcam (only_shared_cam)
  expect_error(clean_data(dat = dat,
                          cam_col_dfcam = "foo",
                          only_shared_cam = TRUE),
               "cam_col_dfcam (value: foo) is not in the column names of dfcam",
               fixed = TRUE)
  
})

test_that("Clean data", {
  
  # Normal case
  dat <- list(data = list(deployments = camtraps,
                          observations = recordTableSample))
  
  cast_cam <- list(Station = "as.character",
                   utm_y = "as.numeric",
                   utm_x = "as.numeric")
  
  cast_rec <- list(Species = "as.character",
                   Station = "as.character",
                   DateTimeOriginal = "as.POSIXct")
  
  # Change some classes to check the cast works
  dat$data$deployments$Station <- factor(dat$data$deployments$Station)
  dat$data$deployments$utm_y <- factor(dat$data$deployments$utm_y)
  
  dat$data$observations$Species <- factor(dat$data$observations$Species)
  dat$data$observations$Station <- factor(dat$data$observations$Station)
  
  dat_clean <- clean_data(dat = dat, 
                          cam_type =  cast_cam,
                          rec_type = cast_rec,
                          split = FALSE)
  # Test classes for records
  expect_equal(class(dat_clean$data$observations$Species), "character")
  expect_equal(class(dat_clean$data$observations$Station), "character")
  expect_equal(class(dat_clean$data$observations$DateTimeOriginal), c("POSIXct", "POSIXt"))
  # Test classes for cameras
  expect_equal(class(dat_clean$data$deployments$Station), "character")
  expect_equal(class(dat_clean$data$deployments$utm_y), "numeric")
  expect_equal(class(dat_clean$data$deployments$utm_x), "numeric")
  
  # Split is TRUE and don't cast cameras
  dat <- list(data = list(deployments = NULL,
                          observations = kga_all))
  
  cast_rec <- list(snapshotName = "as.character",
                   locationID = "as.character",
                   eventDate = "as.Date",
                   eventTime = "times")
  cam_cols <- c("cameraID", "Lat_Y", "Long_X")
  
  dat_clean <- clean_data(dat = dat, 
                          rec_type = cast_rec,
                          split = TRUE,
                          cam_col_dfrec = "cameraID",
                          cam_cols = cam_cols)
  # Test split
  expect_equal(ncol(dat_clean$data$deployments), 3)
  expect_equal(ncol(dat_clean$data$observations), ncol(dat$data$observations) - 2)
  expect_equal(colnames(dat_clean$data$deployments), unname(unlist(cam_cols)))
  expect_equal(nrow(dat_clean$data$deployments), length(unique(kga_all$cameraID)))
  
  # Test classes for records
  expect_equal(class(dat_clean$data$observations$snapshotName), "character")
  expect_equal(class(dat_clean$data$observations$cameraID), "character")
  expect_equal(class(dat_clean$data$observations$eventDate), "Date")
  expect_equal(class(dat_clean$data$observations$eventTime), "times")
  
  
  # Split is TRUE and cast cameras
  dat <- list(data = list(deployments = NULL,
                          observations = kga_all))
  
  # Change Lat to factor to check cast
  dat$data$observations$Lat_Y <- factor(dat$data$observations$Lat_Y)
  cast_rec <- list(Lat_Y = "as.numeric")
  cam_cols <- c("cameraID", "Lat_Y", "Long_X")
  
  dat_clean <- clean_data(dat = dat, 
                          rec_type = cast_rec,
                          split = TRUE,
                          cam_col_dfrec = "cameraID",
                          cam_cols = cam_cols)
  
  # Test split
  expect_equal(ncol(dat_clean$data$deployments), 3)
  expect_equal(ncol(dat_clean$data$observations), ncol(dat$data$observations) - 2)
  expect_equal(colnames(dat_clean$data$deployments), unname(unlist(cam_cols)))
  expect_equal(nrow(dat_clean$data$deployments), length(unique(kga_all$cameraID)))
  
  # Test classes for cameras
  expect_equal(class(dat_clean$data$deployments$Lat_Y), "numeric")
})

test_that("Clean data (reorder)", {
  # create data
  dat <- list(data = list(deployments = camtraps,
                          observations = recordTableSample))
  
  cast_cam <- list(utm_y = "as.numeric",
                   utm_x = "as.numeric",
                   Station = "as.character")
  
  cast_rec <- list(DateTimeOriginal = "as.POSIXct",
                   Station = "as.character",
                   Species = "as.character")
  
  # Dont reorder
  dat_clean <- clean_data(dat = dat, 
                          rec_type = cast_rec,
                          cam_type = cast_cam)
  expect_equal(colnames(dat$data$observations), colnames(dat_clean$data$observations))
  expect_equal(colnames(dat$data$deployments), colnames(dat_clean$data$deployments))
  
  
  # Reorder
  dat_clean <- clean_data(dat = dat, 
                          rec_type = cast_rec,
                          cam_type = cast_cam,
                          reorder = TRUE)
  
  expected_obs <- colnames(dat$data$observations)
  expected_obs <- expected_obs[c(c(3, 1, 2), 4:length(expected_obs))]
  expect_equal(expected_obs, colnames(dat_clean$data$observations))
  
  expected_dep <- colnames(dat$data$deployments)
  expected_dep <- expected_dep[c(c(2, 3, 1), 4:length(expected_dep))]
  expect_equal(expected_dep, colnames(dat_clean$data$deployments))
  
})
  

# Remove rows with NA -----------------------------------------------------

test_that("Remove rows with NA", {
  df <- data.frame(species = c("pigeon", "mouse", NA, "pigeon"), 
                   type = c("animal", "animal", "blank", "animal"),
                   stamp = Sys.time() + seq(60, length.out = 4, by = 60),
                   camera = c("A", "B", "C", NA))
  mapping <- list(spp_col = "species",
                  obstype_col = "type",
                  datetime_col = "stamp",
                  cam_col = "camera")
  
  # Bad mapping
  mapping_error <- list(spp_col = "species",
                        obstype_col = "type",
                        foo = "foo")
  expect_error(remove_rows_with_NA(df, mapping_error),
               "mapping[3] (value: foo) is not in the column names of df",
               fixed = TRUE)
  
  # Success
  res <- remove_rows_with_NA(df, mapping)
  
  expected <- df[1:3, ]
  expect_equal(res, expected)
})
