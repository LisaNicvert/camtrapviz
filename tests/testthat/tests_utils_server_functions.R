# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: test server functions

library(testthat)


test_that("Find default colnames", {
  # One regex without match
  colnames <- c("species", "timestamp")
  regex <- c("^vernacularNames\\.en$|species", "station|deployment|camera",
             "timestamp|datetime")
  names(regex) <-  c("spp_col", "cam_col", "datetime_col")
  
  default <- find_default_colnames(regex_list = regex,
                                   colnames = colnames)
  expect_equal(default, list("spp_col" = "species", 
                             "cam_col" = NULL, 
                             "datetime_col" = "timestamp"))
  
  # Test 2
  df <- mica$data$observations
  regex <- c("^vernacularNames\\.en$|species", "station|deployment|camera",
             "date", "hour|time(?!stamp)", "lat|((^|[^[:alpha:]]+)x([^[:alpha:]]+|$))",
             "lon|((^|[^[:alpha:]]+)y([^[:alpha:]]+|$))", "count", "observationType")
  names(regex) <- c("spp_col", "cam_col", "date_col", "time_col",
                    "lat_col", "lon_col", "count_col", "obstype_col")
  
  default <- find_default_colnames(regex_list = regex,
                                   colnames = colnames(df))
  
})

test_that("Get cameras not in", {
  
  dfrecords <- data.frame(camrec = letters[1:10])
  dfcam <- data.frame(cameras = letters[1:10])
  
  # Test matching cameras
  res <- get_cameras_not_in(dfrecords = dfrecords, 
                            dfcameras = dfcam,
                            cam_col_dfrec = "camrec",
                            cam_col_dfcam = "cameras")
  expect_equal(res$not_in_records, character(0))
  expect_equal(res$not_in_cameras, character(0))
  
  # Missing in cameras
  dfcam2 <- dfcam |> filter(cameras != "a")
  res <- get_cameras_not_in(dfrecords = dfrecords, 
                            dfcameras = dfcam2,
                            cam_col_dfrec = "camrec",
                            cam_col_dfcam = "cameras")
  expect_equal(res$not_in_records, character(0))
  expect_equal(res$not_in_cameras, "a")
  
  # Missing in records
  dfrecords2 <- dfrecords |> filter(camrec != "a")
  res <- get_cameras_not_in(dfrecords = dfrecords2, 
                            dfcameras = dfcam,
                            cam_col_dfrec = "camrec",
                            cam_col_dfcam = "cameras")
  expect_equal(res$not_in_records, "a")
  expect_equal(res$not_in_cameras,  character(0))
  
  # Missing in both
  dfrecords2 <- dfrecords |> filter(camrec != "a")
  dfcam2 <- dfcam |> filter(!(cameras %in% c("b", "c")))
  res <- get_cameras_not_in(dfrecords = dfrecords2, 
                            dfcameras = dfcam2,
                            cam_col_dfrec = "camrec",
                            cam_col_dfcam = "cameras")
  expect_equal(res$not_in_records, "a")
  expect_equal(res$not_in_cameras,  c("b", "c"))
})

# Check empty or NULL -----------------------------------------------------

test_that("empty_or_null", {
  
  expect_true(empty_or_null(""))
  expect_true(empty_or_null(NULL))
  expect_false(empty_or_null("foo"))
  expect_false(empty_or_null(1))
})


# Timezone helpers ------------------------------------------------------------

test_that("Get timezone", {
  
  # Default if no other provided
  expect_equal(get_tz(custom_tz = NULL, data_tz = NULL),
               "Etc/GMT")
  expect_equal(get_tz(custom_tz = "", data_tz = NULL),
               "Etc/GMT")
  expect_equal(get_tz(custom_tz = NULL, data_tz = ""),
               "Etc/GMT")
  # Explicit default
  expect_equal(get_tz(custom_tz = NULL, data_tz = NULL, default_tz = "Etc/GMT+10"),
               "Etc/GMT+10")
  
  # Custom overrides default
  expect_equal(get_tz(custom_tz = "Etc/GMT+10", data_tz = NULL),
               "Etc/GMT+10")
  # Except if custom is empty
  expect_equal(get_tz(custom_tz = "", data_tz = NULL),
               "Etc/GMT")
  # Custom overrides data
  expect_equal(get_tz(custom_tz = "Etc/GMT+10", data_tz = "Etc/GMT+12"),
               "Etc/GMT+10")
  # Except if custom is empty
  expect_equal(get_tz(custom_tz = "", data_tz = "Etc/GMT+12"),
               "Etc/GMT+12")
  # Data saves the day
  expect_equal(get_tz(custom_tz = NULL, data_tz = "Etc/GMT+12"),
               "Etc/GMT+12")
  # Except if empty
  expect_equal(get_tz(custom_tz = NULL, data_tz = ""),
               "Etc/GMT")
})

test_that("Add timezone", {
  tz <- "Etc/GMT+10"
  
  # Date input
  dat <- as.Date(c("2023-01-01", "2023-01-02", "2023-12-12"))
  res <- add_tz(dat, tz = tz)
  expected <- as.POSIXct(dat, tz = tz)
  expect_equal(res, expected)
  
  # Character input
  char <- c("2023-01-01", "2023-01-02", "2023-12-12")
  res <- add_tz(char, tz = tz)
  expected <- as.POSIXct(char, tz = tz)
  expect_equal(res, expected)
  
  # POSIX input (has a timezone)
  posix <- as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 10:00:00", "2023-12-12 03:00:00"),
                      tz = "Etc/GMT")
  expect_equal(attr(posix, "tzone"), "Etc/GMT")
  
  res <- add_tz(posix, tz = tz)
  expected <- as.POSIXct(posix, tz = tz)
  expect_equal(res, expected)

  # POSIX input (has same tz)
  posix <- as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 10:00:00", "2023-12-12 03:00:00"),
                      tz = tz)
  expect_equal(attr(posix, "tzone"), tz)
  
  res <- add_tz(posix, tz = tz)
  expected <- as.POSIXct(posix, tz = tz)
  expect_equal(res, expected)
  
  # POSIX input (has no tz)
  posix <- as.POSIXct(c("2023-01-01 12:00:00", "2023-01-02 10:00:00", "2023-12-12 03:00:00"))
  expect_equal(attr(posix, "tzone"), "")
  
  expect_error(add_tz(posix, tz = tz),
               "Input POSIX vector has no timezone and it will not be modified unless force_tz is TRUE.",
               fixed = TRUE)
  
  # POSIX input (has no tz and force tz is TRUE)
  res <- add_tz(posix, tz = tz, force_tz = TRUE)
  expected <- force_tz(posix, tz = tz)
  expect_equal(res, expected)
})
