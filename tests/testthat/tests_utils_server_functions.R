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
  names(regex) <-  c("spp_col", "cam_col", "timestamp_col")
  
  default <- find_default_colnames(regex_list = regex,
                                   colnames = colnames)
  expect_equal(default, list("spp_col" = "species", 
                             "cam_col" = NULL, 
                             "timestamp_col" = "timestamp"))
  
  # Test 2
  df <- mica$data$observations
  regex <- c("^vernacularNames\\.en$|species", "station|deployment|camera",
             "date", "hour|time(?!stamp)", "lat|((^|[^[:alpha:]]+)x([^[:alpha:]]+|$))",
             "lon|((^|[^[:alpha:]]+)y([^[:alpha:]]+|$))", "count", "observationType")
  names(regex) <- c("spp_col", "cam_col", "date_col", "time_col",
                    "lat_col", "lon_col", "count_col", "obs_col")
  
  default <- find_default_colnames(regex_list = regex,
                                   colnames = colnames(df))
  
})

test_that("Get cameras not in", {
  
  dfrecords <- data.frame(camrec = letters[1:10])
  dfcam <- data.frame(cameras = letters[1:10])
  
  # Test matching cameras
  res <- get_cameras_not_in(dfrecords = dfrecords, 
                            dfcameras = dfcam,
                            cam_col_records = "camrec",
                            cam_col_cameras = "cameras")
  expect_equal(res$not_in_records, character(0))
  expect_equal(res$not_in_cameras, character(0))
  
  # Missing in cameras
  dfcam2 <- dfcam |> filter(cameras != "a")
  res <- get_cameras_not_in(dfrecords = dfrecords, 
                            dfcameras = dfcam2,
                            cam_col_records = "camrec",
                            cam_col_cameras = "cameras")
  expect_equal(res$not_in_records, character(0))
  expect_equal(res$not_in_cameras, "a")
  
  # Missing in records
  dfrecords2 <- dfrecords |> filter(camrec != "a")
  res <- get_cameras_not_in(dfrecords = dfrecords2, 
                            dfcameras = dfcam,
                            cam_col_records = "camrec",
                            cam_col_cameras = "cameras")
  expect_equal(res$not_in_records, "a")
  expect_equal(res$not_in_cameras,  character(0))
  
  # Missing in both
  dfrecords2 <- dfrecords |> filter(camrec != "a")
  dfcam2 <- dfcam |> filter(!(cameras %in% c("b", "c")))
  res <- get_cameras_not_in(dfrecords = dfrecords2, 
                            dfcameras = dfcam2,
                            cam_col_records = "camrec",
                            cam_col_cameras = "cameras")
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
