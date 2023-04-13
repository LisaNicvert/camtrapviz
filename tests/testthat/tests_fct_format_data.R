# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: tests format data functions

library(testthat)

# Cast --------------------------------------------------------------------

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

test_that("add_tryformats", {
  # Normal case
  cast <- list(num = "as.character",
               date = "as.Date",
               date2 = "as.Date")
  
  fmt <- c("%Y-%m-%d", "%d-%m-%Y")
  res <- add_tryformats(cast, 
                        formats = fmt, 
                        names_to_add = c("date", "date2"))
  
  expect_equal(res$date, list("as.Date",
                              tryFormats = fmt))
  expect_equal(res$date2, list("as.Date",
                               tryFormats = fmt))
  
  # NULL
  cast <- list(num = "as.character",
               date = NULL)
  
  fmt <- c("%Y-%m-%d", "%d-%m-%Y")
  res <- add_tryformats(cast, 
                        formats = fmt, 
                        names_to_add = "date")
  
  expect_true(is.null(res$date))
})

# Final function ----------------------------------------------------------

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
  
  # Split is TRUE
  dat <- list(data = list(deployments = NULL,
                          observations = kga_all))
  cast_cam <- list(locationID = "as.character",
                   Lat_Y = "as.numeric",
                   Long_X = "as.numeric")
  cast_rec <- list(snapshotName = "as.character",
                   locationID = "as.character",
                   eventDate = "as.Date",
                   eventTime = "times")
  dat_clean <- clean_data(dat = dat, 
                          cam_type =  cast_cam,
                          rec_type = cast_rec,
                          split = TRUE,
                          cam_col_records = "locationID")
  # Test split
  expect_equal(ncol(dat_clean$data$deployments), 3)
  # Test classes for records
  expect_equal(class(dat_clean$data$observations$snapshotName), "character")
  expect_equal(class(dat_clean$data$observations$locationID), "character")
  expect_equal(class(dat_clean$data$observations$eventDate), "Date")
  expect_equal(class(dat_clean$data$observations$eventTime), "times")
  # Test classes for cameras
  expect_equal(class(dat_clean$data$deployments$locationID), "character")
  expect_equal(class(dat_clean$data$deployments$Lat_Y), "numeric")
  expect_equal(class(dat_clean$data$deployments$Long_X), "numeric")
})
