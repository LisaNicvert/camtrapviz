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


# Prepare data ------------------------------------------------------------

test_that("Format data", {
  
  # Normal case
  mapping <- list(spp_col = "Species",
                  cam_col = "Station",
                  date_col = NULL,
                  time_col = NULL,
                  timestamp_col = "DateTimeOriginal")
  cast_type <- list(spp_col = "as.character",
                    cam_col = "as.character",
                    date_col = "as_datetime",
                    time_col = "chron::time",
                    timestamp_col = "as_datetime")
  
  df <- format_table(recordTableSample, mapping, cast_type)
  expect_equal(class(df$Species), "character")
  expect_equal(class(df$Station), "character")
  expect_equal(class(df$DateTimeOriginal), c("POSIXct", "POSIXt"))
  
  # NULL mapping has no corresponding regex
  cast_type <- list(spp_col = "as.character",
                    cam_col = "as.character",
                    timestamp_col = "as_datetime")
  
  df <- format_table(recordTableSample, mapping, cast_type)
  expect_equal(class(df$Species), "character")
  expect_equal(class(df$Station), "character")
  expect_equal(class(df$DateTimeOriginal), c("POSIXct", "POSIXt"))
  
  # Mapping has more elements
  mapping <- list(spp_col = "Species",
                  cam_col = "Station")
  cast_type <- list(spp_col =  "as.character")
  
  expect_error(format_table(recordTableSample, mapping, cast_type),
               "all non-null columns listed in mapping must be in cast_type", 
               fixed = TRUE)
})

# Final function ----------------------------------------------------------

test_that("Clean data", {
  
  # Normal case
  mapping_cam <- list(cam_col = "Station",
                      lat_col = "utm_y",
                      lon_col = "utm_x",
                      setup_col = NULL,
                      retrieval_col = NULL)
  cast_cam <- list(cam_col = "as.character",
                   lat_col = "as.numeric",
                   lon_col = "as.numeric")
  
  mapping_rec <- list(spp_col = "Species",
                      cam_col = "Station",
                      date_col = NULL,
                      time_col = NULL,
                      timestamp_col = "DateTimeOriginal")
  cast_rec <- list(spp_col = "as.character",
                   cam_col = "as.character",
                   date_col = "as_datetime",
                   timestamp_col = "as_datetime")
  
  dat <- list(data = list(deployments = camtraps,
                          observations = recordTableSample))
  dat_clean <- clean_data(dat = dat, 
                          mapping_cameras = mapping_cam,
                          cam_type =  cast_cam,
                          mapping_records = mapping_rec, 
                          rec_type = cast_rec,
                          split = FALSE)
  
  expect_equal(class(dat_clean$data$observations$Species), "character")
  expect_equal(class(dat_clean$data$observations$Station), "character")
  expect_equal(class(dat_clean$data$observations$DateTimeOriginal), c("POSIXct", "POSIXt"))
  
  expect_equal(class(dat_clean$data$deployments$Station), "character")
  expect_equal(class(dat_clean$data$deployments$utm_y), "numeric")
  expect_equal(class(dat_clean$data$deployments$utm_x), "numeric")
})