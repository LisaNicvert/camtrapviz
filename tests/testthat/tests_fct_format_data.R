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


# Prepare cameras ---------------------------------------------------------
test_that("Prepare cameras", {
  # Split is TRUE
  cam_cols <- list(cam_col = "cameraID",
                   loc_col = "locationID",
                   elev_col = "elevation")
  
  dat <- list(data = list(deployments = NULL,
                          observations = kga_all))
  dat_clean <- prepare_cameras(dat = dat, 
                               split = TRUE,
                               cam_columns = cam_cols,
                               cam_col = "cameraID")
  expect_equal(ncol(dat_clean$data$observations), ncol(dat$data$observations) - length(cam_cols) + 1)
  expect_equal(ncol(dat_clean$data$deployments), length(cam_cols))
  
  expect_equal(colnames(dat_clean$data$deployments), unname(unlist(cam_cols)))
})

test_that("Prepare cameras (no split)", {
  # Fake obs
  obs <- data.frame(letters = letters[1:10])
  # Duplicated camera
  dep <- data.frame(cam = paste0("cam", c(LETTERS[1:5], LETTERS[5])),
                    lat = c(1:5, 5),
                    lon = c(11:15, 15))
  
  dat <- list(data = list(deployments = dep,
                          observations = obs))
  
  dat_clean <- prepare_cameras(dat = dat, 
                               split = FALSE)
  
  # obs should stay the same
  expect_equal(dat_clean$data$observations, dat$data$observations)
  
  # A camera should be removed from dep
  expect_equal(dat_clean$data$deployments$cam, paste0("cam", LETTERS[1:5]))
  expect_equal(dat_clean$data$deployments$lat, 1:5)
  expect_equal(dat_clean$data$deployments$lon, 11:15)
})



test_that("Prepare cameras (error)", {
  obs <- data.frame(cam = paste0("cam", LETTERS[1:5]),
                    date = rep(Sys.time(), 5),
                    lat = 1:5,
                    lon = 11:15)
  
  dat <- list(data = list(deployments = NULL,
                          observations = obs))
  
  cam_cols <- c("cam", "lat", "lon")
  
  # Threr should be an error
  expect_error(prepare_cameras(dat = dat, 
                               cam_columns = cam_cols,
                               split = TRUE, cam_col = "CAM"), 
               "cam_col should be in cam_columns.", fixed = TRUE)
  
  # No error expected because no split
  obs <- data.frame(cam = paste0("cam", LETTERS[1:5]),
                    date = rep(Sys.time(), 5))
  dep <- data.frame(cam = paste0("cam", LETTERS[1:5]),
                    lat = 1:5,
                    lon = 11:15)
  dat <- list(data = list(deployments = dep,
                          observations = obs))
  
  expect_no_error(prepare_cameras(dat = dat, 
                               cam_columns = cam_cols,
                               split = FALSE, cam_col = "CAM"))
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
  cast_cam <- list(cameraID = "as.character",
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
                          cam_col_records = "cameraID")
  # Test split
  expect_equal(ncol(dat_clean$data$deployments), 3)
  expect_equal(ncol(dat_clean$data$observations), ncol(dat$data$observations) - 2)
  
  # Test classes for records
  expect_equal(class(dat_clean$data$observations$snapshotName), "character")
  expect_equal(class(dat_clean$data$observations$cameraID), "character")
  expect_equal(class(dat_clean$data$observations$eventDate), "Date")
  expect_equal(class(dat_clean$data$observations$eventTime), "times")
  # Test classes for cameras
  expect_equal(class(dat_clean$data$deployments$cameraID), "character")
  expect_equal(class(dat_clean$data$deployments$Lat_Y), "numeric")
  expect_equal(class(dat_clean$data$deployments$Long_X), "numeric")
})
