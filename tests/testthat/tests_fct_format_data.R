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
  
  # There should be an error
  expect_error(split_records_cameras(dat = dat, 
                                     cam_cols = cam_cols,
                                     cam_col = "CAM"), 
               "cam_col should be in cam_cols.", fixed = TRUE)
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
