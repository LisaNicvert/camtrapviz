# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-25
#
# Script Description: tests for data wrangling function


library(testthat)


test_that("Get cameras", {
  # Normal test
  cam1 <- c("C1", "C2", "C3")
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4"))
  
  # NA
  cam1 <- c("C1", "C2", "C3", NA)
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4", NA))
  
  # Duplicates
  cam1 <- c("C1", "C1", "C2", "C3")
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4"))
  
  # Reorder
  cam1 <- c("C2", "C3", "C1")
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4"))
})

test_that("Get nspecies", {
  # With obstype
  df <- data.frame(obstype = c("animal", "animal", "animal", "animal", "blank"),
                   species = c("cat", "cat", "cow", "dog", NA))
  n <- get_nspecies(df, species_col = "species", obs_col = "obstype")
  
  expect_equal(n, 3)
  
  # Without obstype
  n <- get_nspecies(df, species_col = "species", keep_NA = TRUE)
  expect_equal(n, 4)
  
  # Without obstype and rm NA
  n <- get_nspecies(df, species_col = "species", keep_NA = FALSE)
  expect_equal(n, 3)
})

# Summarize cameras -------------------------------------------------------


test_that("Summarize cameras raises errors", {
  expect_error(summarize_cameras(mica$data$observations, 
                                 cam_col = "deploymentID"),
               "If timestamp_col is not specified or NULL, both date_col and time_col must be provided.")
  
  expect_error(summarize_cameras(mica$data$observations, 
                                 cam_col = "deploymentID",
                                 timestamp_col = NULL),
               "If timestamp_col is not specified or NULL, both date_col and time_col must be provided.")
  
  expect_error(summarize_cameras(recordTableSample, 
                                 cam_col = "deploymentID",
                                 date_col = "Date"),
               "If timestamp_col is not specified or NULL, both date_col and time_col must be provided.")
  
  expect_message(summarize_cameras(recordTableSample, 
                                   cam_col = "Station",
                                   timestamp_col = "DateTimeOriginal",
                                   date_col = "Date"),
                 "timestamp_col is provided, so date_col and time_col will be ignored.")
  
  expect_error(summarize_cameras(recordTableSample, 
                                 cam_col = "Station",
                                 timestamp_col = "DateTimeOriginal",
                                 dfcam = camtraps,
                                 cam_col_dfcam = "Station"),
               "If dfcam is not NULL, then setup_col or retrieval_col must be provided.")
  
})

test_that("Summarize cameras (no dfcam)", {
  expected_colnames <- c("deploymentID", "setup", "retrieval",
                         "setup_origin", "retrieval_origin",
                         "sampling_length")
  
  res <- summarize_cameras(mica$data$observations, 
                           cam_col = "deploymentID", 
                           timestamp_col = "timestamp")
  
  expect_equal(colnames(res), expected_colnames)
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("picture", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
  # Test duration
  len <- mica$data$observations |> group_by(deploymentID) |>
    summarize(len = as.numeric(max(timestamp) - min(timestamp), "days"))
  expect_equal(res$sampling_length, len$len, tolerance = 10e-3)
  
  # With date/time
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate")
  
  expected_colnames <- c("cameraID", "setup", "retrieval",
                         "setup_origin", "retrieval_origin",
                         "sampling_length")
  expect_equal(colnames(res), expected_colnames)
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("picture", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
  len <- kga |> group_by(cameraID) |>
    mutate(timestamp = as.POSIXct(paste(eventDate, eventTime))) |>
    summarize(len = as.numeric(max(timestamp) - min(timestamp), "days"))
  expect_equal(res$sampling_length, len$len, tolerance = 10e-3)
})

test_that("Summarize cameras with camera df", {
  # No NAs
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expected_colnames <- c("cameraID", "setup", "retrieval",
                         "setup_origin", "retrieval_origin",
                         "sampling_length")
  expect_equal(colnames(res), expected_colnames)
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("metadata", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
  # Test duration
  setup <- kga_cameras$Setup.Date[kga_cameras$cameraID == "KGA_A02"]
  retrieval <- kga[kga$cameraID == "KGA_A02", ]
  retrieval <- max(as.POSIXct(paste(retrieval$eventDate, retrieval$eventTime)))
  len <- as.numeric(retrieval - as.POSIXct(setup), "days")
  expect_equal(res$sampling_length[res$cameraID == "KGA_A02"], len, 
               tolerance = 10e-3)
  
})

test_that("Summarize cameras with different cameras name", {
  kga_test <- kga |> rename("cam" = "cameraID")
  res <- summarize_cameras(kga_test, 
                           cam_col = "cam", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expect_equal(colnames(res)[1], "cam")
})

test_that("Summarize cameras with NA in setup/retrieval", {
  
  # NA in setup
  kga_cameras_test <- kga_cameras
  kga_cameras_test$Setup.Date[kga_cameras_test$cameraID == "KGA_A02"] <- NA
  
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras_test,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expected <- kga |> filter(cameraID == "KGA_A02") |> 
    mutate(dtime = as.POSIXct(paste(eventDate, eventTime))) |>
    summarise(mintime = min(dtime))
  expected <- expected$mintime
  
  expect_equal(res$setup[res$cameraID == "KGA_A02"], expected)
  
  # Missing camera in setup
  kga_cameras_test <- kga_cameras |> filter(cameraID != "KGA_A02")
  
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras_test,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expect_equal(res$setup[res$cameraID == "KGA_A02"], expected)
  
  # Missing camera in data
  kga_test <- kga |> filter(cameraID != "KGA_A02")
  res <- summarize_cameras(kga_test, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expected <- as.POSIXct(kga_cameras$Setup.Date[kga_cameras$cameraID == "KGA_A02"])
  expect_equal(res$setup[res$cameraID == "KGA_A02"], expected)
  expect_true(is.na(res$retrieval[res$cameraID == "KGA_A02"]))
  
  # Missing camera in data and in setup
  kga_test2 <- kga |> filter(cameraID != "KGA_A02")
  kga_cameras_test <- kga_cameras |> filter(cameraID != "KGA_A03")
  res <- summarize_cameras(kga_test2, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras_test,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expected <- kga_test2 |> filter(cameraID == "KGA_A03") |> 
    mutate(dtime = as.POSIXct(paste(eventDate, eventTime))) |>
    summarise(mintime = min(dtime),
              maxtime = max(dtime))
  expect_equal(res$setup[res$cameraID == "KGA_A03"], expected$mintime)
  expect_equal(res$retrieval[res$cameraID == "KGA_A03"], expected$maxtime)
  
  expected <- as.POSIXct(kga_cameras$Setup.Date[kga_cameras$cameraID == "KGA_A02"])
  expect_equal(res$setup[res$cameraID == "KGA_A02"], expected)
  expect_true(is.na(res$retrieval[res$cameraID == "KGA_A02"]))
  
})

test_that("Summarize cameras with a camera with one obs", {
  # Only one camera
  df <- data.frame(species = "cat",
                   stamp = as.POSIXct("2020-04-01 12:00:00"),
                   camera = "A01")
  
  res <- summarize_cameras(df, 
                           timestamp_col = "stamp",
                           cam_col = "camera")
  expected <- data.frame(camera = "A01",
                         setup = as.POSIXct("2020-04-01 12:00:00"),
                         retrieval = as.POSIXct("2020-04-01 12:00:00"),
                         setup_origin = "picture",
                         retrieval_origin = "picture",
                         sampling_length = 0)
  expect_equal(res, expected)
  
  # Two cameras
  df <- data.frame(species = c("cat", "cow", "dog"),
                   stamp = as.POSIXct(c("2020-04-01 12:00:00",
                                        "2020-04-07 12:00:00",
                                        "2020-04-12 12:00:00")),
                   camera = c("A01", "A02", "A02"))
  
  res <- summarize_cameras(df, 
                           timestamp_col = "stamp",
                           cam_col = "camera")
  expected <- data.frame(camera = c("A01", "A02"),
                         setup = as.POSIXct(c("2020-04-01 12:00:00",
                                              "2020-04-07 12:00:00")),
                         retrieval = as.POSIXct(c("2020-04-01 12:00:00",
                                                  "2020-04-12 12:00:00")),
                         setup_origin = c("picture", "picture"),
                         retrieval_origin = c("picture", "picture"),
                         sampling_length = c(0, 5))
  expect_equal(res, expected)
  
  # Two pictures with same date
  df <- data.frame(species = c("cat", "cow"),
                   stamp = as.POSIXct(c("2020-04-01 12:00:00",
                                        "2020-04-01 12:00:00")),
                   camera = c("A01", "A01"))
  
  res <- summarize_cameras(df, 
                           timestamp_col = "stamp",
                           cam_col = "camera")
  expected <- data.frame(camera = "A01",
                         setup = as.POSIXct("2020-04-01 12:00:00"),
                         retrieval = as.POSIXct("2020-04-01 12:00:00"),
                         setup_origin = "picture",
                         retrieval_origin = "picture",
                         sampling_length = 0)
  expect_equal(res, expected)
})
