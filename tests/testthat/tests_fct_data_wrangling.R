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
  
})

test_that("Summarize cameras (no dfcam)", {
  expected_colnames <- c("deploymentID", "pictures", "sampling_length", 
                         "setup", "retrieval",
                         "setup_origin", "retrieval_origin")
  
  res <- summarize_cameras(mica$data$observations, 
                           cam_col = "deploymentID", 
                           timestamp_col = "timestamp")
  
  expect_equal(colnames(res), expected_colnames)
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("picture", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
  # Test npic
  npic <- mica$data$observations |> 
    group_by(deploymentID) |> 
    summarise(n = n())
  expect_equal(npic$n, res$pictures)
  # Test duration
  len <- mica$data$observations |> group_by(deploymentID) |>
    summarize(len = as.numeric(max(timestamp) - min(timestamp), "days"))
  expect_equal(res$sampling_length, len$len, tolerance = 10e-3)
  
  # With date/time
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate")
  
  expected_colnames <- c("cameraID", "pictures", "sampling_length", 
                         "setup", "retrieval",
                         "setup_origin", "retrieval_origin")
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
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  
  expected_colnames <- c("cameraID", "pictures", "sampling_length", 
                         "setup", "retrieval",
                         "setup_origin", "retrieval_origin")
  expect_equal(colnames(res), expected_colnames)
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("metadata", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
  # Test npic
  npic <- kga |> 
    group_by(cameraID) |> 
    summarise(n = n())
  expect_equal(npic$n, res$pictures)
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
    mutate(dtime = as.POSIXct(paste(eventDate, eventTime),
                              tz = "UTC")) |>
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
  expected <- as.POSIXct(kga_cameras$Setup.Date[kga_cameras$cameraID == "KGA_A02"],
                         tz = "UTC")
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
    mutate(dtime = as.POSIXct(paste(eventDate, eventTime),
                              tz = "UTC")) |>
    summarise(mintime = min(dtime),
              maxtime = max(dtime))
  expect_equal(res$setup[res$cameraID == "KGA_A03"], expected$mintime)
  expect_equal(res$retrieval[res$cameraID == "KGA_A03"], expected$maxtime)
  
  expected <- as.POSIXct(kga_cameras$Setup.Date[kga_cameras$cameraID == "KGA_A02"],
                         tz = "UTC")
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
                         pictures = 1,
                         sampling_length = 0,
                         setup = as.POSIXct("2020-04-01 12:00:00"),
                         retrieval = as.POSIXct("2020-04-01 12:00:00"),
                         setup_origin = "picture",
                         retrieval_origin = "picture")
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
                         pictures = c(1, 2),
                         sampling_length = c(0, 5),
                         setup = as.POSIXct(c("2020-04-01 12:00:00",
                                              "2020-04-07 12:00:00")),
                         retrieval = as.POSIXct(c("2020-04-01 12:00:00",
                                                  "2020-04-12 12:00:00")),
                         setup_origin = c("picture", "picture"),
                         retrieval_origin = c("picture", "picture"))
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
                         pictures = 2,
                         sampling_length = 0,
                         setup = as.POSIXct("2020-04-01 12:00:00"),
                         retrieval = as.POSIXct("2020-04-01 12:00:00"),
                         setup_origin = "picture",
                         retrieval_origin = "picture")
  expect_equal(res, expected)
})

test_that("Summarize cameras (dfcam with no setup or retrieval)", {
  # Create dfs
  df <- data.frame(species = c("cat", "cow", "dog", "rabbit"),
                   stamp = as.POSIXct(c("2020-04-01 12:00:00",
                                        "2020-04-02 12:00:00",
                                        "2020-04-11 12:00:00",
                                        "2020-04-12 12:00:00")),
                   camera = c("A01", "A01", "A02", "A02"))
  dfcam <- data.frame(camera = c("A01", "A02", "A03"))
  
  # Give cam_col_dfcam
  res <- summarize_cameras(df, 
                           timestamp_col = "stamp",
                           cam_col = "camera",
                           dfcam = dfcam,
                           cam_col_dfcam = "camera")
  
  expected <- data.frame(camera = c("A01", "A02", "A03"),
                         pictures = c(2, 2, 0),
                         sampling_length = c(1, 1, NA),
                         setup = as.POSIXct(c("2020-04-01 12:00:00",
                                              "2020-04-11 12:00:00",
                                              NA)),
                         retrieval = as.POSIXct(c("2020-04-02 12:00:00",
                                                  "2020-04-12 12:00:00",
                                                  NA)),
                         setup_origin = c("picture", "picture", NA),
                         retrieval_origin = c("picture", "picture", NA))
  expect_equal(res, expected)
  
  # Don't give cam_col_dfcam
  res <- summarize_cameras(df, 
                           timestamp_col = "stamp",
                           cam_col = "camera",
                           dfcam = dfcam)
  expect_equal(res, expected)
})


test_that("Summarize cameras with species col", {
  # Create dfs
  df <- data.frame(species = c("cat", "cow", "dog", "dog"),
                   stamp = as.POSIXct(c("2020-04-01 12:00:00",
                                        "2020-04-02 12:00:00",
                                        "2020-04-11 12:00:00",
                                        "2020-04-12 12:00:00")),
                   camera = c("A01", "A01", "A02", "A02"))
  dfcam <- data.frame(camera = c("A01", "A02", "A03"))
  
  # Expected
  expected <- data.frame(camera = c("A01", "A02", "A03"),
                         pictures = c(2, 2, 0),
                         species = c(2, 1, NA),
                         sampling_length = c(1, 1, NA),
                         setup = as.POSIXct(c("2020-04-01 12:00:00",
                                              "2020-04-11 12:00:00",
                                              NA)),
                         retrieval = as.POSIXct(c("2020-04-02 12:00:00",
                                                  "2020-04-12 12:00:00",
                                                  NA)),
                         setup_origin = c("picture", "picture", NA),
                         retrieval_origin = c("picture", "picture", NA))
  expected_nodfcam <- expected |> 
    filter(camera != "A03")
  
  # Test without dfcam
  res <- summarize_cameras(df,
                           cam_col = "camera",
                           timestamp_col = "stamp",
                           spp_col = "species")
  
  expect_equal(res, expected_nodfcam)
  
  # Test with dfcam (one more camera)
  res <- summarize_cameras(df,
                           cam_col = "camera",
                           timestamp_col = "stamp",
                           spp_col = "species",
                           dfcam = dfcam)
  
  expect_equal(res, expected)
  
  # Test with dfcam (one more camera in records)
  dfcam2 <- data.frame(camera = "A01")
  
  res <- summarize_cameras(df,
                           cam_col = "camera",
                           timestamp_col = "stamp",
                           spp_col = "species",
                           dfcam = dfcam2)
  
  expect_equal(res, expected_nodfcam)
})

test_that("Summarize cameras with spp_col and obs_type", {
  # Create dfs
  df <- data.frame(species = c("cat", "cow", NA, NA),
                   type = c("animal", "animal", "human", "blank"),
                   stamp = as.POSIXct(c("2020-04-01 12:00:00",
                                        "2020-04-02 12:00:00",
                                        "2020-04-11 12:00:00",
                                        "2020-04-12 12:00:00")),
                   camera = c("A01", "A01", "A02", "A02"))
  dfcam <- data.frame(camera = c("A01", "A02", "A03"))
  
  # Expected
  expected <- data.frame(camera = c("A01", "A02", "A03"),
                         pictures = c(2, 2, 0),
                         species = c(2, 2, NA),
                         sampling_length = c(1, 1, NA),
                         setup = as.POSIXct(c("2020-04-01 12:00:00",
                                              "2020-04-11 12:00:00",
                                              NA)),
                         retrieval = as.POSIXct(c("2020-04-02 12:00:00",
                                                  "2020-04-12 12:00:00",
                                                  NA)),
                         setup_origin = c("picture", "picture", NA),
                         retrieval_origin = c("picture", "picture", NA))
  expected_nodfcam <- expected |> 
    filter(camera != "A03")
  
  # Test without dfcam
  res <- summarize_cameras(df,
                           cam_col = "camera",
                           timestamp_col = "stamp",
                           spp_col = "species",
                           obs_col = "type")
  
  expect_equal(res, expected_nodfcam)
  
  # Test with dfcam
  res <- summarize_cameras(df,
                           cam_col = "camera",
                           timestamp_col = "stamp",
                           spp_col = "species",
                           obs_col = "type",
                           dfcam = dfcam)
  
  expect_equal(res, expected)
  
  # Test without obstype
  res <- summarize_cameras(df,
                           cam_col = "camera",
                           timestamp_col = "stamp",
                           spp_col = "species",
                           dfcam = dfcam)
  expected_notype <- expected |> 
    mutate(species = c(2, 1, NA))
  expect_equal(res, expected_notype)
  
})

# Summarize species -------------------------------------------------------

test_that("Get all species", {
  # Initialize data
  df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, "cameratrapper", "tourist"),
                   type = c("animal", "animal", "animal", "fire", "blank", "human", "human"))
  
  # With obs_type
  res <- get_all_species(df, spp_col = "species", obs_col = "type")
  expected <- df
  expected$species[expected$type == "fire"] <- "fire"
  expected$species[expected$type == "blank"] <- "blank"
  expect_equal(res, expected)
  
  # With obs_type but return vector
  res <- get_all_species(df, spp_col = "species", obs_col = "type",
                         return_df = FALSE)
  expect_equal(res, expected$species)
  
  # Without obs_type
  res <- get_all_species(df, spp_col = "species")
  expect_equal(res, df$species)
  
  # Without obs_type but return df
  res <- get_all_species(df, spp_col = "species",
                         return_df = TRUE)
  expect_equal(res, df |> select(species))
})

test_that("Get unique species", {
  
  # Initialize data
  df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, "cameratrapper", "tourist"),
                   type = c("animal", "animal", "animal", "fire", "blank", "human", "human"))
  
  # No observation type and don't reorder
  res <- get_unique_species(df, spp_col = "species")
  expected <- unique(df$species)
  expect_equal(res, expected)
  
  # No observation type, reorder and return character df
  res <- get_unique_species(df, spp_col = "species", 
                            reorder = TRUE,
                            return_df = TRUE)
  expected <- data.frame(species = c("cameratrapper", "cat", "rabbit", "tourist", NA))
  expected <- expected |> 
    mutate(ID = species, .before = 1)
  expect_equal(res, expected)
  
  # No observation type and reorder
  res <- get_unique_species(df, spp_col = "species", 
                            reorder = TRUE)
  expected_char <- expected$species
  expect_equal(res, expected_char)
  
  # With observation type
  res <- get_unique_species(df, 
                            spp_col = "species", obs_col = "type",
                            reorder = TRUE)
  expected <- data.frame(species = c("cat", "rabbit",
                                     "blank", "fire", "cameratrapper", "tourist"),
                         type = c("animal", "animal",
                                  "blank", "fire", "human", "human"))
  expected <- expected |> 
    mutate(ID = paste(species, type, sep = "_"), 
           .before = 1)
  expect_equal(res, expected)
  
  # With observation type and return character
  res <- get_unique_species(df, 
                            spp_col = "species", obs_col = "type",
                            return_df = FALSE,
                            reorder = TRUE)
  expected_char <- expected$species
  expect_equal(res, expected_char)
  
  # Factor (and return df)
  df_fac <- df
  df_fac$species <- factor(df_fac$species)
  df_fac$type <- factor(df_fac$type)
  res <- get_unique_species(df_fac, 
                            spp_col = "species", obs_col = "type",
                            reorder = TRUE)
  expect_equal(class(df_fac$species), "factor")
  expect_equal(class(df_fac$type), "factor")
  expect_equal(res, expected)
  
  # Factor (and return vector)
  res <- get_unique_species(df_fac, 
                            spp_col = "species", obs_col = "type", 
                            return_df = FALSE,
                            reorder = TRUE)
  expect_equal(class(df_fac$species), "factor")
  expect_equal(class(df_fac$type), "factor")
  expected <- c("cat", "rabbit", "blank", "fire", "cameratrapper", "tourist")
  expect_equal(res, expected)
  
  # NA in obstype
  df <- data.frame(species = c("cat", "human", NA, NA),
                   type = c("animal", "human", "blank", NA))
  res <- get_unique_species(df, 
                            spp_col = "species", obs_col = "type",
                            reorder = TRUE)
  expected <- data.frame(species = c("cat", "blank", "human", NA),
                         type = c("animal", "blank", "human", NA))
  expected <- expected |> 
    mutate(ID = paste(species, type, sep = "_"), 
           .before = 1)
  expect_equal(res, expected)
  
})

test_that("Get species (obstype but no other than animal)", {
  df <- data.frame(species = c("rabbit", "cat", "cat", "cow"),
                   type = rep("animal", 4))
  res <- get_unique_species(df, 
                     spp_col = "species", obs_col = "type",
                     reorder = TRUE)
  
  expected <- data.frame(species = c("cat", "cow", "rabbit"),
                         type = rep("animal", 3))
  expected <- expected |> 
    mutate(ID = paste(species, type, sep = "_"), 
           .before = 1)
  expect_equal(res, expected)
})


test_that("Summarize species", {
  df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", "rabbit", NA, NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "animal", "human", "blank"),
                   camera = c("C1", "C1", "C1", "C1", "C2", "C3", "C3", "C4"),
                   count = c(1, 1, 1, 50, 3, 4, 1, NA))
  
  # Without obstype ---
  res <- summarize_species(df, 
                           spp_col = "species", 
                           cam_col = "camera")
  
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA),
                         sightings = c(2, 2, 1, 1, 2),
                         individuals = c(2, 2, 1, 1, 2),
                         n_cameras = c(1, 2, 1, 1, 2))
  expected$prop_cam <- expected$n_cameras/4
  expect_equal(res, expected)
  
  # With obstype ---
  res <- summarize_species(df, 
                           spp_col = "species", cam_col = "camera",
                           obs_col = "type")
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA, NA),
                         type = c("animal", "animal", "animal", "animal", "blank", "human"),
                         sightings = c(2, 2, 1, 1, 1, 1),
                         individuals = c(2, 2, 1, 1, 1, 1),
                         n_cameras = c(1, 2, 1, 1, 1, 1))
  expected$prop_cam <- expected$n_cameras/4
  expect_equal(res, expected)
  
  # With count ---
  res <- suppressWarnings(summarize_species(df, 
                                            spp_col = "species", 
                                            cam_col = "camera",
                                            obs_col = "type",
                                            count_col = "count"))
  expected$individuals <- c(2, 53, 4, 1, NA, 1)
  expected$prop_cam <- expected$n_cameras/4
  expect_equal(res, expected)
  
  # With ncam ---
  res <- suppressWarnings(summarize_species(df, 
                                            spp_col = "species", 
                                            cam_col = "camera",
                                            obs_col = "type",
                                            count_col = "count",
                                            ncam = 50))
  expected$prop_cam <- expected$n_cameras/50
  expect_equal(res, expected)
  
})

test_that("Summarize species throws a warning", {
  df <- data.frame(species = rep("cat", 10),
                   camera = 1:10)
  expect_warning(summarize_species(df, 
                                   spp_col = "species", 
                                   cam_col = "camera",
                                   ncam = 3),
                 "ncam is smaller than the number of cameras in df: are you sure it's what you want?", 
                 fixed = TRUE)
})

test_that("Summarize species with NA in count",{
  df <- data.frame(species = rep("cat", 5),
                   camera = 1:5,
                   count = c(1, 1, 3, NA, 1))
  
  # Without NA replacement ---
  # Check warning
  expect_warning(summarize_species(df, 
                                   spp_col = "species", 
                                   cam_col = "camera",
                                   count_col = "count"),
                 "There are NAs in the count column; if you want to replace them with a value, use NA_count_placeholder.",
                 fixed = TRUE)
  # Check value
  res <- suppressWarnings(summarize_species(df, 
                                            spp_col = "species", 
                                            cam_col = "camera",
                                            count_col = "count"))
  expect_true(is.na(res$individuals))
  
  # With NA replacement ---
  res <- summarize_species(df, 
                           spp_col = "species", 
                           cam_col = "camera",
                           count_col = "count",
                           NA_count_placeholder = 1)
  expect_equal(res$individuals, 7)
})

test_that("Summarize species without camera", {
  df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", "rabbit", NA, NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "animal", "human", "blank"),
                   count = c(1, 1, 1, 50, 3, 4, 1, NA))
  
  # Without obstype ---
  res <- summarize_species(df, spp_col = "species")
  
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA),
                         sightings = c(2, 2, 1, 1, 2),
                         individuals = c(2, 2, 1, 1, 2))
  expect_equal(res, expected)
  
  # With obstype ---
  res <- summarize_species(df, 
                           spp_col = "species",
                           obs_col = "type")
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA, NA),
                         type = c("animal", "animal", "animal", "animal", "blank", "human"),
                         sightings = c(2, 2, 1, 1, 1, 1),
                         individuals = c(2, 2, 1, 1, 1, 1))
  expect_equal(res, expected)
  
  # With count ---
  res <- suppressWarnings(summarize_species(df, 
                                            spp_col = "species",
                                            obs_col = "type",
                                            count_col = "count"))
  expected$individuals <- c(2, 53, 4, 1, NA, 1)
  expect_equal(res, expected)
})


test_that("Summarize species per camera too", {
  # Synthetic data ---
  df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", NA, NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "human", "blank"),
                   camera = c("C1", "C1", "C1", "C1", "C2", "C3", "C4"),
                   count = c(1, 1, 1, 50, 3, 1, NA))
  
  # Check error message ---
  expect_error(summarize_species(df, 
                                 spp_col = "species",
                                 by_cam = TRUE),
               "cam_col must be provided when by_cam is TRUE.")
  
  # Simplest case ---
  tab <- summarize_species(df,
                           cam_col = "camera",
                           spp_col = "species",
                           by_cam = TRUE)
  expected <- data.frame(species = c("cat", 
                                     "cow", "cow", 
                                     "zebra",
                                     NA, NA),
                         camera = c("C1", 
                                    "C1", "C2", 
                                    "C1", 
                                    "C3", "C4"),
                         sightings = c(2, 
                                       1, 1, 
                                       1, 
                                       1, 1),
                         individuals = c(2, 
                                         1, 1, 
                                         1, 
                                         1, 1),
                         sightings_prop = c(0.5, 
                                            0.25, 1,
                                            0.25, 
                                            1, 1),
                         individuals_prop = c(0.5, 
                                              0.25, 1,
                                              0.25, 
                                              1, 1))
  expect_equal(tab, expected)
  
  # Add count ---
  expect_warning(summarize_species(df,
                                   cam_col = "camera",
                                   spp_col = "species",
                                   count_col = "count",
                                   by_cam = TRUE),
                 "There are NAs in the count column; if you want to replace them with a value, use NA_count_placeholder.",
                 fixed = TRUE)
  
  tab <- suppressWarnings(summarize_species(df,
                                            cam_col = "camera",
                                            spp_col = "species",
                                            count_col = "count",
                                            by_cam = TRUE))
  expected2 <- expected
  expected2$individuals <- c(2, 
                             50, 3, 
                             1, 
                             1, NA)
  expected2$individuals_prop <- c(2/53,
                                  50/53, 1, 
                                  1/53, 
                                  1, NA)
  expect_equal(tab, expected2)
  
  # Add missing camera ---
  dflevels <- df
  camlevels <- c("C5", "C1", "C2", "C3", "C4")
  dflevels$camera <- factor(dflevels$camera, 
                            levels = camlevels)
  
  tab <- suppressWarnings(summarize_species(dflevels,
                                            cam_col = "camera",
                                            spp_col = "species",
                                            count_col = "count", 
                                            by_cam = TRUE,
                                            keep_all_camera_levels = TRUE))
  
  newcol <- data.frame(species = NA, 
                       camera = "C5", 
                       sightings = NA, individuals = NA,
                       sightings_prop = NA, individuals_prop = NA,
                       empty = TRUE)
  expected3 <- expected2 |> mutate(empty = FALSE)
  expected3 <- rbind(expected3[1:4, ], newcol, expected3[5:6, ])
  expected3$camera <- factor(expected3$camera,
                             levels = camlevels)
  rownames(expected3) <- NULL
  expect_equal(tab, expected3)
  
  # No missing camera in levels ---
  dflevels <- df
  dflevels$camera <- factor(dflevels$camera)
  tab <- suppressWarnings(summarize_species(dflevels,
                                            cam_col = "camera",
                                            spp_col = "species",
                                            count_col = "count", 
                                            by_cam = TRUE,
                                            keep_all_camera_levels = TRUE))
  
  expected4 <- expected2
  expected4$camera <- factor(expected4$camera)
  expect_equal(tab, expected4)

})

test_that("Reorder named values", {
  # Create camera names that have coordinates
  coord_cam <- letters[1:5]
  
  # Create test named vector
  val <- 1:4
  names(val) <- letters[3:6]
  
  # Keep all values
  res <- reorder_named_values(val, coord_cam, keep_all_names = TRUE)
  expect_equal(res, 
               c(a = NA, b = NA, c = 1, d = 2, e = 3))
  
  # Don't keep all values
  res <- reorder_named_values(val, coord_cam, keep_all_names = FALSE)
  expect_equal(res, 
               c(c = 1, d = 2, e = 3))
})


# Filter data -------------------------------------------------------------
test_that("Filter data", {
  df <- recordTableSample
  df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal)
  
  dfcam <- camtraps
  dfcam$Setup_date <- as.Date(dfcam$Setup_date,
                              format = "%d/%m/%Y")
  dfcam$Retrieval_date <- as.Date(dfcam$Setup_date,
                                  format = "%d/%m/%Y")
  
  dat <- list(data = list(observations = df,
                          deployments = dfcam))
  
  # All NULL (no filter)
  res <- filter_data(dat)
  expect_equal(dat, res)
  
  # Filter out all species
  spp_filter <- unique(dat$data$observations$Species)
  res <- filter_data(dat, spp_col = "Species", 
                     spp_filter = spp_filter)
  expect_equal(nrow(res$data$observations), 0)
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter out no species
  spp_filter <- character(0)
  res <- filter_data(dat, spp_col = "Species", 
                     spp_filter = spp_filter)
  expect_equal(dat, res)
  
  # Filter out all cameras
  cam_filter <- unique(dat$data$observations$Station)
  res <- filter_data(dat, 
                     cam_col_rec = "Station", 
                     cam_filter = cam_filter)
  expect_equal(nrow(res$data$observations), 0)
  expect_equal(nrow(res$data$deployments), 0)
  
  # Filter out all dates
  maxdate <- max(dat$data$observations$DateTimeOriginal)
  daterange <- as.Date(c(maxdate + 1, maxdate + 10))
  res <- filter_data(dat, 
                     timestamp_col = "DateTimeOriginal", 
                     daterange = daterange)
  expect_equal(nrow(res$data$observations), 0)
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter species
  res <- filter_data(dat, 
                     spp_col = "Species", 
                     spp_filter = "PBE")
  no_PBE <- unique(dat$data$observations$Species[dat$data$observations$Species != "PBE"])
  expect_equal(unique(res$data$observations$Species), no_PBE)
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter observations
  dat_type <- dat
  dat_type$data$observations$type <- c(rep("category1", nrow(dat$data$observations) - 7),
                                       rep("category2", 7))
  res <- filter_data(dat_type, 
                     obs_col = "type", 
                     obs_filter =  "category2")
  expect_equal(unique(res$data$observations$type), "category1")
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter observations and species
  res <- filter_data(dat_type, 
                     obs_col = "type", 
                     obs_filter =  "category2",
                     spp_col = "Species", 
                     spp_filter = "PBE")
  expect_equal(unique(res$data$observations$type), "category1")
  expect_equal(unique(res$data$observations$Species), no_PBE)
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter cameras
  camfilter <- c("StationA", "StationB")
  res <- filter_data(dat, 
                     cam_col_rec = "Station", 
                     cam_filter = camfilter)
  cams <- unique(dat$data$observations$Station[!dat$data$observations$Station %in% camfilter])
  expect_equal(unique(res$data$observations$Station), cams)
  expect_equal(res$data$deployments$Station, cams)
  
  # Filter custom ---
  colname <- "delta.time.days"
  val <- c(0, 1.8)
  
  expect_error(filter_data(dat, 
                           custom_filter  = val),
               "If custom_filter is provided, custom_col cannot be NULL.",
               fixed = TRUE)
  
  res <- filter_data(dat, 
                     custom_col = colname, 
                     custom_filter = val)
  
  val_res <- unique(res$data$observations[[colname]])
  val_expected <- unique(dat$data$observations[[colname]][!(dat$data$observations[[colname]] %in% val)])
  expect_equal(val_res, val_expected)
})

test_that("Filter data (dates)", {
  dat <- list(data = list(observations = records,
                          deployments = caminfo))
  
  # Filter dates with POSIX that have a tz ---
  daterange_orig <- range(dat$data$observations$timestamp)
  daterange <- c(daterange_orig[1] + lubridate::hours(26), 
                 daterange_orig[2])
  
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange)
  resrange <- range(res$data$observations$timestamp)
  # There is one obs every hour, so first and last obs should fall
  # exactly at date ranges
  expect_equal(resrange[1], daterange[1])
  expect_equal(resrange[2], daterange[2])
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter dates with POSIX that have no tz ---
  daterange <- as.POSIXct(c("2023-01-02 02:00:00", "2023-01-03 00:00:00"))
  
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange)
  resrange <- range(res$data$observations$timestamp)
  # There is one obs every hour, so first and last obs should fall
  # exactly at date ranges
  # Here we compare only char representations because else there is a tz
  # mismatch (as tz for daterange is empty)
  expect_equal(format(resrange[1], "%F %T"),
               format(daterange[1], "%F %T"))
  expect_equal(format(resrange[2], "%F %T"),
               format(daterange[2], "%F %T"))
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter dates with POSIX having a different tz ---
  daterange <- as.POSIXct(c("2023-01-02 02:00:00", "2023-01-03 00:00:00"),
                          tz = "Etc/GMT-4")
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange)
  resrange <- range(res$data$observations$timestamp)
  # convert daterange to data timezone
  attr(daterange, "tzone") <- "Etc/GMT"
  # There is one obs every hour, so first and last obs should fall
  # exactly at date ranges
  expect_equal(resrange[1], daterange[1])
  expect_equal(resrange[2], daterange[2])
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter dates (with dates objects) ---
  daterange <- as.Date(c("2023-01-02", "2023-01-03"))
  
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange)
  resrange <- range(res$data$observations$timestamp)
  # Convert to POSIX
  expect_equal(resrange[1], 
               as.POSIXct(daterange[1],
                          tz = "Etc/GMT"))
  expect_equal(resrange[2], 
               as.POSIXct(daterange[2],
                          tz = "Etc/GMT"))
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter dates (with characters) ---
  daterange <- c("2023-01-02", "2023-01-03")
  
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange)
  resrange <- range(res$data$observations$timestamp)
  # Convert to POSIX
  expect_equal(resrange[1], 
               as.POSIXct(daterange[1],
                          tz = "Etc/GMT"))
  expect_equal(resrange[2], 
               as.POSIXct(daterange[2],
                          tz = "Etc/GMT"))
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter dates (with custom timezone different from original one) ---
  daterange <- as.POSIXct(c("2023-01-02 02:00:00", "2023-01-03 00:00:00"),
                          tz = "Etc/GMT")
  custom_tz <- "Etc/GMT+10"
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange,
                     tz = custom_tz)
  resrange <- range(res$data$observations$timestamp)
  expect_equal(resrange[1], daterange[1])
  expect_equal(resrange[2], daterange[2])
  expect_equal(res$data$deployments, dat$data$deployments)
  
  # Filter dates (with custom timezone different from original one and timezoneless POSIX) ---
  daterange <- as.POSIXct(c("2023-01-02 02:00:00", "2023-01-03 00:00:00"))
  custom_tz <- "Etc/GMT-10"
  res <- filter_data(dat, 
                     timestamp_col = "timestamp", 
                     daterange = daterange,
                     tz = custom_tz)
  resrange <- range(res$data$observations$timestamp)
  # Add timezone then convert to GMT to check against final data
  daterange_tz <- force_tz(daterange, custom_tz)
  attr(daterange_tz, "tzone") <- "Etc/GMT"
  
  expect_equal(resrange[1], daterange_tz[1])
  expect_equal(resrange[2], daterange_tz[2])
  expect_equal(res$data$deployments, dat$data$deployments)
  
})

# Diversity ---------------------------------------------------------------

test_that("Get diversity indices", {
  countdf <- data.frame(camera = c("C1", "C1", "C1",
                                   "C2",
                                   "C3", "C3", "C3"),
                        species = c("cat", "cow", "rabbit",
                                    "cat",
                                    "cat", "cow", "rabbit"),
                        individuals = c(30, 30, 30,
                                        30,
                                        88, 1, 1),
                        individuals_prop = c(1/3, 1/3, 1/3,
                                             1,
                                             88/90, 1/90, 1/90))
  
  res <- get_diversity_indices(countdf, 
                               spp_col = "species", cam_col = "camera")
  expected <- data.frame(camera = c("C1", "C2", "C3"),
                         richness = c(3, 1, 3),
                         shannon = c(-3*((1/3)*log(1/3)),
                                     0, 
                                     -((88/90)*log(88/90) + (1/90)*log(1/90) + (1/90)*log(1/90))),
                         simpson = c((3*(30*29))/(90*89),
                                     1,
                                     (88*87)/(90*89))
                         )
  expect_equal(res, expected)
})

# Circular density distribution -------------------------------------------

test_that("Time to circular", {
  tim <- chron::times(c("00:00:00", "12:00:00", "08:30:00"))
  
  # Return simple vector ---
  # Radians
  res <- time_to_circular(tim, circular = FALSE)
  expect_true("numeric" %in% class(res))
  expect_equal(res, as.numeric(tim)*2*pi)
  
  # Hours
  res <- time_to_circular(tim, circular = FALSE, units = "hours")
  expect_true("numeric" %in% class(res))
  expect_equal(res, c(0, 12, 8.5))
  
  # Return circular object ---
  # Radians
  res <- time_to_circular(tim, circular = TRUE)
  
  expect_true("circular" %in% class(res))
  expect_equal(res[[1]], 0)
  expect_equal(res[[2]], pi)
  expect_equal(res[[3]], 8.5*(2*pi)/24)
  
  # Hours
  res <- time_to_circular(tim, circular = TRUE,
                          units = "hours")
  
  expect_true("circular" %in% class(res))
  expect_equal(res[[1]], 0)
  expect_equal(res[[2]], 12)
  expect_equal(res[[3]], 8.5)
  
})

test_that("Fit von Mises", {
  # Prepare test data ---
  testdat <- kga |> dplyr::select(snapshotName, eventTime) |> 
    filter(snapshotName == "gemsbok")
  
  # Check with radians ---
  # Fit model
  mod <- fit_vonMises(testdat$eventTime, k = 3)
  # Get density
  dt <- vonMises_density(mod, unit = "radians")
  
  # Check
  expect_equal(nrow(dt), length(seq(0, 2*pi, by = 0.01)))
})
    