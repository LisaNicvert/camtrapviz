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
                         setup = as.POSIXct(c("2020-04-01 12:00:00",
                                              "2020-04-11 12:00:00",
                                              NA)),
                         retrieval = as.POSIXct(c("2020-04-02 12:00:00",
                                                  "2020-04-12 12:00:00",
                                                  NA)),
                         setup_origin = c("picture", "picture", NA),
                         retrieval_origin = c("picture", "picture", NA),
                         sampling_length = c(1, 1, NA))
  expect_equal(res, expected)
  
  # Don't give cam_col_dfcam
  res <- summarize_cameras(df, 
                           timestamp_col = "stamp",
                           cam_col = "camera",
                           dfcam = dfcam)
  expect_equal(res, expected)
})


# Summarize species -------------------------------------------------------
test_that("Get species", {
  df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, "cameratrapper", "tourist"),
                   type = c("animal", "animal", "animal", "fire", "blank", "human", "human"))
  
  # No observation type
  res <- get_species(df, spp_col = "species")
  expected <- data.frame(species = c("cameratrapper", "cat", "rabbit", "tourist", NA))
  rownames(expected) <- paste("ID", 1:nrow(expected), sep = "_")
  expect_equal(res, expected)
  
  # Observation type
  res <- get_species(df, 
                     spp_col = "species", obs_col = "type")
  expected <- data.frame(species = c("cat", "rabbit",
                                     "blank", "fire", "cameratrapper", "tourist"),
                         type = c("animal", "animal",
                                  "blank", "fire", "human", "human"))
  rownames(expected) <- paste("ID", 1:nrow(expected), sep = "_")
  expect_equal(res, expected)
  
  # NA in obstype
  df <- data.frame(species = c("cat", "human", NA, NA),
                   type = c("animal", "human", "blank", NA))
  res <- get_species(df, 
                     spp_col = "species", obs_col = "type")
  expected <- data.frame(species = c("cat", "blank", "human", NA),
                         type = c("animal", "blank", "human", NA))
  rownames(expected) <- paste("ID", 1:nrow(expected), sep = "_")
  expect_equal(res, expected)
})

test_that("Get species (return vector)", {
  df <- data.frame(species = c("rabbit", "cat", "cat", NA, NA, "cameratrapper", "tourist"),
                   type = c("animal", "animal", "animal", "fire", "blank", "human", "human"))
  
  # No observation type
  res <- get_species(df, spp_col = "species", return_df = FALSE)
  expected <- c("cameratrapper", "cat", "rabbit", "tourist", NA)
  expect_equal(res, expected)
  
  # Observation type
  res <- get_species(df, 
                     spp_col = "species", obs_col = "type",
                     return_df = FALSE)
  expected <- c("cat", "rabbit", "blank", "fire", "cameratrapper", "tourist")
  expect_equal(res, expected)
  
  # NA in obstype
  df <- data.frame(species = c("cat", "human", NA, NA),
                   type = c("animal", "human", "blank", NA))
  res <- get_species(df, 
                     spp_col = "species", obs_col = "type",
                     return_df = FALSE)
  expected <- c("cat", "blank", "human", NA)
  expect_equal(res, expected)
})

test_that("Summarize species", {
  df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", "rabbit", NA, NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "animal", "human", "blank"),
                   camera = c("C1", "C1", "C1", "C1", "C2", "C3", "C3", "C4"),
                   count = c(1, 1, 1, 50, 3, 4, 1, NA))
  
  # Without obstype ---
  res <- summarize_species(df, species_col = "species", cam_col = "camera")
  
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA),
                         n_sightings = c(2, 2, 1, 1, 2),
                         n_individuals = c(2, 2, 1, 1, 2),
                         n_cameras = c(1, 2, 1, 1, 2))
  expected$prop_cam <- expected$n_cameras/4
  expect_equal(res, expected)
  
  # With obstype ---
  res <- summarize_species(df, 
                           species_col = "species", cam_col = "camera",
                           obs_col = "type")
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA, NA),
                         type = c("animal", "animal", "animal", "animal", "blank", "human"),
                         n_sightings = c(2, 2, 1, 1, 1, 1),
                         n_individuals = c(2, 2, 1, 1, 1, 1),
                         n_cameras = c(1, 2, 1, 1, 1, 1))
  expected$prop_cam <- expected$n_cameras/4
  expect_equal(res, expected)
  
  # With count ---
  res <- suppressWarnings(summarize_species(df, 
                                            species_col = "species", cam_col = "camera",
                                            obs_col = "type",
                                            count_col = "count"))
  expected$n_individuals <- c(2, 53, 4, 1, NA, 1)
  expected$prop_cam <- expected$n_cameras/4
  expect_equal(res, expected)
  
  # With ncam ---
  res <- suppressWarnings(summarize_species(df, 
                                            species_col = "species", cam_col = "camera",
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
                                   species_col = "species", 
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
                                   species_col = "species", 
                                   cam_col = "camera",
                                   count_col = "count"),
                 "There are NAs in the count column; if you want to replace them with a value, use NA_count_placeholder.",
                 fixed = TRUE)
  # Check value
  res <- suppressWarnings(summarize_species(df, 
                                            species_col = "species", 
                                            cam_col = "camera",
                                            count_col = "count"))
  expect_true(is.na(res$n_individuals))
  
  # With NA replacement ---
  res <- summarize_species(df, 
                           species_col = "species", 
                           cam_col = "camera",
                           count_col = "count",
                           NA_count_placeholder = 1)
  expect_equal(res$n_individuals, 7)
})

test_that("Summarize species without camera", {
  df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", "rabbit", NA, NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "animal", "human", "blank"),
                   count = c(1, 1, 1, 50, 3, 4, 1, NA))
  
  # Without obstype ---
  res <- summarize_species(df, species_col = "species")
  
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA),
                         n_sightings = c(2, 2, 1, 1, 2),
                         n_individuals = c(2, 2, 1, 1, 2))
  expect_equal(res, expected)
  
  # With obstype ---
  res <- summarize_species(df, 
                           species_col = "species",
                           obs_col = "type")
  expected <- data.frame(species = c("cat", "cow", "rabbit", "zebra", NA, NA),
                         type = c("animal", "animal", "animal", "animal", "blank", "human"),
                         n_sightings = c(2, 2, 1, 1, 1, 1),
                         n_individuals = c(2, 2, 1, 1, 1, 1))
  expect_equal(res, expected)
  
  # With count ---
  res <- suppressWarnings(summarize_species(df, 
                                            species_col = "species",
                                            obs_col = "type",
                                            count_col = "count"))
  expected$n_individuals <- c(2, 53, 4, 1, NA, 1)
  expect_equal(res, expected)
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

# Diversity ---------------------------------------------------------------

test_that("Get diversity df", {
  
  # Normal case ---
  df <- data.frame(species = c("zebra", "cat", "cat", "cow", "cow", NA, NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "human", "blank"),
                   camera = c("C1", "C1", "C1", "C1", "C2", "C2", "C3"),
                   count = c(1, 1, 1, 50, 3, 1, NA))
  
  tab <- get_diversity_table(df,
                             cam_col = "camera",
                             spp_col = "species")
  expected <- data.frame(camera = c("C1", "C1", "C1", 
                                    "C2", "C2", 
                                    "C3"),
                         species = c("cat", "cow", "zebra",
                                     "cow", NA,
                                     NA),
                         count = c(2, 1, 1, 
                                   1, 1,
                                   1),
                         prop = c(0.5, 0.25, 0.25,
                                  0.5, 0.5,
                                  1))
  expect_equal(tab, expected)
  
  # Add count ---
  tab <- get_diversity_table(df,
                             cam_col = "camera",
                             spp_col = "species",
                             count_col = "count")
  expected2 <- expected
  expected2$count <- c(2, 50, 1, 
                       3, 1,
                       NA)
  expected2$prop <- c(2/53, 50/53, 1/53, 
                      3/4, 1/4,
                      NA)
  expect_equal(tab, expected2)
  
  # Add missing camera ---
  dflevels <- df
  dflevels$camera <- factor(dflevels$camera, 
                            levels = c("C4", "C1", "C2", "C3"))
  
  tab <- get_diversity_table(dflevels,
                             cam_col = "camera",
                             spp_col = "species",
                             count_col = "count", 
                             keep_all_levels = TRUE)
  expected3 <- data.frame(camera = factor(c("C4", "C1", "C1", "C1", 
                                            "C2", "C2", 
                                            "C3"),
                                          levels = c("C4", "C1", "C2", "C3")),
                          species = c(NA, 
                                      "cat", "cow", "zebra",
                                      "cow", NA,
                                      NA),
                          count = c(NA,
                                    2, 50, 1, 
                                    3, 1,
                                    NA),
                          prop = c(NA,
                                   2/53, 50/53, 1/53, 
                                   3/4, 1/4,
                                   NA),
                          empty = c(TRUE, 
                                    FALSE, FALSE, FALSE,
                                    FALSE, FALSE,
                                    FALSE))
  
  expect_equal(tab, expected3)
  
  # No missing camera in levels ---
  dflevels <- df
  dflevels$camera <- factor(dflevels$camera)
  tab <- get_diversity_table(dflevels,
                             cam_col = "camera",
                             spp_col = "species",
                             count_col = "count", 
                             keep_all_levels = TRUE)
  
  expected4 <- expected2
  expected4$camera <- factor(expected4$camera)
  expect_equal(tab, expected4)
})

test_that("Get diversity indices", {
  countdf <- data.frame(camera = c("C1", "C1", "C1",
                                   "C2",
                                   "C3", "C3", "C3"),
                        species = c("cat", "cow", "rabbit",
                                    "cat",
                                    "cat", "cow", "rabbit"),
                        count = c(30, 30, 30,
                                  30,
                                  88, 1, 1),
                        prop = c(1/3, 1/3, 1/3,
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
