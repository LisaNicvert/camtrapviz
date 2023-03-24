library(testthat)

test_that("Read data with NAs", {
  df <- data.frame(letters = letters[1:5],
                   num = 1:5)
  df$letters[1] <- ""
  df$letters[2] <- "  "
  df$num[5] <- "NA"
  
  tfile <- paste0(tempfile(), ".csv")
  write.csv(df, file = tfile, row.names = FALSE)
  
  dat <- read_data(tfile, sep_records = ",", NA_strings = c("", "NA"))
  
  expect_true(is.na(dat$data$observations$letters[1]))
  expect_false(is.na(dat$data$observations$num[2]))
  expect_true(is.na(dat$data$observations$num[5]))
})

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
                   time_col = "chron::time",
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
                                 setup_col = "Setup_date"),
               "If dfcam is not NULL, then cam_col_dfcam must be provided.")
  
  expect_error(summarize_cameras(recordTableSample, 
                                 cam_col = "Station",
                                 timestamp_col = "DateTimeOriginal",
                                 dfcam = camtraps,
                                 cam_col_dfcam = "Station"),
               "If dfcam is not NULL, then setup_col or retrieval_col must be provided.")

})

test_that("Summarize cameras (no dfcam)", {
  res <- summarize_cameras(mica$data$observations, 
                           cam_col = "deploymentID", 
                           timestamp_col = "timestamp")
  
  expect_equal(colnames(res), c("deploymentID", "setup", "retrieval",
                                "setup_origin", "retrieval_origin"))
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("picture", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
  
  # With date/time
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate")
  
  expect_equal(colnames(res), c("cameraID", "setup", "retrieval",
                                "setup_origin", "retrieval_origin"))
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("picture", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
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
  expect_equal(colnames(res), c("cameraID", "setup", "retrieval",
                                "setup_origin", "retrieval_origin"))
  expect_equal(class(res$setup), c("POSIXct", "POSIXt"))
  expect_true(all(res$setup < res$retrieval))
  expect_equal(rep("setup", nrow(res)), res$setup_origin)
  expect_equal(rep("picture", nrow(res)), res$retrieval_origin)
})

test_that("Summarize cameras with different cameras name", {
  kga_test <- kga %>% rename("cam" = "cameraID")
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
  # Remove camera A11 because it is not in kga_cameras

  # NA in setup
  kga_cameras_test <- kga_cameras
  kga_cameras_test$Setup.Date[kga_cameras_test$cameraID == "KGA_A01"] <- NA
  
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras_test,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expected <- kga %>% filter(cameraID == "KGA_A01") %>% 
    mutate(dtime = as.POSIXct(paste(eventDate, eventTime))) %>%
    summarise(mintime = min(dtime)) %>%
    magrittr::extract2("mintime")
  expect_equal(res$setup[res$cameraID == "KGA_A01"], expected)
  
  # Missing camera in setup
  kga_cameras_test <- kga_cameras %>% filter(cameraID != "KGA_A01")
  
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras_test,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expect_equal(res$setup[res$cameraID == "KGA_A01"], expected)
  
  # Missing camera in data
  kga_test2 <- kga %>% filter(cameraID != "KGA_A01")
  res <- summarize_cameras(kga, 
                           cam_col = "cameraID", 
                           time_col = "eventTime",
                           date_col = "eventDate",
                           dfcam = kga_cameras,
                           cam_col_dfcam = "cameraID", 
                           setup_col = "Setup.Date")
  expected <- as.POSIXct(kga_cameras$Setup.Date[kga_cameras$cameraID == "KGA_A01"])
  expect_equal(res$setup[res$cameraID == "KGA_A01"], expected)
  
})