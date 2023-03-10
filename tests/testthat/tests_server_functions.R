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

test_that("Cast columns", {
  df <- data.frame(num = 1:10,
                   char = letters[1:10])
  cast <- c(num = "as.character",
            char = "as.factor")
  
  dfcast <- cast_columns(df, cast)
  
  expect_equal(class(dfcast$num), "character")
  expect_equal(class(dfcast$char), "factor")
  
})

test_that("Format data", {
  
  # Normal case
  mapping <- list(spp_col = "Species",
                  cam_col = "Station",
                  date_col = NULL,
                  time_col = NULL,
                  timestamp_col = "DateTimeOriginal")
  cast_type <- c(spp_col = "as.character",
                 cam_col = "as.character",
                 date_col = "as_datetime",
                 time_col = "chron::time",
                 timestamp_col = "as_datetime")
  
  df <- format_table(recordTableSample, mapping, cast_type)
  expect_equal(class(df$Species), "character")
  expect_equal(class(df$Station), "character")
  expect_equal(class(df$DateTimeOriginal), c("POSIXct", "POSIXt"))
  
  # NULL mapping has no corresponding regex
  cast_type <- c(spp_col = "as.character",
                 cam_col = "as.character",
                 timestamp_col = "as_datetime")
  
  df <- format_table(recordTableSample, mapping, cast_type)
  expect_equal(class(df$Species), "character")
  expect_equal(class(df$Station), "character")
  expect_equal(class(df$DateTimeOriginal), c("POSIXct", "POSIXt"))
  
  # Mapping has more elements
  mapping <- list(spp_col = "Species",
                  cam_col = "Station")
  cast_type <- c(spp_col =  "as.character")
  
  expect_error(format_table(recordTableSample, mapping, cast_type),
               "all non-null columns listed in mapping must be in cast_type", 
               fixed = TRUE)
})
