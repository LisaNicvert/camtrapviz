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
