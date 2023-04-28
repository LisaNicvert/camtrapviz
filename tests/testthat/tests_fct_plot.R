# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-27
#
# Script Description: tests for plotting functions

library(testthat)

test_that("Plot points", {
  
  df <- recordTableSample
  df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal)
  
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   spp_col = "Species")
  
  br <- as.character(ggplot2::layer_scales(p)$y$get_breaks())
  expect_equal(br, unique(df$Station))
  
  # With cameras_list
  cameras_list <- c("StationA", "StationB", "StationC", "StationD")
  p <- plot_points(df, 
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   spp_col = "Species",
                   cameras_list = cameras_list)
  br <- as.character(ggplot2::layer_scales(p)$y$get_breaks())
  expect_equal(br, cameras_list)
})

test_that("Plot species bars", {
  df <- data.frame(species = c("cat", "cat", "cat", "cow", "rabbit", NA),
                   type = c("animal", "animal", "animal", "animal", "animal", "blank"),
                   count = c(1, 1, 10, 10, 3, NA))
  
  # Don't replace NA ---
  p <- suppressWarnings(plot_species_bars(df, 
                                          spp_col = "species", 
                                          obs_col = "type",
                                          count_col = "count"))
  br <- suppressWarnings(as.character(ggplot2::layer_scales(p)$x$get_breaks()))
  # Test that NA species was replaced with blank
  expect_equal(br, c("rabbit", "cow", "cat", "blank"))

  # Replace NA with one
  p <- plot_species_bars(df, 
                         spp_col = "species", 
                         obs_col = "type",
                         count_col = "count",
                         NA_count_placeholder = 1)
  br <- as.character(ggplot2::layer_scales(p)$x$get_breaks())
  # Test that NA species was replaced with blank
  expect_equal(br, c("blank", "rabbit", "cow", "cat"))
})
