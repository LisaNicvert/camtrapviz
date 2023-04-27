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
  
  plot_points(df, 
              camera_col = "Station",
              timestamp_col = "DateTimeOriginal",
              spp_col = "Species")
  
  # With cameras_list
  cameras_list <- c("StationA", "StationB", "StationC", "StationD")
  plot_points(df, 
              camera_col = "Station",
              timestamp_col = "DateTimeOriginal",
              spp_col = "Species",
              cameras_list = cameras_list)
})
