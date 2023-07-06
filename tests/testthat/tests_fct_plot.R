# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-27
#
# Script Description: tests for plotting functions

library(testthat)
library(ggplot2)
library(ggiraph)

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

test_that("Plot map (radii)", {
  # Default radii ---
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station")
  
  # Custom radii ---
  radii <- c(1, 120, 500)
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           radius = radii,
           rescale = TRUE)
  
  # Don't rescale
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           radius = radii)
  
  # Named radii ---
  
  # All radii
  radii <- c("StationC" = 1, "StationA" = 120, "StationB" = 500)
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           radius = radii,
           rescale = TRUE)
  
  # Not all radii
  radii <- c("StationA" = 120, "StationB" = 500)
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           radius = radii,
           rescale = FALSE)
})

test_that("Plot map (colors)", {
  
  # Color vector ---
  cols <- c("red", "blue", "yellow")
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           color = cols)
  
  # With NA radius ---
  cols <- c("red", "blue", "yellow")
  radius <- c(NA, 4, 4)
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           color = cols, 
           radius = radius)
  
})

test_that("Plot map (labels)", {
  
  # Unnamed labels ---
  labels <- c("AAA", "BBB", "CCC")
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           label = labels)
})

test_that("Plot map (labels displayed on map)", {
  
  # Unnamed labels ---
  labels <- c("AAA", "BBB", "CCC")
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           label = labels,
           display_camnames = TRUE)
  
  # Default labels ---
  plot_map(camtraps,
           lat_col = "utm_y",
           lon_col = "utm_x",
           crs = 32650, # Here we use the EPSG code for UTM zone 50N
           cam_col = "Station",
           display_camnames = TRUE)
})

test_that("Plot density", {
  # Prepare test data ---
  testdat <- kga |> dplyr::select(snapshotName, eventTime) |> 
    filter(snapshotName == "gemsbok")
  testdat$hour <- as.numeric(testdat$eventTime)*24
  
  # Check with radians ---
  # Fit model
  mod <- fit_vonMises(testdat$eventTime, k = 3)
  # Get density
  dt <- vonMises_density(mod)
  
  gg <- ggplot(dt) +
    geom_histogram_interactive(data = testdat, 
                   aes(x = hour,
                       y = after_stat(density),
                       tooltip = paste0("Density: ", round(after_stat(density), 3), "\n",
                                        "Time: ", after_stat(x)),
                       data_id = after_stat(x)),
                   alpha = 0.7,
                   binwidth = 1) +
    geom_line(aes(x = x, y = density)) +
    scale_x_continuous(breaks = seq(0, 24, by = 4)) +
    ggtitle("Estimated density") +
    xlab("Time (hours)") +
    ylab("Density") +
    theme_linedraw()
  
  gi <- ggiraph::girafe(ggobj = gg)
  gi <- ggiraph::girafe_options(gi,
                                opts_hover(css = "fill:orange"))
  
  gi
})
