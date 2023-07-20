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
                   points_col = "Species")
  
  br <- as.character(ggplot2::layer_scales(p)$y$get_breaks())
  expect_equal(br, unique(df$Station))
  
  # With cameras_list
  cameras_list <- c("StationA", "StationB", "StationC", "StationD")
  p <- plot_points(df, 
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   points_col = "Species",
                   cameras_list = cameras_list)
  br <- as.character(ggplot2::layer_scales(p)$y$get_breaks())
  expect_equal(br, cameras_list)
  
  # With custom labels
  labels <- c(x = "Date (in French)",
              y = "Stations used in this project")
  p <- plot_points(df, 
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   points_col = "Species",
                   ylab = labels[["y"]],
                   xlab = labels[["x"]])
  expect_equal(p$labels$y, labels[["y"]])
  expect_equal(p$labels$x, labels[["x"]])
})

test_that("Plot points (different parameter combinations)", {
  df <- recordTableSample
  df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal)
  
  # Default parameters ---
  expect_no_error(plot_points(df,
                              camera_col = "Station",
                              timestamp_col = "DateTimeOriginal"))
  
  # Color points as species ---
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   points_col = "Species")
  expect_equal(as_label(p$mapping[[3]]), "Species")
  
  # Color points as species ---
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   points_col = "Species")
  expect_equal(as_label(p$mapping[[3]]), "Species")
  
  # Interactive graph with no color or tooltip ---
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   interactive = TRUE)
  expect_true("GeomInteractivePoint" %in% class(p$layers[[1]]$geom))
  expect_equal(as_label(p$layers[[1]]$mapping$tooltip),
               "DateTimeOriginal") # Tooltip
  
  # Interactive graph with colors and tooltip (default) ---
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   points_col = "Species",
                   interactive = TRUE)
  expect_true("GeomInteractivePoint" %in% class(p$layers[[1]]$geom)) # Interactive
  expect_equal(as_label(p$mapping[[3]]), "Species") # Color
  expect_equal(as_label(p$layers[[1]]$mapping$tooltip),
               "paste(.data[[\"Species\"]], .data[[\"DateTimeOriginal\"]], sep = \": \")") # Tooltip
  
  # Interactive graph with colors and (non-default) tooltip ---
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   points_col = "Species", 
                   tooltip_info = "Directory",
                   interactive = TRUE)
  expect_true("GeomInteractivePoint" %in% class(p$layers[[1]]$geom)) # Interactive
  expect_equal(as_label(p$mapping[[3]]), "Species") # Color
  expect_equal(as_label(p$layers[[1]]$mapping$tooltip),
               "paste(.data[[\"Directory\"]], .data[[\"DateTimeOriginal\"]], sep = \": \")") # Tooltip
  

  # Interactive graph with no color but custom tooltip ---
  p <- plot_points(df,
                   camera_col = "Station",
                   timestamp_col = "DateTimeOriginal",
                   tooltip_info = "Species", 
                   interactive = TRUE)
  expect_true("GeomInteractivePoint" %in% class(p$layers[[1]]$geom)) # Interactive
  expect_equal(length(p$mapping), 2) # No color
  expect_equal(as_label(p$layers[[1]]$mapping$tooltip),
               "paste(.data[[\"Species\"]], .data[[\"DateTimeOriginal\"]], sep = \": \")") # Tooltip

})

test_that("Plot points (colors)", {
  df <- recordTableSample
  df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal)
  
  # Unique color ---
  (p <- plot_points(df,
                    camera_col = "Station",
                    timestamp_col = "DateTimeOriginal",
                    cols = "deeppink"))
  gb <- layer_data(p)
  expect_equal(unique(gb$colour), "deeppink")
  
  # Species colors ---
  (p <- plot_points(df,
                    camera_col = "Station",
                    timestamp_col = "DateTimeOriginal",
                    points_col = "Species"))
  gb <- layer_data(p)
  expect_equal(unique(gb$colour), 
               c("#7570B3", "#66A61E", "#D95F02", "#1B9E77", "#E7298A"))
  
  # Custom species colors ---
  spp <- unique(df$Species)
  pal <- RColorBrewer::brewer.pal(length(spp), "Set2")
  (p <- plot_points(df,
              camera_col = "Station",
              timestamp_col = "DateTimeOriginal",
              points_col = "Species",
              cols = pal))
  gb <- layer_data(p)
  expect_true(all(unique(gb$colour) %in% pal))

  # Named species colors ---
  # Shuffle colours
  shuf <- sample(1:length(pal), 
                 size = length(pal), replace = FALSE)
  pal2 <- pal[shuf]
  names(pal2) <- spp[shuf] # Add colour names
  (p <- plot_points(df,
                    camera_col = "Station",
                    timestamp_col = "DateTimeOriginal",
                    points_col = "Species",
                    cols = pal2))
  gb <- layer_data(p)
  # Get datetime of one observation of EGY
  dttime <- df$DateTimeOriginal[df$Species == "EGY"][1]
  # Get colour of this observation
  coltest <- gb$colour[gb$x == as.numeric(dttime)]
  expect_equal(coltest, 
               unname(pal2["EGY"]))
})

test_that("Plot points with rectangles", {
  
  df <- recordTableSample
  df$DateTimeOriginal <- as.POSIXct(df$DateTimeOriginal)
  
  caminfo <- camtraps
  caminfo$Setup_date <- as.Date(caminfo$Setup_date, 
                                format = "%d/%m/%Y") 
  caminfo$Retrieval_date <- as.Date(caminfo$Retrieval_date, 
                                    format = "%d/%m/%Y")
  
  # Normal case ---
  (p <- plot_points(df,
                    camera_col = "Station",
                    timestamp_col = "DateTimeOriginal",
                    caminfo = caminfo,
                    caminfo_setup = "Setup_date",
                    caminfo_retrieval = "Retrieval_date"))
  gb <- layer_data(p)
  # Check it is the good layer
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% colnames(gb)))
  expect_equal(nrow(gb), 3) # 3 rectangles
  
  
  # Cameras as factors ---
  df_fac <- df |> 
    mutate(Station = factor(Station))
  
  (p <- plot_points(df_fac,
                    camera_col = "Station",
                    timestamp_col = "DateTimeOriginal",
                    points_col = "Species",
                    caminfo = caminfo,
                    caminfo_setup = "Setup_date",
                    caminfo_retrieval = "Retrieval_date"))
  # Check it is the good layer
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% colnames(gb)))
  expect_equal(nrow(gb), 3) # 3 rectangles
  
  # Some more sampling ---
  row <- caminfo[1,]
  row$Station <- "StationD"
  caminfo_plus <- caminfo |> 
    bind_rows(row)
  
  expect_warning(plot_points(df,
                             camera_col = "Station",
                             timestamp_col = "DateTimeOriginal",
                             points_col = "Species",
                             caminfo = caminfo_plus,
                             caminfo_setup = "Setup_date",
                             caminfo_retrieval = "Retrieval_date"))
  suppressWarnings(
    (p <- plot_points(df,
                      camera_col = "Station",
                      timestamp_col = "DateTimeOriginal",
                      points_col = "Species",
                      caminfo = caminfo_plus,
                      caminfo_setup = "Setup_date",
                      caminfo_retrieval = "Retrieval_date"))
  )
  # Check it is the good layer
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% colnames(gb)))
  expect_equal(nrow(gb), 3) # 3 rectangles
  
  # Some more cameras ---
  # Factor
  df_fac <- df |> 
    mutate(Station = factor(Station, 
                            levels = c(unique(Station), "StationD")))
  
  expect_warning(plot_points(df_fac,
                             camera_col = "Station",
                             timestamp_col = "DateTimeOriginal",
                             points_col = "Species",
                             caminfo = caminfo,
                             caminfo_setup = "Setup_date",
                             caminfo_retrieval = "Retrieval_date"))
  
  suppressWarnings(
    (p <- plot_points(df_fac,
                      camera_col = "Station",
                      timestamp_col = "DateTimeOriginal",
                      points_col = "Species",
                      caminfo = caminfo,
                      caminfo_setup = "Setup_date",
                      caminfo_retrieval = "Retrieval_date"))
  )
  # Check it is the good layer
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% colnames(gb)))
  expect_equal(nrow(gb), 3) # 3 rectangles
  # Check camera level was added
  br <- as.character(ggplot2::layer_scales(p)$y$get_breaks())
  expect_equal(levels(df_fac$Station), br)
  
  # Data points ---
  camdup <- df |> 
    filter(Station == "StationA") |> 
    mutate(Station = "StationD")
  df_plus <- df |> bind_rows(camdup)
  expect_warning(plot_points(df_plus,
                             camera_col = "Station",
                             timestamp_col = "DateTimeOriginal",
                             points_col = "Species",
                             caminfo = caminfo,
                             caminfo_setup = "Setup_date",
                             caminfo_retrieval = "Retrieval_date"))
  
  suppressWarnings(
    (p <- plot_points(df_plus,
                      camera_col = "Station",
                      timestamp_col = "DateTimeOriginal",
                      points_col = "Species",
                      caminfo = caminfo,
                      caminfo_setup = "Setup_date",
                      caminfo_retrieval = "Retrieval_date"))
  )
  # Check it is the good layer
  expect_true(all(c("xmin", "xmax", "ymin", "ymax") %in% colnames(gb)))
  expect_equal(nrow(gb), 3) # 3 rectangles
  # Check camera level was added
  br <- as.character(ggplot2::layer_scales(p)$y$get_breaks())
  expect_equal(levels(df_fac$Station), br)
  
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
