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


# Plot points -------------------------------------------------------------

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



# Plot bars ---------------------------------------------------------------

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



# Plot map ----------------------------------------------------------------

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

# Activity plot -----------------------------------------------------------

test_that("Activity plot (histogram) with frequency", {
  
  # Data ---
  data(recordTableSample, package = "camtrapR")
  
  # Convert hours to times format
  recordTableSample <- recordTableSample |> 
    mutate(Time = chron::times(Time))
  # Convert to radians
  recordTableSample <- recordTableSample |> 
    mutate(time_radians = as.numeric(Time)*2*pi,
           .after = Time)
  
  PBE_records <- recordTableSample[recordTableSample$Species == "PBE", ]
  
  # Clock time ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "Time"))
  
  expect_equal(p$labels$x, "Time (hours)")
  expect_equal(p$labels$y, "Count")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("00:00", "04:00", "08:00", 
                      "12:00", "16:00", "20:00", "24:00"))
  ld <- layer_data(p)
  
  tst_01 <- ld$y[1]
  true_01 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("01:00:00")
  count_01 <- sum(true_01)
  expect_equal(tst_01, count_01)
  
  # Radians ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "time_radians",
                      unit = "radians"))
  
  expect_equal(p$labels$x, "Time (radians)")
  expect_equal(p$labels$y, "Count")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("0", "0.333\U03C0", "0.667\U03C0", 
                      "\U03C0", "1.333\U03C0", "1.667\U03C0", "2\U03C0"))
  ld <- layer_data(p)
  
  tst_01 <- ld$y[1]
  true_01 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("01:00:00")
  count_01 <- sum(true_01)
  expect_equal(tst_01, count_01)
})


test_that("Activity plot (histogram) with density", {
  
  # Data ---
  data(recordTableSample, package = "camtrapR")
  
  # Convert hours to times format
  recordTableSample <- recordTableSample |> 
    mutate(Time = chron::times(Time))
  # Convert to radians
  recordTableSample <- recordTableSample |> 
    mutate(time_radians = as.numeric(Time)*2*pi,
           .after = Time)
  
  PBE_records <- recordTableSample[recordTableSample$Species == "PBE", ]
  
  # Clock time ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "Time",
                      hist_breaks = 2,
                      freq = FALSE))
  
  expect_equal(p$labels$x, "Time (hours)")
  expect_equal(p$labels$y, "Density")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("00:00", "04:00", "08:00", 
                      "12:00", "16:00", "20:00", "24:00"))
  ld <- layer_data(p)
  
  hist_02 <- ld$density[1] # Histogram height
  true_02 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("02:00:00")
  prop_02 <- sum(true_02)/length(PBE_records$Time)
  area_02 <- hist_02*2 # Area = hist height x hist width
  expect_equal(prop_02, area_02)
  
  # Radians ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "time_radians",
                      unit = "radians",
                      hist_breaks = 2*(2*pi)/24,
                      freq = FALSE))
  
  expect_equal(p$labels$x, "Time (radians)")
  expect_equal(p$labels$y, "Density")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("0", "0.333\U03C0", "0.667\U03C0", 
                      "\U03C0", "1.333\U03C0", "1.667\U03C0", "2\U03C0"))
  ld <- layer_data(p)
  
  hist_02 <- ld$density[1] # Histogram height
  true_02 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("02:00:00")
  prop_02 <- (sum(true_02)/length(PBE_records$Time))
  area_02 <- hist_02*2*(2*pi)/24 # Area = hist height x hist width
  expect_equal(prop_02, area_02)
})


test_that("AInteractive ativity plot (histogram) with frequency", {
  
  # Data ---
  data(recordTableSample, package = "camtrapR")
  
  # Convert hours to times format
  recordTableSample <- recordTableSample |> 
    mutate(Time = chron::times(Time))
  # Convert to radians
  recordTableSample <- recordTableSample |> 
    mutate(time_radians = as.numeric(Time)*2*pi,
           .after = Time)
  
  PBE_records <- recordTableSample[recordTableSample$Species == "PBE", ]
  
  # Clock time ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "Time",
                      interactive = TRUE))
  (g <- girafe(ggobj = p))
  
  expect_equal(p$labels$x, "Time (hours)")
  expect_equal(p$labels$y, "Count")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("00:00", "04:00", "08:00", 
                      "12:00", "16:00", "20:00", "24:00"))
  as.character(p$labels$tooltip)
  
  ld <- layer_data(p)
  
  tst_01 <- ld$y[1]
  true_01 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("01:00:00")
  count_01 <- sum(true_01)
  expect_equal(tst_01, count_01)
  # Tooltip check
  expect_equal(ld$tooltip[1],
               "Count: 4\nTime: 00:00 — 01:00")
  
  # Radians ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "time_radians",
                      unit = "radians",
                      interactive = TRUE))
  
  (g <- girafe(ggobj = p))
  
  expect_equal(p$labels$x, "Time (radians)")
  expect_equal(p$labels$y, "Count")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("0", "0.333\U03C0", "0.667\U03C0", 
                      "\U03C0", "1.333\U03C0", "1.667\U03C0", "2\U03C0"))
  ld <- layer_data(p)
  
  tst_01 <- ld$y[1]
  true_01 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("01:00:00")
  count_01 <- sum(true_01)
  expect_equal(tst_01, count_01)
  # Tooltip check
  expect_equal(ld$tooltip[1],
               "Count: 4\nTime: 0 — 0.083\U03C0")
})


test_that("Interactive activity plot (histogram) with density", {
  
  # Data ---
  data(recordTableSample, package = "camtrapR")
  
  # Convert hours to times format
  recordTableSample <- recordTableSample |> 
    mutate(Time = chron::times(Time))
  # Convert to radians
  recordTableSample <- recordTableSample |> 
    mutate(time_radians = as.numeric(Time)*2*pi,
           .after = Time)
  
  PBE_records <- recordTableSample[recordTableSample$Species == "PBE", ]
  
  # Clock time ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "Time",
                      hist_breaks = 2,
                      freq = FALSE,
                      interactive = TRUE))
  (g <- girafe(ggobj = p))
  
  expect_equal(p$labels$x, "Time (hours)")
  expect_equal(p$labels$y, "Density")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("00:00", "04:00", "08:00", 
                      "12:00", "16:00", "20:00", "24:00"))
  ld <- layer_data(p)
  
  hist_02 <- ld$density[1] # Histogram height
  true_02 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("02:00:00")
  prop_02 <- sum(true_02)/length(PBE_records$Time)
  area_02 <- hist_02*2 # Area = hist height x hist width
  expect_equal(prop_02, area_02)
  # Tooltip check
  expect_equal(ld$tooltip[1],
               "Density: 0.111\nTime: 00:00 — 02:00")
  
  # Radians ---
  (p <- plot_activity(true_data = PBE_records, 
                      times_true = "time_radians",
                      unit = "radians",
                      hist_breaks = 2*(2*pi)/24,
                      freq = FALSE,
                      interactive = TRUE))
  (g <- girafe(ggobj = p))
  
  expect_equal(p$labels$x, "Time (radians)")
  expect_equal(p$labels$y, "Density")
  lab <- as.character(ggplot2::layer_scales(p)$x$get_labels())
  expect_equal(lab, c("0", "0.333\U03C0", "0.667\U03C0", 
                      "\U03C0", "1.333\U03C0", "1.667\U03C0", "2\U03C0"))
  ld <- layer_data(p)
  
  hist_02 <- ld$density[1] # Histogram height
  true_02 <- PBE_records$Time >= chron::times("00:00:00") & PBE_records$Time <= chron::times("02:00:00")
  prop_02 <- (sum(true_02)/length(PBE_records$Time))
  area_02 <- hist_02*2*(2*pi)/24 # Area = hist height x hist width
  expect_equal(prop_02, area_02)
  # Tooltip check
  expect_equal(ld$tooltip[1],
               "Density: 0.424\nTime: 0 — 0.167\U03C0")
})


# Helper ------------------------------------------------------------------

test_that("Format hour", {
  h <- format_hour(2)
  expect_equal(h, "02:00")
  
  h <- format_hour(22)
  expect_equal(h, "22:00")
})

test_that("Format radian", {
  r <- format_radian(2*pi)
  expect_equal(r, "2\U03C0")
  
  r <- format_radian(pi)
  expect_equal(r, "\U03C0")
  
  r <- format_radian(0)
  expect_equal(r, "0")
  
  r <- format_radian(pi/2)
  expect_equal(r, "0.5\U03C0")
})

test_that("Format hour or radian", {
  res <- format_num(pi/2, "radians")
  expect_equal(res, "0.5\U03C0")
  
  res <- format_num(12, "clock")
  expect_equal(res, "12:00")
})
