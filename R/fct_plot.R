# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: plotting functions

#' Plot points
#' 
#' Plot species occurrences at cameras as points
#'
#' @param df The dataframe
#' @param camera_col Name of the camera column
#' @param spp_col Name of the species column
#' @param timestamp_col Name of the timestamp column. If it
#' is a datetime, it must be of class POSIXct. It can be `NULL` 
#' if `date_col` and `time_col` are provided.
#' @param date_col Name of the date column. It is assumed
#' to be of class `Date` (else, results are not guaranteed). 
#' It can be `NULL` if `timestamp_col` is provided.
#' @param time_col Name of the time column. It is assumed
#' to be of class `times` (else, results are not guaranteed). 
#' It can be `NULL` if `timestamp_col` is provided.
#' @param interactive Logical; make the plot interactive with `ggiraph`?
#' @param cameras_list A character vector of all cameras that should appeat
#' on the plot (optional)
#' @param textsize Base text size for the axis text 
#' (axes titles are 1.2 times bigger)
#' @param ptsize Size for the points in the plot
#'
#' @details If `date_col` and `time_col` are provided along with
#' `timestamp_col`, they will be ignored.
#' 
#' @return A `ggplot` object representing time on the x-axis
#' and cameras on the y-axis. Colors of the points correspond to 
#' different species.
#'
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' recordTableSample$DateTimeOriginal <- as.POSIXct(recordTableSample$DateTimeOriginal)
#' plot_points(recordTableSample, 
#'             camera_col = "Station", 
#'             timestamp_col = "DateTimeOriginal", 
#'             spp_col = "Species")
plot_points <- function(df, 
                        camera_col,
                        spp_col,
                        timestamp_col,
                        cameras_list = NULL,
                        date_col = NULL,
                        time_col = NULL,
                        interactive = TRUE,
                        textsize = 10,
                        ptsize = 1.5) {
  
  # Initialize plotting data
  dfp <- df
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if (is.null(date_col) | is.null(time_col)) {
      stop("If timestamp_col is not provided, date_col and time_col must be provided.")
    }
  }
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if("timestamp_col" %in% colnames(dfp)) {
      warning("timestamp_col already exists and this might interfer with plotting")
    }
    # Create a composite timestamp
    dfp$timestamp_col <- paste(as.character(dfp[[date_col]]), 
                               as.character(dfp[[time_col]]))
    
    dfp$timestamp_col <- as.POSIXct(dfp$timestamp_col)
    
    # Change timestamp_col value
    timestamp_col <- "timestamp_col"
  }
  
  if (!is.null(cameras_list)) {
    # Filter data (keep only cameras in cameras_list)
    dfp <- dfp |> filter(.data[[camera_col]] %in% cameras_list)
    
    # Coerce cameras to factor
    levels <- unique(cameras_list)
    dfp[[camera_col]] <- factor(dfp[[camera_col]],
                                levels = levels)
  }
  
  if (interactive) {
    gg <- ggplot(dfp, aes(x = .data[[timestamp_col]], 
                          y = .data[[camera_col]],
                          col = .data[[spp_col]],
                          tooltip = paste(.data[[spp_col]], 
                                          .data[[timestamp_col]],
                                          sep = ": "),
                          data_id = .data[[camera_col]]
    )) +
      scale_y_discrete(drop = FALSE) +
      geom_point_interactive(show.legend = FALSE, size = ptsize)
      
  } else {
    gg <- ggplot(dfp, aes(x = .data[[timestamp_col]], 
                          y = .data[[camera_col]],
                          col = .data[[spp_col]])) +
      geom_point(show.legend = FALSE, size = ptsize)
  }
  
  gg <- gg +
    theme_linedraw() + 
    theme(axis.text = element_text(size = textsize),
          axis.title = element_text(size = textsize*1.2)) +
    xlab("Date") +
    ylab("Camera")
  
  return(gg)
}

#' Plot species bars
#'
#' Plot the barplot of species abundance
#'
#' @param df The dataframe
#' @param spp_col Name of the species column
#' @param count_col Name of the count column (optional). If missing,
#' it will be assumed to be 1 for all observations.
#' @param obs_col Name of the observation type column (optional).
#' If it is present, the function will plot only the observations
#' for which `obs_col` is "animal". 
#' @param interactive Logical; make the plot interactive with `ggiraph`?
#' @param NA_count_placeholder Value with which to replace NAs present
#' in the column containing counts. If not specified, NA is the default
#' and species which have NA in counts will have a NA count.
#'
#' @return A `ggplot` object representing horizontal bars of species
#' count. The x-axis is the observed number of individuals and the y-axis
#' are the different species.
#' 
#' @export
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' plot_species_bars(recordTableSample,
#'                   spp_col = "Species")
plot_species_bars <- function(df, 
                              spp_col, 
                              count_col = NULL,
                              obs_col = NULL,
                              interactive = TRUE,
                              NA_count_placeholder = NA) {
  
  # Summarize species
  dfp <- summarize_species(df, 
                           species_col = spp_col, 
                           count_col = count_col,
                           obs_col = obs_col,
                           NA_count_placeholder = NA_count_placeholder)
  
  # Replace species with obs_type when species is NA
  if (!is.null(obs_col)) {
    dfp[[spp_col]][is.na(dfp[[spp_col]])] <- dfp[[obs_col]][is.na(dfp[[spp_col]])] 
  }
  
  if (interactive) {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], n_individuals),
                          y = n_individuals,
                          tooltip = paste(.data[[spp_col]], 
                                          n_individuals, 
                                          sep = ": ")
    )) +
      geom_col_interactive()
  } else {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], n_individuals),
                          y = n_individuals)) +
      geom_col()
  }
  gg <- gg +
    coord_flip() +
    theme_linedraw() +
    ylab("Count") +
    theme(axis.title.y = element_blank())
  
  return(gg)
}

#' Plot map
#' 
#' Plot a leaflet map representing cameras' coordinates as points.
#' 
#' @param df A dataframe containing cameras information
#' @param lat_col Name of the latitude (or the projected y-coordinate) 
#' column
#' @param lon_col Name of the longitude (or the projected y-coordinate) 
#' column
#' @param crs EPSG code for the coordinate reference system (CRS)
#' Defaults to EPSG:4326, which is the code for WGS84 standard.
#' @param cam_col Name of the camera name column
#' @param color color for the points (can be a unique value or a character vector,
#' in the same order as the rows of df)
#' @param radius A named vector of radii tu use for the cirles. Names
#' correspond to camera names.
#' @param rescale rescale circles? If `TRUE`, radii will be linearly resized 
#' so that the maximum corresponds to 300m, and radii smaller than
#' 10 will be set to 10m. 
#' @param label label to display when hovering over the map points
#' @param width Map width
#' @param height Map height
#' @param popup A vector of characters to display in the popup for 
#' each camera. It must have the same length as the number of cameras
#' in df and it must be ordered in the same way as the cameras in df.
#' @param display_camnames Display camera names on the map?
#'
#' @return a `leaflet` map representing cameras as points.
#' If the CRS of the input data is different from EPSG:4326 (WGS84), 
#' data are re-projected using WGS84.
#' When hovering over a camera, it becomes red and its name is shown.
#' When clicking on a camera, a popup displaying the camera name appears.
#' 
#' @export
#' 
#' @examples
#' data(camtraps, package = "camtrapR")
#' plot_map(camtraps,
#'          lat_col = "utm_y", 
#'          lon_col = "utm_x",
#'          crs = 32650, # Here we use the EPSG code for UTM zone 50N
#'          cam_col = "Station")
plot_map <- function(df, 
                     lat_col, lon_col, 
                     popup = NULL,
                     display_camnames = FALSE,
                     crs = 4326,
                     width = NULL, height = NULL,
                     cam_col,
                     color = "black",
                     radius = 3,
                     rescale = FALSE,
                     label = NULL) {
  
  if (length(color) != 1) {
    if (length(color) != nrow(df)) {
      stop("color must be either the same length as df or of length one")
    }
  }
  
  if(!is.null(crs)) { # Specify the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col),
                          crs = as.numeric(crs))
    # Reproject in WGS84 (a.k.a. EPSG:4326)
    df_sf <- sf::st_transform(df_sf, 4326)
  } else { # Let leaflet choose the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col))
  }
  
  # Color as a vector
  if (length(color) == 1) {
    color <- rep(color, nrow(df_sf))
  }
  
  # Reorder radii if named
  if (length(radius) != 0 & !is.null(names(radius))) {
    radius <- reorder_named_values(radius, names = df[[cam_col]],
                                         keep_all_names = TRUE)
  }
  
  # Set custom color and placeholder for NA values
  if (NA %in% radius) {
    # NA color
    
    color[which(is.na(radius))] <- "purple"
    # Placeholder value
    radius[which(is.na(radius))] <- 0
  }
  
  radius <- unname(radius)
  
  # Rescale radii measures
  if (rescale) {
    radius <- 3 + radius*(20/max(radius)) # set max to 20
    # radius[radius < 3] <- 3 # Set min to 3
  }
  
  if (is.null(label)) {
    label <- df_sf[[cam_col]]
  }
  
  if (is.null(popup)) {
    popup = paste0("Camera: ", df_sf[[cam_col]])
  }
  
  if (display_camnames) {
    label_pt <- popup
    labeldir_pt <- "left"
  } else {
    label_pt <- label
    labeldir_pt <- "auto"
  }
  
  
  lmap <- leaflet(df_sf,
                  width = width, height = height) |> 
    addTiles() |> 
    addCircleMarkers(data = df_sf,
               label = label_pt,
               layerId = df_sf[[cam_col]],
               popup = popup,
               popupOptions = popupOptions(closeOnClick = TRUE),
               stroke = FALSE,
               fillOpacity = 0.8,
               fillColor = color,
               radius = radius,
               labelOptions = labelOptions(direction = labeldir_pt)
               )
  
  if (display_camnames) {
    lmap <- lmap |> 
      addLabelOnlyMarkers(data = df_sf,
                          label = df_sf[[cam_col]],
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      direction = 'right', 
                                                      offset =  c(6, 0),
                                                      textOnly = TRUE))
  }
  
  lmap
}


#' Update map
#' 
#' Update a leaflet map

#' @param map_id ID of the map to update
#'
#' @param session Shiny session
#' @param df A dataframe containing cameras information
#' @param lat_col Name of the latitude (or the projected y-coordinate) 
#' column
#' @param lon_col Name of the longitude (or the projected y-coordinate) 
#' column
#' @param crs EPSG code for the coordinate reference system (CRS)
#' Defaults to EPSG:4326, which is the code for WGS84 standard.
#' @param cam_col Name of the camera name column
#' @param color color for the points (can be a unique value or a character vector,
#' in the same order as the rows of df)
#' @param radius A named vector of radii tu use for the cirles. Names
#' correspond to camera names.
#' @param rescale rescale circles? If `TRUE`, radii will be linearly resized 
#' so that the maximum corresponds to 300m, and radii smaller than
#' 10 will be set to 10m. 
#' @param label label to display when hovering over the map points
#' @param popup A vector of characters to display in the popup for 
#' each camera. It must have the same length as the number of cameras
#' in df and it must be ordered in the same way as the cameras in df.
#' @param display_camnames Display camera names on the map?
#'
#' @return a `leaflet` map representing cameras as points.
#' If the CRS of the input data is different from EPSG:4326 (WGS84), 
#' data are re-projected using WGS84.
#' When hovering over a camera, it becomes red and its name is shown.
#' When clicking on a camera, a popup displaying the camera name appears.
update_map <- function(map_id,
                       session,
                       df, 
                       lat_col, lon_col, 
                       popup = NULL,
                       display_camnames = FALSE,
                       crs = 4326,
                       cam_col,
                       color = "black",
                       radius = 3,
                       rescale = FALSE,
                       label = NULL) {
  if (length(color) != 1) {
    if (length(color) != nrow(df)) {
      stop("color must be either the same length as df or of length one")
    }
  }
  
  if(!is.null(crs)) { # Specify the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col),
                          crs = as.numeric(crs))
    # Reproject in WGS84 (a.k.a. EPSG:4326)
    df_sf <- sf::st_transform(df_sf, 4326)
  } else { # Let leaflet choose the CRS
    df_sf <- sf::st_as_sf(df, 
                          coords = c(lon_col, lat_col))
  }
  
  # Color as a vector
  if (length(color) == 1) {
    color <- rep(color, nrow(df_sf))
  }
  
  # Reorder radii if named
  if (length(radius) != 0 & !is.null(names(radius))) {
    radius <- reorder_named_values(radius, names = df[[cam_col]],
                                   keep_all_names = TRUE)
  }
  
  # Set custom color and placeholder for NA values
  if (NA %in% radius) {
    # NA color
    
    color[which(is.na(radius))] <- "purple"
    # Placeholder value
    radius[which(is.na(radius))] <- 0
  }
  
  radius <- unname(radius)
  
  # Rescale radii measures
  if (rescale) {
    radius <- 3 + radius*(20/max(radius)) # set max to 20
    # radius[radius < 3] <- 3 # Set min to 3
  }
  
  if (is.null(label)) {
    label <- df_sf[[cam_col]]
  }
  
  if (is.null(popup)) {
    popup = paste0("Camera: ", df_sf[[cam_col]])
  }
  
  if (display_camnames) {
    label_pt <- popup
    labeldir_pt <- "left"
  } else {
    label_pt <- label
    labeldir_pt <- "auto"
  }
  
  leafletProxy(mapId = map_id, session) |>
    clearMarkers() |>
    addCircleMarkers(data = df_sf,
                     label = label_pt,
                     layerId = df_sf[[cam_col]],
                     popup = popup,
                     popupOptions = popupOptions(closeOnClick = TRUE),
                     stroke = FALSE,
                     fillOpacity = 0.8,
                     fillColor = color,
                     radius = radius,
                     labelOptions = labelOptions(direction = labeldir_pt)
    )
  
  if (display_camnames) {
    leafletProxy(mapId = map_id, session) |>
      addLabelOnlyMarkers(data = df_sf,
                          label = lapply(paste0("<span style='color:", color, 
                                                "'>", df_sf[[cam_col]], 
                                                "<span>"), 
                                         htmltools::HTML),
                          labelOptions = labelOptions(noHide = TRUE, 
                                                      direction = 'right', 
                                                      offset =  c(6, 0),
                                                      textOnly = TRUE))
  }
}
