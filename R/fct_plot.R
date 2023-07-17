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
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param caminfo Dataframe containing camera information. Must have at least one column
#' with the camera ID, the setup and the retrieval date.
#' @param caminfo_camera_col Name of the camera column in caminfo file.
#' @param caminfo_setup  Name of the setup column in caminfo file. The column 
#' must be coercible to POSIX (eg a Date).
#' @param caminfo_retrieval  Name of the retrieval column in caminfo file. The column 
#' must be coercible to POSIX (eg a Date).
#' @param alpha_rect transparency of the rectangle plotted for the sampling period
#' (when `caminfo` is given)
#' @param col_rect stroke color of the rectangle plotted for the sampling period
#' (when `caminfo` is given)
#' @param fill_rect fill color of the rectangle plotted for the sampling period
#' (when `caminfo` is given)
#' @param height_rect height of the rectangle plotted for the sampling period
#' (when `caminfo` is given). The height is centered on the middle. 2 corresponds
#' th the space between the y-axis ticks entirely filled.
#' @param points_col Name of the column to use for the color of data points 
#' (defaults to species column). If the graph is interactive,
#' this will also be displayed in the labels. It is interpred as a discrete color scale.
#' @param cols Color palette for the points. Can be either a palette
#' with as many colors as the levels of `points_col` or a single color name.
#' In case `cols` is the vector it can be named with values of `points_col`
#' and will correspont to the mapping between colors and `points_col`.
#' Else the mapping is done by alphabetical order.
#' @param date_breaks Character describing x-axis ticks spacing (e.g. "10 day"). 
#' For the possible values, see documentation of `ggplot2::scale_y_datetime` for the
#' argument `date_breaks`.
#' @param text_x_angle Tilting angle for the x-axis text.
#' @param date_format Character string encoding the display format for x-axis
#' labels.
#' @param date_limits Vector of the lower and upper limit of the x-axis  (must be a
#' POSIX). The timezone should be the same as the timezone defined in the `timezone`
#' argument.
#' @param timezone Timezone code. For the possible values, refer to the
#' `timezone` argument of `ggplot2::scale_y_datetime`.
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
#' # Without camera samplking information
#' plot_points(recordTableSample, 
#'             camera_col = "Station", 
#'             timestamp_col = "DateTimeOriginal")
#' # With camera sampling information
#' data("camtraps", package = "camtrapR")
#' camtraps$Setup_date <- as.Date(camtraps$Setup_date, 
#'                                format = "%d/%m/%Y") 
#' camtraps$Retrieval_date <- as.Date(camtraps$Retrieval_date, 
#'                                    format = "%d/%m/%Y")
#' plot_points(recordTableSample, 
#'             camera_col = "Station", 
#'             timestamp_col = "DateTimeOriginal",
#'             caminfo = camtraps,
#'             caminfo_setup = "Setup_date",
#'             caminfo_retrieval = "Retrieval_date")
plot_points <- function(df, 
                        camera_col,
                        timestamp_col,
                        date_col = NULL,
                        time_col = NULL,
                        cameras_list = NULL,
                        points_col = NULL,
                        caminfo = NULL,
                        caminfo_camera_col = camera_col,
                        caminfo_setup = "setup",
                        caminfo_retrieval = "retrieval",
                        interactive = FALSE,
                        textsize = 10,
                        text_x_angle = 0,
                        ptsize = 1.5,
                        date_breaks = NULL,
                        date_format = "%b %d",
                        date_limits = NULL,
                        timezone = "UTC",
                        alpha_rect = 0.5,
                        col_rect = "black",
                        height_rect = 0.8,
                        fill_rect = NA,
                        xlab = "Date",
                        ylab = "Camera",
                        cols = "black") {
  
  # Initialize plotting data
  dfp <- df
  
  # Check that the column names exist
  if( !(camera_col %in% colnames(df))) {
    stop("camera_col must be a column of df.")
  }
  if (!is.null(points_col)) {
    if( !(points_col %in% colnames(df))) {
      stop("points_col must be a column of df.")
    }
  }
  if (!is.null(points_col)) {
    if( !(points_col %in% colnames(df))) {
      stop("points_col must be a column of df.")
    }
  }
  
  if (!is.null(caminfo)) {
    if( !(caminfo_setup %in% colnames(caminfo))) {
      stop("caminfo_setup must be a column of caminfo.")
    }
    
    if( !(caminfo_retrieval %in% colnames(caminfo))) {
      stop("caminfo_retrieval must be a column of caminfo.")
    }
    if( !(caminfo_camera_col %in% colnames(caminfo))) {
      stop("caminfo_camera_col must be a column of caminfo.")
    }
  }
  
  if (missing(timestamp_col) || is.null(timestamp_col)) { # no timestamp
    if (is.null(date_col) | is.null(time_col)) {
      stop("If timestamp_col is not provided, date_col and time_col must be provided.")
    }
    if( !(date_col %in% colnames(df))) {
      stop("date_col must be a column of df.")
    }
    if( !(time_col %in% colnames(df))) {
      stop("time_col must be a column of df.")
    }
  } else { # If provided, timestamp must be in df
    if( !(timestamp_col %in% colnames(df))) {
      stop("timestamp_col must be a column of df.")
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
    
    # Define levels based on cameras_list
    levels <- unique(cameras_list)
  } else {
    
    if (!is.factor(dfp[[camera_col]])) { # If not already a factor
      # Define levels based on df
      levels <- sort(unique(dfp[[camera_col]]))
    } else { # Keep factor levels if it is a factor
      levels <- levels(dfp[[camera_col]])
    }
  }
  
  # Coerce cameras to factor
  dfp[[camera_col]] <- factor(dfp[[camera_col]],
                              levels = levels)
  
  if (!is.null(points_col)) {
    gg <- ggplot(dfp,
                 aes(x = .data[[timestamp_col]], 
                     y = .data[[camera_col]],
                     col = .data[[points_col]]))
  } else {
    gg <- ggplot(dfp,
                 aes(x = .data[[timestamp_col]], 
                     y = .data[[camera_col]]))
  }
  
  gg <- gg + scale_y_discrete(drop = FALSE)
  
  
  if (!is.null(caminfo)) {
    # Match caminfo and df cameras ---
    caminfo_cam <- unique(caminfo[[caminfo_camera_col]])
    
    if( !all(caminfo_cam %in% levels) ) {
      abs <- caminfo_cam[!caminfo_cam %in% levels]
      warning(paste(abs, collapse = ", "), " is/are present in caminfo but not in df. It/they will be removed.")
      caminfo <- caminfo |> 
        filter(.data[[caminfo_camera_col]] %in% levels)
    }
    
    if( !all(levels %in% caminfo_cam) ) {
      abs <- levels[!levels %in% caminfo_cam]
      warning(paste(abs, collapse = ", "), " is/are present in df but not in caminfo.")
    }
    
    # Corece to factor
    caminfo[[caminfo_camera_col]] <- factor(caminfo[[caminfo_camera_col]],
                                            levels = levels)
    
    # Coerce setup and retrieval to POSIX ---
    caminfo[[caminfo_setup]] <- as.POSIXct(caminfo[[caminfo_setup]])
    caminfo[[caminfo_retrieval]] <- as.POSIXct(caminfo[[caminfo_retrieval]])
    
    if (!interactive) {
      gg <- gg +
        ggplot2::geom_rect(data = caminfo,
                           aes(xmin = .data[[caminfo_setup]],
                               xmax = .data[[caminfo_retrieval]],
                               ymin = as.numeric(.data[[caminfo_camera_col]]) - height_rect/2,
                               ymax = as.numeric(.data[[caminfo_camera_col]]) + height_rect/2
                           ),
                           inherit.aes = FALSE,
                           alpha = alpha_rect,
                           col = col_rect,
                           fill = fill_rect)
    } else {
      gg <- gg +
        ggiraph::geom_rect_interactive(data = caminfo,
                                       aes(xmin = .data[[caminfo_setup]],
                                           xmax = .data[[caminfo_retrieval]],
                                           ymin = as.numeric(.data[[caminfo_camera_col]]) - height_rect/2,
                                           ymax = as.numeric(.data[[caminfo_camera_col]]) + height_rect/2,
                                           tooltip = paste("From", .data[[caminfo_setup]], 
                                                           "to", .data[[caminfo_retrieval]])
                                       ),
                                       inherit.aes = FALSE,
                                       alpha = alpha_rect,
                                       col = col_rect,
                                       fill = fill_rect)
    }
    
  }
  
  if (interactive) {
    gg <- gg +
      {if (!is.null(points_col)) geom_point_interactive(aes(tooltip = paste(.data[[points_col]], 
                                                                         .data[[timestamp_col]],
                                                                         sep = ": "),
                                                         data_id = .data[[camera_col]]),
                                                     show.legend = FALSE, size = ptsize)} +
      {if (is.null(points_col)) geom_point_interactive(aes(x = .data[[timestamp_col]], 
                                                        y = .data[[camera_col]],
                                                        tooltip = .data[[timestamp_col]],
                                                        data_id = .data[[camera_col]]),
                                                    show.legend = FALSE, size = ptsize,
                                                    col = cols)}
      
  } else {
    gg <- gg +
      { if(is.null(points_col)) geom_point(show.legend = FALSE, 
                                           size = ptsize, 
                                           col = cols) } +
      { if(!is.null(points_col)) geom_point(show.legend = FALSE, 
                                           size = ptsize) }
  }
  
  # Define color palette
  if (!is.null(points_col)) {  # Points colors defined by another column
    ptcol <- sort(unique(dfp[[points_col]]))
    if (length(cols) == 1) { # Override default
      # This is RColorBrewer palette Dark2 (v1.1.3)
      pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", 
               "#66A61E", "#E6AB02", "#A6761D", "#666666")
      if (length(pal) >= length(ptcol)) {
        cols <- pal[1:length(ptcol)]
      } else {
        cols <- grDevices::colorRampPalette(pal)(length(ptcol))
      }
      names(cols) <- ptcol
    } else { # Check color length
      if (length(cols) != length(ptcol)) {
        warning("cols do not match points_col length")
      }
      # Add names (implicit: colors are ordered as the species in
      # alphabetical order)
      if (is.null(names(cols))) {
        names(cols) <- ptcol
      }
    }
    # Add color scale
    gg <- gg +
      ggplot2::scale_color_manual(values = cols,
                                  breaks = names(cols))
  }
  
  gg <- gg +
    theme_linedraw(base_size = textsize) + 
    { if (!is.null(date_breaks) & !is.null(date_limits)) 
      ggplot2::scale_x_datetime(breaks = seq(date_limits[1],
                                             date_limits[2],
                                             by = date_breaks),
                                limits = date_limits,
                                timezone = timezone,
                                date_labels = date_format) } +
    { if (!is.null(date_breaks) & is.null(date_limits) ) 
      ggplot2::scale_x_datetime(date_breaks = date_breaks,
                                limits = date_limits,
                                timezone = timezone,
                                date_labels = date_format) } +
    { if (is.null(date_breaks)) 
      ggplot2::scale_x_datetime(limits = date_limits,
                                timezone = timezone,
                                date_labels = date_format) } +
    theme(axis.text.x = element_text(angle = text_x_angle, 
                                     hjust = ifelse(text_x_angle %% 180 == 0,
                                                    0.5, 1),
                                     vjust = ifelse(text_x_angle %% 90 == 0,
                                                    0.5, 1))
          ) +
    xlab(xlab) +
    ylab(ylab)
  
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
