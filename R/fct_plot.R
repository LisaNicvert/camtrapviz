# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: plotting functions

#' Plot species occurrences at cameras
#' 
#' Plot species occurrences at cameras as points in time.
#'
#' @param df The dataframe
#' @param cam_col Name of the camera column
#' @param datetime_col Name of the timestamp column. If it
#' is a datetime, it must be of class POSIXct. It can be `NULL` 
#' if `date_col` and `time_col` are provided.
#' @param date_col Name of the date column. It is assumed
#' to be of class `Date` (else, results are not guaranteed). 
#' It can be `NULL` if `datetime_col` is provided.
#' @param time_col Name of the time column. It is assumed
#' to be of class `times` (else, results are not guaranteed). 
#' It can be `NULL` if `datetime_col` is provided.
#' @param interactive Logical; make the plot interactive with `ggiraph`?
#' @param cam_vec A character vector of all cameras that should appear
#' on the plot (optional)
#' @param textsize Base text size for the axis text 
#' (axes titles are 1.2 times bigger)
#' @param ptsize Size for the points in the plot
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param dfcam Dataframe containing camera information. Must have at least one column
#' with the camera ID, the setup and the retrieval date.
#' @param cam_col_dfcam Name of the camera column in dfcam file.
#' @param setup_dfcam  Name of the setup column in dfcam file. The column 
#' must be coercible to POSIX (eg a Date). If it is not a POSIX, it will be 
#' converted to a POSIX attempting to use the timezone defined in `tz` (or its
#' default). If it is a POSIX, the timezone will be converted to `tz`.
#' @param retrieval_dfcam  Name of the retrieval column in dfcam file. The column 
#' must be coercible to POSIX (eg a Date). If it is not a POSIX, it will be 
#' converted to a POSIX attempting to use the timezone defined in `tz` (or its
#' default). If it is a POSIX, the timezone will be converted to `tz`.
#' @param alpha_rect transparency of the rectangle plotted for the sampling period
#' (when `dfcam` is given)
#' @param col_rect stroke color of the rectangle plotted for the sampling period
#' (when `dfcam` is given)
#' @param fill_rect fill color of the rectangle plotted for the sampling period
#' (when `dfcam` is given)
#' @param height_rect height of the rectangle plotted for the sampling period
#' (when `dfcam` is given). The height is centered on the middle. 2 corresponds
#' th the space between the y-axis ticks entirely filled.
#' @param points_col Name of the column to use for the color of data points 
#' (defaults to species column). If the graph is interactive,
#' this will also be displayed in the labels. It is interpred as a discrete color scale.
#' @param cols Color palette for the points. Can be either a palette
#' or a single color name.
#' If a single color name is provided, but `points_col` has several values,
#' this parameter will be overriden with the default palette.
#' If a palette of colors is provided in `cols`, the values will be matched with
#' `points_col` values. `cols` can be named with values of `points_col`
#' to map colors. Else, the mapping is done by alphabetical order.
#' If a palette is provided, but has more colors than the values of `points_col`,
#' a warning will be issued and a subset will be selected (based on names
#' of the palette that are in the values of `points_col`, if applicable).
#' If it has less colors, an error will be returned.
#' @param date_breaks Character describing x-axis ticks spacing (e.g. "10 day"). 
#' For the possible values, see documentation of `ggplot2::scale_y_datetime` for the
#' argument `date_breaks`.
#' @param text_x_angle Tilting angle for the x-axis text.
#' @param date_format Character string encoding the display format for x-axis
#' labels.
#' @param date_limits Vector of the lower and upper limit of the x-axis  (must be a
#' POSIX). The timezone should be the same as the timezone defined in the `timezone`
#' argument.
#' @param tz Timezone code to use for the data. If provided, the 
#' data will be converted to this timezone. If missing, will search
#' a timezone in `datetime_col` (if provided), else will default to
#' UTC (Etc/GMT for the R code).
#' The data timezone (or the default timezone) will also override
#' any timezone present in `setup_dfcam` or `retrieval_dfcam`.
#' @param tooltip_info Name of the column to display in the tooltip
#' when hovering points (if interactive is `TRUE`).
#' The data of this column will be displayed additionally to the 
#' of the point datetime. If `NULL`, only the datetime will be displayed.
#'
#' @details If `date_col` and `time_col` are provided along with
#' `datetime_col`, they will be ignored.
#' 
#' @return A `ggplot` object representing time on the x-axis
#' and cameras on the y-axis. Colors of the points correspond to 
#' different species.
#'
#' @export
#' 
#' @seealso [\code{vignette("plots", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/plots.html)
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' recordTableSample$DateTimeOriginal <- as.POSIXct(recordTableSample$DateTimeOriginal)
#' # Without camera sampling information
#' plot_points(recordTableSample, 
#'             cam_col = "Station", 
#'             datetime_col = "DateTimeOriginal")
#' # With camera sampling information
#' data("camtraps", package = "camtrapR")
#' camtraps$Setup_date <- as.Date(camtraps$Setup_date, 
#'                                format = "%d/%m/%Y") 
#' camtraps$Retrieval_date <- as.Date(camtraps$Retrieval_date, 
#'                                    format = "%d/%m/%Y")
#' plot_points(recordTableSample, 
#'             cam_col = "Station", 
#'             datetime_col = "DateTimeOriginal",
#'             dfcam = camtraps,
#'             setup_dfcam = "Setup_date",
#'             retrieval_dfcam = "Retrieval_date")
plot_points <- function(df, 
                        cam_col,
                        datetime_col = NULL,
                        tz = NULL,
                        date_col = NULL,
                        time_col = NULL,
                        cam_vec = NULL,
                        points_col = NULL,
                        dfcam = NULL,
                        cam_col_dfcam = cam_col,
                        setup_dfcam = "setup",
                        retrieval_dfcam = "retrieval",
                        interactive = FALSE,
                        textsize = 10,
                        text_x_angle = 0,
                        ptsize = 1.5,
                        date_breaks = NULL,
                        date_format = "%b %d",
                        date_limits = NULL,
                        alpha_rect = 0.5,
                        col_rect = "black",
                        height_rect = 0.8,
                        fill_rect = NA,
                        tooltip_info = points_col,
                        xlab = "Date",
                        ylab = "Camera",
                        cols = "black") {
  
  # Initialize plotting data
  dfp <- df
  
  # Check that the column names exist
  if( !(cam_col %in% colnames(df))) {
    stop("cam_col must be a column of df.")
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
  
  if (!is.null(dfcam)) {
    if( !(setup_dfcam %in% colnames(dfcam))) {
      stop("setup_dfcam must be a column of dfcam.")
    }
    
    if( !(retrieval_dfcam %in% colnames(dfcam))) {
      stop("retrieval_dfcam must be a column of dfcam.")
    }
    if( !(cam_col_dfcam %in% colnames(dfcam))) {
      stop("cam_col_dfcam must be a column of dfcam.")
    }
  }
  
  if (is.null(datetime_col)) { # no timestamp
    if (is.null(date_col) | is.null(time_col)) {
      stop("If datetime_col is not provided, date_col and time_col must be provided.")
    }
    if( !(date_col %in% colnames(df))) {
      stop("date_col must be a column of df.")
    }
    if( !(time_col %in% colnames(df))) {
      stop("time_col must be a column of df.")
    }
  } else { # If provided, timestamp must be in df
    if( !(datetime_col %in% colnames(df))) {
      stop("datetime_col must be a column of df.")
    }
  }
  
  # Set the timezone ---
  if (!is.null(datetime_col)) {
    data_tz <- attr(dfp[[datetime_col]], "tzone")
  } else {
    data_tz <- NULL
  }
  tz <- get_tz(custom_tz = tz, 
               data_tz = data_tz, 
               default_tz = "Etc/GMT")
  
  if (is.null(datetime_col)) { # no timestamp
    if("datetime_col" %in% colnames(dfp)) {
      warning("datetime_col already exists and this might interfer with plotting")
    }
    # Create a composite timestamp with custom tz
    dfp$datetime_col <- paste(as.character(dfp[[date_col]]), 
                               as.character(dfp[[time_col]]))
    
    dfp$datetime_col <- as.POSIXct(dfp$datetime_col,
                                    tz = tz) 
    # Change datetime_col value
    datetime_col <- "datetime_col"
    
  } else { # datetime_col not NULL
    dfp[[datetime_col]] <- add_tz(dfp[[datetime_col]],
                                   tz = tz,
                                   force_tz = TRUE)
  }
  
  # camera to factor if needed
  dfp <- format_factor(dfp, 
                       col = cam_col, 
                       levels = cam_vec)
  
  if (!is.null(points_col)) {
    # Coerce points col to factor
    dfp[[points_col]] <- factor(dfp[[points_col]])
    gg <- ggplot(dfp,
                 aes(x = .data[[datetime_col]], 
                     y = .data[[cam_col]],
                     col = .data[[points_col]]))
  } else {
    gg <- ggplot(dfp,
                 aes(x = .data[[datetime_col]], 
                     y = .data[[cam_col]]))
  }
  
  gg <- gg + scale_y_discrete(drop = FALSE)
  
  
  levels <- levels(dfp[[cam_col]])
  
  if (!is.null(dfcam)) {
    # Match dfcam and df cameras ---
    dfcam_cam <- unique(dfcam[[cam_col_dfcam]])
    
    if( !all(dfcam_cam %in% levels) ) {
      abs <- dfcam_cam[!dfcam_cam %in% levels]
      warning(paste(abs, collapse = ", "), " is/are present in dfcam but not in df. It/they will be removed.")
      dfcam <- dfcam |> 
        filter(.data[[cam_col_dfcam]] %in% levels)
    }
    
    if( !all(levels %in% dfcam_cam) ) {
      abs <- levels[!levels %in% dfcam_cam]
      warning(paste(abs, collapse = ", "), " is/are present in df but not in dfcam.")
    }
    
    # Coerce to factor
    dfcam[[cam_col_dfcam]] <- factor(dfcam[[cam_col_dfcam]],
                                            levels = levels)
    
    # Coerce setup and retrieval to POSIX ---
    dfcam[[setup_dfcam]] <- add_tz(dfcam[[setup_dfcam]], 
                                       tz = tz,
                                       force_tz = TRUE)
    dfcam[[retrieval_dfcam]] <- add_tz(dfcam[[retrieval_dfcam]], 
                                           tz = tz,
                                           force_tz = TRUE)
    
    if (!interactive) {
      gg <- gg +
        ggplot2::geom_rect(data = dfcam,
                           aes(xmin = .data[[setup_dfcam]],
                               xmax = .data[[retrieval_dfcam]],
                               ymin = as.numeric(.data[[cam_col_dfcam]]) - height_rect/2,
                               ymax = as.numeric(.data[[cam_col_dfcam]]) + height_rect/2
                           ),
                           inherit.aes = FALSE,
                           alpha = alpha_rect,
                           col = col_rect,
                           fill = fill_rect)
    } else {
      gg <- gg +
        ggiraph::geom_rect_interactive(data = dfcam,
                                       aes(xmin = .data[[setup_dfcam]],
                                           xmax = .data[[retrieval_dfcam]],
                                           ymin = as.numeric(.data[[cam_col_dfcam]]) - height_rect/2,
                                           ymax = as.numeric(.data[[cam_col_dfcam]]) + height_rect/2,
                                           tooltip = paste("From", .data[[setup_dfcam]], 
                                                           "to", .data[[retrieval_dfcam]])
                                       ),
                                       inherit.aes = FALSE,
                                       alpha = alpha_rect,
                                       col = col_rect,
                                       fill = fill_rect)
    }
    
  }
  
  if (interactive) {
    gg <- gg +
      {if (!is.null(points_col) & !is.null(tooltip_info)) 
        geom_point_interactive(aes(tooltip = paste(.data[[tooltip_info]], 
                                                   .data[[datetime_col]],
                                                   sep = ": "),
                                   data_id = .data[[cam_col]]),
                               show.legend = FALSE, size = ptsize)} +
      {if (!is.null(points_col) & is.null(tooltip_info)) 
        geom_point_interactive(aes(tooltip = .data[[datetime_col]],
                                   data_id = .data[[cam_col]]),
                               show.legend = FALSE, size = ptsize)} +
      {if (is.null(points_col)  & !is.null(tooltip_info)) 
        geom_point_interactive(aes(x = .data[[datetime_col]], 
                                   y = .data[[cam_col]],
                                   tooltip = paste(.data[[tooltip_info]], 
                                                   .data[[datetime_col]],
                                                   sep = ": "),
                                   data_id = .data[[cam_col]]),
                               show.legend = FALSE, size = ptsize,
                               col = cols)} +
      {if (is.null(points_col)  & is.null(tooltip_info)) 
        geom_point_interactive(aes(x = .data[[datetime_col]], 
                                   y = .data[[cam_col]],
                                   tooltip = .data[[datetime_col]],
                                   data_id = .data[[cam_col]]),
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
    if (length(cols) == 1 && length(ptcol) != 1) { # Override default
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
      if (length(cols) > length(ptcol)) {
        warning("cols do not match points_col length: they will be truncated")
        # Select color subset
        if (is.null(names(cols))) { # Select first values
          cols <- cols[1:length(ptcol)]
        } else { # Select based on names
          if (!all(as.character(ptcol) %in% names(cols))) {
            warning("Some values of points_col are not in cols names: the first values of cols will be selected.")
            cols <- cols[1:length(ptcol)]  
            names(cols) <- NULL
          } else { # All values are in the names
            cols <- cols[as.character(ptcol)]
          }
          
        }
      } else if (length(cols) < length(ptcol)) {
        stop("There are less colors in cols than there are values in points_col.")
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
                                date_labels = date_format) } +
    { if (!is.null(date_breaks) & is.null(date_limits) ) 
      ggplot2::scale_x_datetime(date_breaks = date_breaks,
                                limits = date_limits,
                                date_labels = date_format) } +
    { if (is.null(date_breaks)) 
      ggplot2::scale_x_datetime(limits = date_limits,
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

#' Barplot of species abundance
#'
#' Plot species abundance as a barplot.
#'
#' @param df The dataframe
#' @param spp_col Name of the species column
#' @param count_col Name of the count column (optional). If missing,
#' it will be assumed to be 1 for all observations.
#' @param obstype_col Name of the observation type column (optional).
#' If it is present, the function will plot only the observations
#' for which `obstype_col` is "animal". 
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
#' @seealso [\code{vignette("plots", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/plots.html)
#'
#' @examples
#' data("recordTableSample", package = "camtrapR")
#' plot_species_bars(recordTableSample,
#'                   spp_col = "Species")
plot_species_bars <- function(df, 
                              spp_col, 
                              count_col = NULL,
                              obstype_col = NULL,
                              interactive = FALSE,
                              NA_count_placeholder = NA) {
  
  # Summarize species
  dfp <- summarize_species(df, 
                           spp_col = spp_col, 
                           count_col = count_col,
                           obstype_col = obstype_col,
                           NA_count_placeholder = NA_count_placeholder)
  
  # Replace species with obs_type when species is NA
  if (!is.null(obstype_col)) {
    dfp[[spp_col]][is.na(dfp[[spp_col]])] <- dfp[[obstype_col]][is.na(dfp[[spp_col]])] 
  }
  
  if (interactive) {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], individuals),
                          y = individuals,
                          tooltip = paste(.data[[spp_col]], 
                                          individuals, 
                                          sep = ": ")
    )) +
      geom_col_interactive()
  } else {
    gg <- ggplot(dfp, aes(x = stats::reorder(.data[[spp_col]], individuals),
                          y = individuals)) +
      geom_col()
  }
  gg <- gg +
    coord_flip() +
    theme_linedraw() +
    ylab(ifelse(is.null(count_col), 
                "Capture events", "Individuals")) +
    theme(axis.title.y = element_blank())
  
  return(gg)
}

#' Plot cameras map
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
#' @param radius A vector of radii to use for the cirles. It can be named
#' with names correspond to camera names. If it is not named,
#' the labels are assumed to be in the same order as the cameras.
#' @param rescale rescale circles? If `TRUE`, radii will be linearly resized 
#' so that the maximum corresponds to 300m, and radii smaller than
#' 10 will be set to 10m. 
#' @param label vector of labels to display when hovering over the map 
#' points. It can be named with names correspond to camera names. 
#' If it is not named,
#' the labels are assumed to be in the same order as the cameras.
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
#' @seealso [\code{vignette("plots", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/plots.html)
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
                     radius = rep(3, nrow(df)),
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
  
  
  # If radius provided
  if (length(radius) != 0) {
    # Name radius as cameras if not named
    if (is.null(names(radius))) {
      names(radius) <- df[[cam_col]][1:length(radius)]
    }
    # Reorder and add missing cameras
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
  } else {
    # Name labels as cameras if not named
    if (is.null(names(label))) {
      names(label) <- df[[cam_col]][1:length(label)]
    }      
    # Reorder and add missing cameras
    label <- reorder_named_values(label, names = df[[cam_col]],
                                  keep_all_names = TRUE)
    
  }
  
  if (NA %in% label) {
    # NA label
    label[which(is.na(label))] <- "No data"
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
#' @param lon_col Name of the longitude (or the projected x-coordinate) 
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
#' @noRd
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

#' Plot activity data
#'
#' @param dfrec dataframe containing species records
#' @param time_dfrec Name of the column containing times in `dfrec`.
#' If it is a `chron::times` object, it will be converted to 
#' radians (0 -- 2pi) or clock times (0 -- 24) (depending on `unit`). Else, it is expected to be in
#' the same unit as `unit`.
#' @param hist_breaks Breaks for the histogram (in hours or radians
#' depending on the value of `unit`).
#' @param x_breaks Breaks for the x-axis ticks (in hours or radians
#' depending on the value of `unit`).
#' @param unit Unit for the plot (radians or clock). Will change the 
#' density on the y scale and the labels and limits of the x-axis.
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param freq Display count data (`TRUE`) or density (`FALSE`) ?
#' @param dffit Dataframe of fitted distribution
#' @param time_dffit Name of the column containing times in `dffit`.
#' This column must contain numeric values in radians and will be
#' converted if unit is `hours`.
#' @param y_fit  Name of the column containing density in `dffit`.
#' This column must contain numeric values corresponding to the
#' radians density and will be converted if unit is `hours`.
#' @param plot_hist Plot the histogram of counts?
#' @param plot_density Plot the activity curve?
#' @param interactive Make the plot interactive?
#' @param n Count for the observed data. It is required only when
#' you wish to plot the predicted species "density count" 
#' (so `plot_density` is `TRUE` and `freq` is `FALSE`).
#'
#' @return A ggplot representing a histogram of observed activity times
#' from `dfrec`.
#' 
#' @details
#' If the column `time_dfrec` if of type `times`, automatic conversion to 
#' clock times or radians will be performed. Else, the script trusts
#' the user for the provided unit.
#' + If plotting a histogram: when `freq` is `TRUE`, the height of
#' the bars in each category represents the count that falls in this category.
#' When `freq` is `FALSE`, the area of the bar in each category represents
#' the proportion of the data that falls in this category.
#' + If plotting a curve: when `freq` is `TRUE`: the area under the curve 
#' is equal to `n` when converting the x-scale to hours, even if the x axis
#' is in radians. So the height of the curve does not change between hours and radians. 
#' When `freq` is `FALSE`, the area under the curve is one and depends on the x unit
#' (so the height of the curve changes between hours and radians). This is to match the
#' original code of `plot.actmod`.
#' 
#' @export
#' 
#' @seealso [\code{vignette("activity-patterns", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/activity-patterns.html)
#'
#' @examples
#' library(activity)
#' library(chron)
#' data(recordTableSample, package = "camtrapR")
#' # Convert hours to times format
#' recordTableSample$Time <- chron::times(recordTableSample$Time)
#' # Select the desired species
#' PBE_records <- recordTableSample[recordTableSample$Species == "PBE", ]
#' 
#' # Plot only data
#' plot_activity(dfrec = PBE_records,
#'               time_dfrec = "Time",
#'               unit = "clock")
#' 
#' # Plot only data (density)
#' plot_activity(dfrec = PBE_records,
#'               time_dfrec = "Time",
#'               unit = "clock",
#'               freq = FALSE)
#' 
#' # Fit model
#' # Convert hours to times format
#' PBE_records$time_radians <- as.numeric(PBE_records$Time)*2*pi
#' vm <- activity::fitact(PBE_records$time_radians)
#' pdf_vm <- as.data.frame(vm@pdf)
#' 
#' # Plot data and fitted model in radians
#' plot_activity(dffit = pdf_vm,
#'               time_dffit = "x",
#'               y_fit = "y",
#'               unit = "radians",
#'               freq = FALSE,
#'               n = nrow(PBE_records))
#' 
#' # Plot data and fitted model in hours
#' plot_activity(dffit = pdf_vm,
#'               time_dffit = "x",
#'               y_fit = "y",
#'               unit = "clock",
#'               freq = FALSE,
#'               n = nrow(PBE_records))
plot_activity <- function(dfrec = NULL, 
                          time_dfrec = NULL, 
                          dffit = NULL,
                          time_dffit = NULL,
                          y_fit = NULL,
                          n = ifelse(!is.null(dfrec), nrow(dfrec), NULL),
                          plot_hist = ifelse(is.null(dfrec), FALSE, TRUE),
                          plot_density = ifelse(is.null(dffit), FALSE, TRUE),
                          hist_breaks = ifelse(unit == "clock", 1, (2*pi)/24),
                          x_breaks = ifelse(unit == "clock", 4, ((2*pi)/24)*4),
                          unit = c("clock", "radians"),
                          xlab = ifelse(unit == "clock", "Time (hours)", "Time (radians)"),
                          ylab = ifelse(freq, "Count", "Density"),
                          freq = TRUE,
                          interactive = FALSE) {
  
  # Check unit
  unit <- match.arg(unit)
  
  if (unit == "clock") {
      xmax <- 24
      x_breaks <- seq(0, xmax, by = x_breaks)
      labs <- format_hour(x_breaks)
  } else if (unit == "radians") {
    xmax <- 2*pi
    x_breaks <- seq(0, xmax, by = x_breaks)
    labs <- format_radian(x_breaks)
  }
  
  if (plot_hist) {
    # Check objects ---
    if (is.null(dfrec)) {
      stop("Need dfrec to plot histogram")
    }
    if (is.null(time_dfrec)) {
      stop("Need a time column time_dfrec to plot histogram")
    }
    
    # Prepare data (convert times if needed) ---
    data_plot <- dfrec
    
    if ("times" %in% class(dfrec[[time_dfrec]])) {
      # Transform to numeric
      data_plot[[time_dfrec]] <- as.numeric(data_plot[[time_dfrec]])
      
      # Convert depending on unit
      if (unit == "clock") {
        data_plot[[time_dfrec]] <- data_plot[[time_dfrec]]*24
      } else if (unit == "radians") {
        data_plot[[time_dfrec]] <- data_plot[[time_dfrec]]*2*pi
      }
    }
    
    # Plot ---
    if (interactive) {
      gg <- ggplot(data_plot) +
        {if (freq) ggiraph::geom_histogram_interactive(aes(x = .data[[time_dfrec]],
                                                           data_id = after_stat(x),
                                                           tooltip = paste0("Count: ", after_stat(count), "\n",
                                                                            "Time: ", format_num(after_stat(x) - hist_breaks/2, unit), 
                                                                            " \u2014 ", format_num(after_stat(x) + hist_breaks/2, unit))),
                                                       breaks = seq(0, xmax, by = hist_breaks))} +
        {if (!freq) ggiraph::geom_histogram_interactive(aes(x = .data[[time_dfrec]],
                                                            y = after_stat(density),
                                                            data_id = after_stat(x),
                                                            tooltip = paste0("Density: ", round(after_stat(density), 3), "\n",
                                                                             "Time: ", format_num(after_stat(x) - hist_breaks/2, unit), 
                                                                             " \u2014 ",  format_num(after_stat(x) + hist_breaks/2, unit))),
                                                        breaks = seq(0, xmax, by = hist_breaks))}
    } else {
      gg <- ggplot(data_plot) +
        {if (freq) ggplot2::geom_histogram(aes(x = .data[[time_dfrec]]),
                                           breaks = seq(0, xmax, by = hist_breaks))} +
        {if (!freq) ggplot2::geom_histogram(aes(x = .data[[time_dfrec]],
                                                y = after_stat(density)),
                                            breaks = seq(0, xmax, by = hist_breaks))}
    }
  }
  
  if (plot_density) {
    # Check objects ---
    if (is.null(dffit)) {
      stop("Need dffit to plot density curve")
    }
    if (is.null(time_dffit)) {
      stop("Need a time column time_dffit to plot density curve")
    }
    if (is.null(y_fit)) {
      stop("Need a density column y to plot density curve")
    }
    
    # Convert units (if needed) ---
    fdata_plot <- dffit
    if (unit == "clock") {
      fdata_plot[[time_dffit]] <- fdata_plot[[time_dffit]]*24/(2*pi) # convert times on the hour scale
      fdata_plot[[y_fit]] <- fdata_plot[[y_fit]]*(2*pi)/24 # convert density in density per hour
      # for the density to scale to one when x is in hours we need to convert divide by the scaling factor
    }
    
    if (freq) { # Plot counts
      if (unit == "clock") {
        fdata_plot[[y_fit]] <- fdata_plot[[y_fit]]*n
      } else if (unit == "radians") {
        fdata_plot[[y_fit]] <- fdata_plot[[y_fit]]*((2*pi)/24)*n # Convert to
        # hours as in the original plot.actmod
        # This means that the area under the curves integrates to n when the scale is
        # in hours, so the frequency density is similar to the hour frequency density
      }
    }
    
    # Create ggplot (optional step) ---
    if (!plot_hist) { # gg object has not been created yet
      gg <- ggplot(data = fdata_plot)
    }
    
    gg <- gg +
      geom_line(data = fdata_plot,
                aes(x = .data[[time_dffit]], 
                    y = .data[[y_fit]]))
    
  }
  
  gg <- gg +
    scale_x_continuous(breaks = x_breaks,
                       limits = c(0, xmax),
                       labels = labs) +
    xlab(xlab) +
    ylab(ylab) +
    theme_linedraw()
  gg
  
}

#' Plot species diversity at cameras
#'
#' @param df The dataframe with diversity indices per camera
#' @param div_col Name of the column containing the diversity index to plot
#' @param cam_col Name of the column containing the cameras names
#' @param interactive Make the plot interactive?
#' @param cam_vec A character vector of all cameras that should appear
#' on the plot (optional)
#' @param round Rounding option for the diversity indices
#' displayed when hovering. `NULL` is for no rounding.
#'
#' @return A ggplot object representing diversity indices
#' as bars (in x) following the different cameras (in y).
#' 
#' @export
#'
#' @seealso [\code{vignette("plots", package = "camtrapviz")}](https://lisanicvert.github.io/camtrapviz/articles/plots.html)
#'
#' @examples
#' # Create synthetic data ---
#' df <- data.frame("camera" = c("C1", "C2", "C4", "C3"),
#'                  "count" = c(1, 4, 2, 22))
#' 
#' plot_diversity(df, 
#'                div_col = "count", 
#'                cam_col = "camera")
plot_diversity <- function(df, 
                           div_col, 
                           cam_col,
                           cam_vec = NULL,
                           round = 3,
                           interactive = FALSE) {
  
  # camera to factor if needed
  dfp <- format_factor(df, 
                       col = cam_col, 
                       levels = cam_vec)
  gg <- ggplot(dfp)
  
  if (interactive) {
    gg <- gg +
      ggiraph::geom_col_interactive(aes(x = .data[[cam_col]], 
                                        y = .data[[div_col]],
                                        tooltip = ifelse(is.null(round),
                                                         .data[[div_col]],
                                                         round(.data[[div_col]], round)),
                                        data_id = .data[[cam_col]]))
  } else {
    gg <- gg + 
      geom_col(aes(x = .data[[cam_col]], y = .data[[div_col]]))
  }
    
  gg <- gg +  
    ggplot2::scale_x_discrete(drop = FALSE) +
    coord_flip() +
    theme_linedraw()
  
}


# Helpers -----------------------------------------------------------------

#' Format hour
#' 
#' Formats numeric to be printed as an hour
#' 
#' @param num Numeric vector
#'
#' @return Formatted numeric as (0)x:00
#' @noRd
format_hour <- function(num) {
  hour <- sprintf(num, fmt = "%02d")
  hour <- paste(hour, "00", sep = ":")
  return(hour)
}

#' Format radian
#' 
#' Formats numeric to be printed as an hour
#' 
#' @param num Numeric vector
#'
#' @return Formatted numeric as x pi
#' @noRd
format_radian <- function(num) {
  rad <- round(num/pi, 3)
  rad <- paste0(rad, "\u03C0")
  # Replace 0 pi and 1 pi
  rad[rad == "0\u03C0"] <- "0"
  rad[rad == "1\u03C0"] <- "\u03C0"
  return(rad)
}

#' Format numeric to hour or radian
#'
#' @param num Numeric
#' @param type Format to radian or hour?
#'
#' @return The formatted numeric
#' @noRd
format_num <- function(num, type = c("clock", "radians")) {
  
  type <- match.arg(type)
  
  if(type == "clock") {
    format_hour(num)
  } else if (type == "radians") {
    format_radian(num)
  }
}


#' Format factor in dataframe
#'
#' Given a dataframe and a specific column of this dataframe,
#' will transform this to a factor with the given levels (except
#' if already is a factor)
#'
#' @param df The df
#' @param col The column of the df to transform to factor
#' @param levels THe factor levels
#'
#' @return The df with col as factor. In case some values of col 
#' are not in levels they will be discarded.
#'
#' @noRd
format_factor <- function(df, 
                          col,
                          levels = NULL) {
  
  res <- df
  
  if (!is.null(levels)) {
    # Filter data (keep only data in levels)
    res <- res |> filter(.data[[col]] %in% levels)
    
    # Define levels based on levels
    levels_final <- unique(levels)
  } else {
    if (!is.factor(df[[col]])) { # If not already a factor
      # Define levels based on df
      levels_final <- sort(unique(df[[col]]))
    } else { # Keep factor levels if it is a factor
      levels_final <- levels(df[[col]])
    }
  }
  
  # Coerce cameras to factor
  res[[col]] <- factor(res[[col]],
                       levels = levels_final)
  return(res)
}
