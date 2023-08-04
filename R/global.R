# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-27
#
# Script Description: variables available in the Shiny app


# Resource path -----------------------------------------------------------
# shiny::addResourcePath("images", system.file("images",
#                                              package = "camtrapviz"))
css_dep <- function() {
  htmltools::htmlDependency(
    name = "css_dep",
    version = utils::packageVersion("camtrapviz"),
    package = "camtrapviz",
    src = "www",
    stylesheet = "theme.css"
  )
}

# Widgets dataframes --------------------------------------------------------


## Records -----------------------------------------------------------------

records_widgets <- data.frame(
  widget = c("cam_col",
             "spp_col",
             "date_col",
             "time_col",
             "timestamp_col",
             "tz_col",
             "lat_col",
             "lon_col",
             "crs_col",
             "setup_col",
             "retrieval_col",
             "count_col",
             "obs_col"),
  label = c("Camera",
            "Species",
            "Date",
            "Time",
            "Timestamp",
            "Timezone",
            "Latitude/y",
            "Longitude/x",
            "Coordinates format (CRS)",
            "Camera setup",
            "Camera retrieval",
            "Count",
            "Observation type"),
  details = c("Cameras ID",
              "Species names",
              "Capture events dates",
              "Capture events times",
              "Capture events dates and times",
              "Timezone of the times in the table",
              "Latitude or measure of the y coordinate if the coordinate reference system does not use latitude.",
              "Longitude or measure of the x coordinate  if the coordinate reference system does not use latitude.",
              "EPSG code for the coordinate reference system. Defaults to EPSG:4326 (WGS 84) which is the conventional CRS for latitude/longitude coordinates.",
              "Camera setup date (or date and time)",
              "Camera retrieval date (or date and time)",
              "Species count",
              "Type of the capture event (animal, blank, unknown...)"),
  empty_allowed = c(FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    TRUE,
                    TRUE,
                    FALSE,
                    TRUE,
                    TRUE,
                    TRUE,
                    TRUE),
  type = c("records",
           "records",
           "date_time",
           "date_time",
           "timestamp",
           "records",
           "cameras",
           "cameras",
           "cameras",
           "cameras",
           "cameras",
           "records",
           "records"),
  regex = c("station|deployment|camera",
            "^vernacularNames\\.en$|species", 
            "date", 
            "hour|time(?!stamp)", 
            "timestamp|datetime",
            NA,
            "lat|((^|[^[:alpha:]]+)y([^[:alpha:]]+|$))",
            "lon|((^|[^[:alpha:]]+)x([^[:alpha:]]+|$))",
            NA,
            "setup|start",
            "retrieval|end",
            "count",
            "observationType"),
  mica = c("deploymentID",
           "vernacularNames.en",
           NA,
           NA,
           "timestamp",
           "UTC",
           NA,
           NA,
           NA,
           NA,
           NA,
           "count",
           "observationType"),
  recordTableSample = c("Station",
                        "Species",
                        NA,
                        NA,
                        "DateTimeOriginal",
                        "UTC+8",
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA,
                        NA),
  cast =  c("as.character",
            "as.character",
            "as.Date",
            "times",
            "as.POSIXct",
            "as.numeric", # timezone?
            "as.numeric",
            "as.numeric",
            "as.character",
            "as.POSIXct",
            "as.POSIXct",
            "as.numeric",
            "as.character"),
  in_columns = c(TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 FALSE,
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE),
  width = c(12, 12,
            6, 6, 
            12,
            12,
            6, 6, 
            12,
            6, 6, 
            12, 12),
  class = c("nomargin", "nomargin",
            "nomarginleft", "nomarginright",
            "nomargin",
            "nomargin",
            "nomarginleft", "nomarginright",
            "nomargin",
            "nomarginleft", "nomarginright",
            "nomargin", "nomargin")
)


## Cameras -----------------------------------------------------------------

cameras_widgets <- records_widgets |> 
  dplyr::filter(widget == "cam_col" | type == "cameras") |>
  mutate(widget = paste(widget, "cov", sep = "_"))

# Set default camera columns for mica
cameras_widgets <- cameras_widgets |>
  mutate(mica = ifelse(widget == "crs_col_cov", 
                       4326, mica)) |>
  mutate(mica = ifelse(widget == "lat_col_cov", 
                       "latitude", mica)) |>
  mutate(mica = ifelse(widget == "lon_col_cov", 
                       "longitude", mica)) |>
  mutate(mica = ifelse(widget == "setup_col_cov", 
                       "start", mica)) |>
  mutate(mica = ifelse(widget == "retrieval_col_cov", 
                       "end", mica))
# Set default camera columns for recordTableSample
cameras_widgets <- cameras_widgets |>
  mutate(recordTableSample = ifelse(widget == "crs_col_cov", 
                                    32650, recordTableSample)) |>
  mutate(recordTableSample = ifelse(widget == "lat_col_cov", 
                                    "utm_y", recordTableSample)) |>
  mutate(recordTableSample = ifelse(widget == "lon_col_cov", 
                                    "utm_x", recordTableSample)) |>
  mutate(recordTableSample = ifelse(widget == "setup_col_cov", 
                                    "Setup_date", recordTableSample)) |>
  mutate(recordTableSample = ifelse(widget == "retrieval_col_cov", 
                                    "Retrieval_date", recordTableSample))


# Timezones codes ---------------------------------------------------------

# Get system specific timezones
tz_choices <- as.character(OlsonNames())

# Get Etc/GMT standard (offset from UTC)
gmt_choices <- grep(pattern = "^Etc/GMT", 
                    tz_choices, value = TRUE)
if (!all(is.null(gmt_choices))) { # If Etc/GMT exist on this system
  # Remove Etc/GMT from choices vector
  tz_choices <- tz_choices[!grepl(pattern = "^Etc/GMT", tz_choices)]
  
  if ("Etc/GMT" %in% gmt_choices) {
    # Remove +/-0 or 0 which are equivalent to GMT
    gmt_choices <- gmt_choices[!grepl(pattern = "^Etc/GMT(\\+|-)*0$", 
                                      gmt_choices)]
  }
  
  # Get numbers
  gmt_choices <- sort(gmt_choices) # Etc/GMT (zero) should be first
  num <- regmatches(gmt_choices,
                    regexpr(pattern = "(\\+|-)\\d+$", gmt_choices))
  if ("Etc/GMT" %in% gmt_choices) {
    num <- c("0", num) # Add zero for Etc/GMT
  }
  
  # Reorder
  if (length(num) == length(gmt_choices)) { # Check that all instances were matched
    gmt_choices <- gmt_choices[order(as.numeric(num))]
  }
  # Else, the not reordered GMT will be placed at the top.
  
  tz_choices <- c(gmt_choices, tz_choices)
}
