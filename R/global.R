# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-27
#
# Script Description: variables available in the Shiny app


# Widgets dataframes --------------------------------------------------------


## Records -----------------------------------------------------------------

records_widgets <- data.frame(
  widget = c("spp_col",
             "cam_col",
             "date_col",
             "time_col",
             "timestamp_col",
             "crs_col",
             "lat_col",
             "lon_col",
             "setup_col",
             "retrieval_col",
             "count_col",
             "obs_col"),
  label = c("Species",
            "Camera",
            "Date",
            "Time",
            "Timestamp",
            "Coordinates format (CRS)",
            "Latitude/y coordinate",
            "Longitude/x coordinate",
            "Setup date/datetime",
            "Retrieval date/datetime",
            "Count",
            "Observation type"),
  empty_allowed = c(FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
                    FALSE,
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
           "cameras",
           "cameras",
           "cameras",
           "cameras",
           "cameras",
           "records",
           "records"),
  regex = c("^vernacularNames\\.en$|species", 
            "station|deployment|camera",
            "date", 
            "hour|time(?!stamp)", 
            "timestamp|datetime",
            NA,
            "lat|((^|[^[:alpha:]]+)y([^[:alpha:]]+|$))",
            "lon|((^|[^[:alpha:]]+)x([^[:alpha:]]+|$))",
            "setup|start",
            "retrieval|end",
            "count",
            "observationType"),
  mica = c("vernacularNames.en",
           "deploymentID",
           NA,
           NA,
           "timestamp",
           NA,
           NA,
           NA,
           NA,
           NA,
           "count",
           "observationType"),
  recordTableSample = c("Species",
                        "Station",
                        NA,
                        NA,
                        "DateTimeOriginal",
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
            "as.character",
            "as.numeric",
            "as.numeric",
            "as.Date",
            "as.Date",
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
                 TRUE,
                 TRUE,
                 TRUE,
                 TRUE)
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

# CRS formats -------------------------------------------------------------
epsg_df <-rgdal::make_EPSG()

epsg <- as.list(epsg_df$code)
names(epsg) <- paste0(epsg_df$note, " (EPSG:", epsg ,")")