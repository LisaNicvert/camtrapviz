# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-07-21
#
# Script Description: generate plots to add to the README


# Libraries ---------------------------------------------------------------
library(camtrapviz)
library(ggiraph)
library(ggplot2)
library(here)
library(mapview)
library(dplyr)

# Import and prepare data -------------------------------------------------

data(recordTableSample, package = "camtrapR")
data(camtraps, package = "camtrapR")

recordTableSample$DateTimeOriginal <- as.POSIXct(recordTableSample$DateTimeOriginal,
                                                 tz = "Etc/GMT-8")
recordTableSample$Date <- as.Date(recordTableSample$Date)
recordTableSample$Time <- chron::times(recordTableSample$Time)

camtraps$Setup_date <- as.Date(camtraps$Setup_date, format = "%d/%m/%Y")
camtraps$Retrieval_date <- as.Date(camtraps$Retrieval_date, format = "%d/%m/%Y")


# Plot points -------------------------------------------------------------

(p <- plot_points(recordTableSample, 
                  camera_col = "Station", 
                  points_col = "Species",
                  timestamp_col = "DateTimeOriginal",
                  caminfo = camtraps,
                  caminfo_setup = "Setup_date",
                  caminfo_retrieval = "Retrieval_date", 
                  date_breaks = "10 day",
                  date_limits = as.POSIXct(c("2009-04-01",
                                             "2009-05-17"),
                                           tz = "UTC"),
                  textsize = 12,
                  interactive = TRUE))
ggsave(here("man/figures/plot_points.png"),
       width = 15, height = 10, 
       unit = "cm")

# Interactive plot (for website)
ggiraph::girafe(ggobj = p)


# Plot maps ---------------------------------------------------------------
m <- plot_map(camtraps, 
              cam_col = "Station", 
              display_camnames = TRUE,
              lat_col = "utm_y", lon_col = "utm_x", 
              crs = 32650)
mapshot(m, file = here("man/figures/map.png"))


# Species bars ------------------------------------------------------------
(p <- plot_species_bars(recordTableSample, 
                        spp_col = "Species", 
                        interactive = TRUE))

ggsave(here("man/figures/plot_spp_bars.png"),
       width = 15, height = 10, 
       unit = "cm")

# Interactive plot (for website)
ggiraph::girafe(ggobj = p)


# Activity plot -----------------------------------------------------------

# Convert times to radians
recordTableSample <- recordTableSample |> 
  mutate(time_radians = as.numeric(Time)*2*pi,
         .after = Time)

PBE_records <- recordTableSample[recordTableSample$Species == "PBE", ]

vm <- activity::fitact(PBE_records$time_radians)
pdf_vm <- as.data.frame(vm@pdf)

plot_activity(fitted_data = pdf_vm,
              times_fit = "x",
              y_fit = "y",
              true_data = PBE_records,
              times_true = "Time",
              unit = "clock",
              freq = TRUE,
              interactive = TRUE)

ggsave(here("man/figures/plot_activity.png"),
       width = 12, height = 10, 
       unit = "cm")
