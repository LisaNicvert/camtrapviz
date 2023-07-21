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

# Import and prepare data -------------------------------------------------

data(recordTableSample, package = "camtrapR")
data(camtraps, package = "camtrapR")

recordTableSample$DateTimeOriginal <- as.POSIXct(recordTableSample$DateTimeOriginal)
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
