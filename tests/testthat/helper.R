library(camtrapR)
library(camtraptor)
library(lubridate)
library(dplyr)

# Load example data ------------------------------------------------------------
data("mica")
data("recordTableSample")
data("camtraps")

kga <- read.csv("/home/lnicvert/test_camtrap_data/csv_comma/KGA_S1_R1.csv",
                sep = ",")
kga$eventDate <- as_date(kga$eventDate)
kga$eventTime <- chron::times(kga$eventTime)

kga_cameras <- read.csv("/home/lnicvert/test_camtrap_data/csv_comma/kga_metadata.csv",
                        sep = ",")
kga_cameras$Setup.Date <- as_date(kga_cameras$Setup.Date,
                                  format = "%m/%d/%Y")
