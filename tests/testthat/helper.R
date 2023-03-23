library(camtrapR)
library(camtraptor)
library(lubridate)

# Load example data ------------------------------------------------------------
data("mica")
data("recordTableSample")
data("camtraps")

kga <- read.csv("/home/lnicvert/test_camtrap_data/csv_comma/KGA_S1_R1.csv",
                sep = ",")
kga$eventDate <- as_date(kga$eventDate)
kga$eventTime <- chron::times(kga$eventTime)

kga_cameras <- read.csv("/home/lnicvert/test_camtrap_data/csv_comma/metadata_snapshot.csv",
                        sep = ",")
kga_cameras <- kga_cameras %>% filter(locationID == "KGA")