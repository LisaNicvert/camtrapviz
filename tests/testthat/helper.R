library(camtrapR)
library(camtraptor)
library(lubridate)
library(dplyr)
library(testthat)


# Helper function ---------------------------------------------------------
check_plot_datetimes <- function(plt, 
                                 true_dates, # Should be POSIX
                                 setup = NULL,
                                 retrieval = NULL,
                                 row = 1) {
  # Check timezone --------------------
  ls <- ggplot2::layer_scales(plt)
  plot_tz <- ls$x$timezone
  
  # Check points ----------------------
  if (is.null(setup) & is.null(retrieval)) {
    # Index is 1
    layer_index <- 1
  } else {
    layer_index <- 2
  }
  
  # Get first point of plot
  ld <- ggplot2::layer_data(plt, layer_index)
  t1p <- ld$x[row]
  t1p <- as.POSIXct(t1p, plot_tz)
  t1p <- format(t1p, "%F %T") # Format as character
  
  t1d <- true_dates[row]
  t1d <- format(t1d, "%F %T") # Format as character
  
  # Check rectangles ------------------------------
  if (!is.null(setup) & !is.null(retrieval)) {
    # Get rectangle layer
    ld2 <- ggplot2::layer_data(plt, 1)
    
    # Plot setup
    setup1p <- ld2$xmin[row]
    setup1p <- as.POSIXct(setup1p, plot_tz)
    setup1p <- format(setup1p, "%F %T") # Format as character
    
    # Data setup
    setup1d <- setup[row]
    setup1d <- format(setup1d, "%F %T")
    
    # Plot retrieval
    retrieval1p <- ld2$xmax[row]
    retrieval1p <- as.POSIXct(retrieval1p, plot_tz)
    retrieval1p <- format(retrieval1p, "%F %T") # Format as character
    
    # Data retrieval
    retrieval1d <- retrieval[row]
    retrieval1d <- format(retrieval1d, "%F %T")
  } else {
    # Set to NULL
    setup1p <- NULL
    setup1d <- NULL
    retrieval1p <- NULL
    retrieval1d <- NULL
  }
  
  return(list(tz = plot_tz,
              plot_time = t1p,
              data_time = t1d,
              plot_setup = setup1p,
              data_setup = setup1d,
              plot_retrieval = retrieval1p,
              data_retrieval = retrieval1d))
}


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

kga_all <- read.csv("/home/lnicvert/test_camtrap_data/records_cameras/records_cameras_comma.csv")


# Create synthetic data ---------------------------------------------------
timestamp <- seq(as.POSIXct("2023-01-01 00:00:00",
                            tz = "Etc/GMT"),
                 as.POSIXct("2023-01-03 00:00:00",
                            tz = "Etc/GMT"),
                 by = "1 hour")
camera <- c(rep("C1", 20),
            rep("C2", 20),
            rep("C3", 9))

records <- data.frame(camera, timestamp)
records <- records |> 
  mutate(date = as.Date(timestamp)) |>
  mutate(time = as.character(format(timestamp, "%T")))

dfcam <- records |> 
  group_by(camera) |> 
  summarise(setup = min(timestamp),
            retrieval = max(timestamp))
