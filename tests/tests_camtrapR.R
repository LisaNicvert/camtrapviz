# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-01-26
#
# Script Description: test camtrapR


# Librairies --------------------------------------------------------------
library(camtrapR)
library(here)
library(sf)
library(mapview)
library(raster)
library(purrr)
library(DT)
library(knitr)
library(ggplot2)

# Load data ---------------------------------------------------------------
data("camtraps")
camtraps


# Test species names ------------------------------------------------------
checkSpeciesNames (speciesNames = c("catafricanwild", 
                                    "black rhinoceros"),
                   searchtype   = "common")

# Column names from RecordTable -------------------------------------------
##    Station Species    DateTimeOriginal       Date     Time delta.time.secs
##   delta.time.mins delta.time.hours delta.time.days   Directory   FileName n_images

# cf https://jniedballa.github.io/camtrapR/articles/camtrapr3.html for columns content description

# Can also include animal counts ()


# Camera operation --------------------------------------------------------
# The camera operation matrix is a day-by-station matrix that states how many cameras 
# were active at a station on a given day
data(camtraps)

dateFormat <- "dmy"    # requires lubridate package
# alternatively, use "%d/%m/%Y" (from base R)

camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = dateFormat
)

# as a reminder, these are the dates in our station information table
camtraps[,-which(colnames(camtraps) %in% c("utm_y", "utm_x"))]


# Plot camera operation ---------------------------------------------------

camtraps_typo <- camtraps
# replace 2009 with 2008 in setup date of station A
camtraps_typo$Setup_date[1] <- gsub("2009", "2008", camtraps_typo$Setup_date[1])

camop_typo <- cameraOperation(CTtable      = camtraps_typo,
                              stationCol   = "Station",
                              setupCol     = "Setup_date",
                              retrievalCol = "Retrieval_date",
                              writecsv     = FALSE,
                              hasProblems  = TRUE,
                              dateFormat   = dateFormat
)
camtrapR:::camopPlot(camop_typo)


# Detection history (for occupancy) ---------------------------------------
data(recordTableSample)
# recordTableSample not defined
camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = dateFormat
)

DetHist1 <- detectionHistory(recordTable         = recordTableSample,
                             camOp                = camop_no_problem,
                             stationCol           = "Station",
                             speciesCol           = "Species",
                             recordDateTimeCol    = "DateTimeOriginal",
                             species              = "VTA",
                             occasionLength       = 7,
                             day1                 = "station",
                             includeEffort        = FALSE
)

DetHist1


#  Spatial Capture-Recapture analyses -------------------------------------
data(recordTableIndividualSample)

camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = TRUE,
                                 dateFormat   = dateFormat
)

sdh <- spatialDetectionHistory(recordTableIndividual = recordTableIndividualSample, 
                               species               = "LeopardCat",  
                               output                = "binary",
                               camOp                 = camop_problem, 
                               CTtable               = camtraps,
                               stationCol            = "Station", 
                               speciesCol            = "Species",
                               Xcol                  = "utm_x",
                               Ycol                  = "utm_y",
                               individualCol         = "Individual",
                               recordDateTimeCol     = "DateTimeOriginal",
                               recordDateTimeFormat  = "%Y-%m-%d %H:%M:%S",
                               occasionLength        = 10, 
                               day1                  = "survey",
                               includeEffort         = TRUE,
                               timeZone              = "Asia/Kuala_Lumpur"
)

summary(sdh)
plot(sdh, tracks = TRUE)


# Multi-season occupancy --------------------------------------------------
# load multi-season data
data(camtrapsMultiSeason)
data(recordTableSampleMultiSeason)

# also, for clarity, lets remove all unnecessary columns from the record table
recordTableSampleMultiSeason <- recordTableSampleMultiSeason[, c("Station", "Species", "DateTimeOriginal")]

# create camera operation matrix
camop_season <- cameraOperation(CTtable         = camtrapsMultiSeason,
                                stationCol   = "Station",
                                setupCol     = "Setup_date",
                                sessionCol   = "session",
                                retrievalCol = "Retrieval_date",
                                hasProblems  = TRUE,
                                dateFormat   = dateFormat
)

# plot camera operation matrix
par(oma = c(0,7,0,0))
camtrapR:::camopPlot(camop_season)

# make multi-season detection history
DetHist_multi <- detectionHistory(recordTable   = recordTableSampleMultiSeason,
                                  camOp                = camop_season,
                                  stationCol           = "Station",
                                  speciesCol           = "Species",
                                  species              = "VTA",
                                  occasionLength       = 10,
                                  day1                 = "station",
                                  recordDateTimeCol     = "DateTimeOriginal",
                                  includeEffort        = TRUE,
                                  scaleEffort          = FALSE,
                                  timeZone             = "UTC",
                                  unmarkedMultFrameInput = TRUE
)

DetHist_multi


# Multi-season spatial recapture ------------------------------------------

# Can be done


# Data exploration and viz ------------------------------------------------
# https://jniedballa.github.io/camtrapR/articles/camtrapr4.html

# load sample camera trap station table
data(camtraps)
# load sample record table
data(recordTableSample)

Mapstest1 <- detectionMaps(CTtable     = camtraps,
                           recordTable  = recordTableSample,
                           Xcol         = "utm_x",
                           Ycol         = "utm_y",
                           stationCol   = "Station",
                           speciesCol   = "Species",
                           printLabels  = TRUE,
                           richnessPlot = TRUE,    # by setting this argument TRUE
                           speciesPlots = FALSE,
                           addLegend    = TRUE
)

# Can write a shapefile
# define shapefile name
shapefileName <- "recordShapefileTest"

# projection: WGS 84 / UTM zone 50N = EPSG:32650
# see: https://spatialreference.org/ref/epsg/32650/
shapefileProjection <- 32650

# run detectionMaps with shapefile creation
Mapstest3 <- detectionMaps(CTtable            = camtraps,
                           recordTable         = recordTableSample,
                           Xcol                = "utm_x",
                           Ycol                = "utm_y",
                           stationCol          = "Station",
                           speciesCol          = "Species",
                           richnessPlot        = FALSE,         # no richness plot
                           speciesPlots        = FALSE,         # no species plots
                           writeShapefile      = TRUE,          # but shapefile creation
                           shapefileName       = shapefileName,
                           shapefileDirectory  = here("tests/shp"),     # change this in your scripts!   
                           shapefileProjection = shapefileProjection
)

detections_sf <- st_as_sf(Mapstest3, coords = c(Xcol, Ycol))
# detections_sf <- st_read(dsn   = here("tests/shp"), 
#                          layer = shapefileName)

mapview(detections_sf)
mapview(detections_sf, zcol = "n_species")

#  Making and using a SpatialPointsDataFrame ------------------------------

# convert sf object to sp object
detections_spdf <- as(detections_sf, "Spatial")

# create a sample raster and extract data from it (if the raster package is available)
raster_test <- raster(x = extend(extent(detections_spdf), y = 500), 
                      nrows = 10, ncols = 10)
# fill raster with random numbers
values(raster_test) <- rpois(n = 100, 
                             lambda = seq(1, 100))    

# plot raster
plot(raster_test,
     main = "some raster with camera trap stations",
     ylab = "UTM N",     # needs to be adjusted if data are not in UTM coordinate system
     xlab = "UTM E")     # needs to be adjusted if data are not in UTM coordinate system

# add points to plot
points(detections_spdf, pch = 16)

# add point labels
text(x      = coordinates(detections_spdf)[,1],
     y      = coordinates(detections_spdf)[,2],
     labels = detections_spdf$Station,
     pos = 1)

# extracting raster values. See ?extract for more information
detections_spdf$raster_value <- extract(x = raster_test, y = detections_spdf)

# checking the attribute table
detections_spdf@data
  

#  Single-species activity plots ------------------------------------------

# activityDensity uses the function densityPlot from the overlap package.

# we first pick a species for our activity trials
species4activity <- "PBE"    # = Prionailurus bengalensis, Leopard Cat

activityDensity(recordTable = recordTableSample,
                species     = species4activity)
activityHistogram (recordTable = recordTableSample,
                   species     = species4activity)
# This function uses functions from the plotrix package to create the clock face. 
# Records are aggregated to the full hour (as in activityHistogram).
activityRadial(recordTable  = recordTableSample,
               species      = species4activity,
               lwd          = 3       # adjust line with of the plot
)


#  Two-species activity plots ---------------------------------------------
# The functions overlapPlot and overlapEst from the overlap package are used for that purpose.
# define species of interest
speciesA_for_activity <- "VTA"    # = Viverra tangalunga, Malay Civet
speciesB_for_activity <- "PBE"    # = Prionailurus bengalensis, Leopard Cat

# create activity overlap plot
activityOverlap (recordTable = recordTableSample,
                 speciesA    = speciesA_for_activity,
                 speciesB    = speciesB_for_activity,
                 writePNG    = FALSE,
                 plotR       = TRUE,
                 add.rug     = TRUE
)


#  Survey summary report --------------------------------------------------
camop_problem <- cameraOperation(CTtable      = camtraps,
                                 stationCol   = "Station",
                                 setupCol     = "Setup_date",
                                 retrievalCol = "Retrieval_date",
                                 hasProblems  = TRUE,
                                 dateFormat   = "dmy")
reportTest <- surveyReport (recordTable          = recordTableSample,
                            CTtable              = camtraps,
                            camOp                = camop_problem,   # new argument since v2.1
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            setupCol             = "Setup_date",
                            retrievalCol         = "Retrieval_date",
                            CTDateFormat         = "%d/%m/%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S" #,
                            #CTHasProblems        = TRUE    # deprecated in v2.1
)



# 5. Multi-species occupancy models ---------------------------------------
# https://jniedballa.github.io/camtrapR/articles/camtrapr5.html
data("camtraps")

camop_no_problem <- cameraOperation(CTtable      = camtraps,
                                    stationCol   = "Station",
                                    setupCol     = "Setup_date",
                                    retrievalCol = "Retrieval_date",
                                    hasProblems  = FALSE,
                                    dateFormat   = "dmy"
)

data("recordTableSample")

# list of detection histories
DetHist_list <- lapply(unique(recordTableSample$Species), FUN = function(x) {
  detectionHistory(
    recordTable         = recordTableSample,
    camOp                = camop_no_problem,
    stationCol           = "Station",
    speciesCol           = "Species",
    recordDateTimeCol    = "DateTimeOriginal",
    species              = x,     # this gets modifies by lapply
    occasionLength       = 7,
    day1                 = "station",
    datesAsOccasionNames = FALSE,
    includeEffort        = TRUE,
    scaleEffort          = FALSE,
    timeZone             = "Asia/Kuala_Lumpur"
  )}
)

# assign species names to the list items
names(DetHist_list) <- unique(recordTableSample$Species)
ylist <- lapply(DetHist_list, FUN = function(x) x$detection_history)

# Create some fake covariates (only for demonstration):
sitecovs <- camtraps[, c(1:3)]
sitecovs$elevation <- c(300, 500, 600)
sitecovs[, c(2:4)] <- scale(sitecovs[,-1])   # scale numeric covariates
sitecovs$fact <- factor(c("A", "A", "B"))    # categorical covariate

data_list <- list(ylist    = ylist,
                  siteCovs = sitecovs,
                  obsCovs  = list(effort = DetHist_list[[1]]$effort))

# text file to save the model
modelfile1 <- here("tests/occ/model.txt")

mod.jags <- communityModel(data_list,
                           occuCovs = list(fixed = "utm_y", ranef = "elevation"),
                           detCovsObservation = list(fixed = "effort"),
                           intercepts = list(det = "ranef", occu = "ranef"),
                           modelFile = modelfile1)
summary(mod.jags)

fit.jags <- fit(mod.jags,
                n.iter = 5000,
                n.burnin = 2500,
                chains = 3)

fit_summary <- summary(fit.jags)

# Estimates
fit_summary$statistics
fit_summary$quantiles

# Plots
plot_effects(mod.jags,
             fit.jags,
             submodel = "state")
plot_coef(mod.jags,
          fit.jags,
          submodel = "state",
          combine = T)

# Can also be done using Nimble