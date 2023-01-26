# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-01-26
#
# Script Description: test camtraptor package


# Librairies --------------------------------------------------------------
library(camtraptor)
library(dplyr)
library(lubridate)


# Load data ---------------------------------------------------------------
data(mica)


# Data exploration --------------------------------------------------------
# It is a nested list
names(mica)

# Data are here
mica$data$deployments
mica$data$media
# mica$data$media$filePath # pictures URM
mica$data$observations

# Data exploration --------------------------------------------------------
get_species(mica)

map_dep(
  mica,
  feature = "n_individuals",
  species = "Anas platyrhynchos",
  sex = "female"
)

## Camera operation -------------
camop <- get_cam_op(mica) # Used in camtrapR too
mica$data$deployments$locationName

camop[1:4, 1:2]

cam_op_with_locationID <- get_cam_op(mica, 
                                     station_col = "locationID")
mica$data$deployments$locationID

cam_op_with_locationID[1:4, 1:2]

## Filter predicates -------------------
pred(arg = "a", value = 5)

get_n_obs(mica,
          pred_in("locationName",
                  c("B_DL_val 5_beek kleine vijver", 
                    "B_DL_val 3_dikke boom")))

map_dep(mica,
        feature = "n_species", 
        pred_or(
          pred_in("locationName",
                  c("B_DL_val 5_beek kleine vijver", "B_DL_val 3_dikke boom")),
          pred_lt("latitude", 50.7)))

## Record table -----------------------------
get_record_table(mica)
get_record_table(mica, 
                 minDeltaTime = 60, 
                 deltaTimeComparedTo = "lastRecord")

get_record_table(mica, 
                 minDeltaTime = 60, 
                 deltaTimeComparedTo = "lastIndependentRecord")

get_record_table(mica, 
                 exclude = c("grey heron", "Anas platyrhynchos", "mens"))

get_record_table(mica, stationCol = "locationID")

## Maps ---------------
map_dep(mica,
        feature = "n_obs")

map_dep(mica,
        feature = "n_species")

map_dep(
  mica,
  "n_obs",
  species = "Anas platyrhynchos",
  sex = c("female", "unknown")
)

# RAI
map_dep(mica,
        feature = "rai",
        species = "Anas platyrhynchos")

map_dep(mica,
        feature = "rai_individuals",
        species = "Anas platyrhynchos")

# Effort
map_dep(mica, 
        feature = "effort",
        effort_unit = "hour")
