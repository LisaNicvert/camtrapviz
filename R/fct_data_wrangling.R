# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-25
#
# Script Description: functions to handle camera trap data

#' Get cameras
#' 
#' Get a unique of all cameras present either in one list or in the
#' other.
#'
#' @param cam1 Character vector of camera names.
#' @param cam2 Character vector of camera names.
#' @param NA.last Value of the NA.last argument in `unique` and 
#' `sort` functions
#'
#' @return A vector of unique cameras that are present in both vectors.
#' NAs are kept and placed in the last position. Alphabetical order is used.
#' 
#' @export
#'
#' @examples
#' cam1 <- c("C1", "C2", "C3", NA)
#' cam2 <- c("C2", "C3", "C4")
#' get_cameras(cam1, cam2)
get_cameras <- function(cam1, cam2, NA.last = TRUE) {
  
  cam1r <- unique(cam1, NA.last = NA.last)
  cam2r <- unique(cam2, NA.last = NA.last)
  
  cam <- sort(unique(c(cam1r, cam2r)), na.last = NA.last)
  return(cam)
}


#' Get species count
#'
#' @param df a dataframe
#' @param species_col name of the species column from the dataframe
#' @param obs_col name of the observation type column from the dataframe
#' @param keep_NA count NAs in the species length?
#'
#' @return The number of species. If `obs_col` is provided, will
#' ignore all species with `obs_col` value different from `animal`.
#' If `keep_NA`, will count NA in the total species count.
#' 
#' @export
#'
#' @examples
#' df <- data.frame(obstype = c("animal", "animal", "animal", "animal", "blank"),
#'                  species = c("cat", "cat", "cow", "dog", NA))
#' get_nspecies(df, species_col = "species", obs_col = "obstype")
get_nspecies <- function(df, species_col, obs_col = NULL,
                         keep_NA = FALSE) {
  
  if (!is.null(obs_col)) {
    # Filter to get only animal species
    species <- df[[species_col]][df[[obs_col]] == "animal"]
  } else {
    species <- df[[species_col]]
  }
  
  if (!keep_NA) {
    # Remove Nas
    species <- species[!is.na(species)]
  }
  
  n <- length(unique(species))
  
  return(n)
}
