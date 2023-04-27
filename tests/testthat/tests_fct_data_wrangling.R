# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-25
#
# Script Description: tests for data wrangling function


library(testthat)


test_that("Get cameras", {
  # Normal test
  cam1 <- c("C1", "C2", "C3")
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4"))
  
  # NA
  cam1 <- c("C1", "C2", "C3", NA)
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4", NA))
  
  # Duplicates
  cam1 <- c("C1", "C1", "C2", "C3")
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4"))
  
  # Reorder
  cam1 <- c("C2", "C3", "C1")
  cam2 <- c("C2", "C3", "C4")
  res <- get_cameras(cam1, cam2)
  expect_equal(res, c("C1", "C2", "C3", "C4"))
})

test_that("Get nspecies", {
  # With obstype
  df <- data.frame(obstype = c("animal", "animal", "animal", "animal", "blank"),
                   species = c("cat", "cat", "cow", "dog", NA))
  n <- get_nspecies(df, species_col = "species", obs_col = "obstype")
  
  expect_equal(n, 3)
  
  # Without obstype
  n <- get_nspecies(df, species_col = "species", keep_NA = TRUE)
  expect_equal(n, 4)
  
  # Without obstype and rm NA
  n <- get_nspecies(df, species_col = "species", keep_NA = FALSE)
  expect_equal(n, 3)
})
