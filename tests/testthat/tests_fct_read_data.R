# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-04-07
#
# Script Description: tests read data functions

library(testthat)

test_that("Read data with NAs", {
  df <- data.frame(letters = letters[1:5],
                   num = 1:5)
  df$letters[1] <- ""
  df$letters[2] <- "  "
  df$num[5] <- "NA"
  
  tfile <- paste0(tempfile(), ".csv")
  write.csv(df, file = tfile, row.names = FALSE)
  
  dat <- read_data(tfile, sep_rec = ",", NA_strings = c("", "NA"))
  
  expect_true(is.na(dat$data$observations$letters[1]))
  expect_false(is.na(dat$data$observations$num[2]))
  expect_true(is.na(dat$data$observations$num[5]))
})
