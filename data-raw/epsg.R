## code to prepare `epsg_df` dataset goes here
epsg_df <-rgdal::make_EPSG()
epsg <- as.list(epsg_df$code)
names(epsg) <- paste0(epsg_df$note, " (EPSG:", epsg ,")")

usethis::use_data(epsg,
                  internal = TRUE, 
                  overwrite = TRUE)
