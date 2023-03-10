% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server_functions.R
\name{filter_cameras_in_both_tables}
\alias{filter_cameras_in_both_tables}
\title{Filter data to keep only cameras in both tables}
\usage{
filter_cameras_in_both_tables(
  records,
  cameras,
  cam_col_records,
  cam_col_cameras
)
}
\arguments{
\item{records}{Records dataframe}

\item{cameras}{Cameras dataframe}

\item{cam_col_records}{Name of the columns with camera values in records}

\item{cam_col_cameras}{Name of the columns with camera values in cameras}
}
\value{
A list of 2 dataframes with filtered values.
}
\description{
Selects rows where cameras are in both tables
}
