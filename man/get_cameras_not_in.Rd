% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_server_functions.R
\name{get_cameras_not_in}
\alias{get_cameras_not_in}
\title{Get non-shared cameras between two dataframes}
\usage{
get_cameras_not_in(dfrecords, dfcameras, cam_col_dfrec, cam_col_dfcam)
}
\arguments{
\item{dfrecords}{records dataframe}

\item{dfcameras}{cameras dataframe}

\item{cam_col_dfrec}{name of the cameras column in the records dataframe}

\item{cam_col_dfcam}{name of the cameras column in the cameras dataframe}
}
\value{
A named list with two components
\itemize{
\item \verb{$not_in_records}: cameras from \code{dfcameras} that are not in \code{dfrecords}.
If all cameras from \code{dfcameras} are in \code{dfrecords}, this is a character vector
of length zero.
\item \verb{$not_in_cameras}: cameras from \code{dfrecords} that are not in \code{dfcameras}
If all cameras from \code{dfrecords} are in \code{dfcameras}, this is a character vector
of length zero.
}
}
\description{
Using two dataframes in input, determine which cameras
are in one of the tables but not in the other one.
}
\examples{
dfrecords <- data.frame(camID = letters[2:7])
dfcam <- data.frame(cameras = letters[1:5])
get_cameras_not_in(dfrecords = dfrecords, 
                   dfcameras = dfcam,
                   cam_col_dfrec = "camID",
                   cam_col_dfcam = "cameras")
}
