% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data_wrangling.R
\name{get_cameras}
\alias{get_cameras}
\title{Get unique cameras vector}
\usage{
get_cameras(cam1, cam2, NA.last = TRUE)
}
\arguments{
\item{cam1}{Character vector of camera names.}

\item{cam2}{Character vector of camera names.}

\item{NA.last}{Value of the NA.last argument in \code{unique} and
\code{sort} functions}
}
\value{
A vector of unique cameras that are present in both vectors.
NAs are kept and placed in the last position. Alphabetical order is used.
}
\description{
Get a unique vector of all cameras present either in one list or in the
other.
}
\examples{
cam1 <- c("C1", "C2", "C3", NA)
cam2 <- c("C2", "C3", "C4")
get_cameras(cam1, cam2)
}
