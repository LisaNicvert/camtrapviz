% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_format_data.R
\name{clean_data}
\alias{clean_data}
\title{Clean data}
\usage{
clean_data(
  dat,
  cam_type = NULL,
  rec_type = NULL,
  only_shared_cam = FALSE,
  cam_col_dfrec = NULL,
  cam_col_dfcam = ifelse(only_shared_cam, cam_col_dfrec, NULL),
  split = FALSE,
  cam_cols = ifelse(split, cam_col_dfrec, NULL),
  reorder = FALSE,
  add_rowid = FALSE
)
}
\arguments{
\item{dat}{The data to clean. It can be either a list with one component \verb{$data}
or a \code{datapackage} object (inheriting list). Either way, the data
are in the \verb{$data} slot with two components:
\itemize{
\item \verb{$deployments} (cameras table)
\item \verb{$observations} (records table)
}}

\item{cam_type}{A named list containing the name of the
function to cast types for the cameras table. It is used
only if \code{split = FALSE}.
If \code{NULL}, the cameras table will not be modified or its columns
reordered.
The list's names are the names of the columns to cast
in \code{dat$data$deployments}.
For details on the content of this list, see the documentation of
the \code{cast_columns} function.}

\item{rec_type}{A named list containing the name of the
function to cast types for the records table.
If \code{split = TRUE}, the type conversion is performed before the split:
so future columns of the cameras table to cast should be in this list.
If \code{NULL}, the records table will not be modified or its columns
reordered.
The list's names are the names of the columns to cast
in \code{dat$data$observations}.
For details on the content of this list, see the documentation of
the \code{cast_columns} function.}

\item{only_shared_cam}{Logical; restrict final data to shared cameras
that are in \code{dat$data$deployments} and in \code{dat$data$observations}?}

\item{cam_col_dfrec}{Name of the column with cameras names in
records (needed only if \code{split} or \code{only_shared_cam} are \code{TRUE})}

\item{cam_col_dfcam}{Name of the column with cameras names in
cameras (needed only if \code{only_shared_cam} is \code{TRUE}). Defaults
to \code{cam_col_dfrec} if \code{only_shared_cam} is \code{TRUE}.
If \code{NULL} will be assumed to be the same as \code{cam_col_dfrec}.}

\item{split}{Logical; should the camera data be extracted from the
records table by splitting the data?}

\item{cam_cols}{A character vector of the columns in \code{dfrec} that should
be moved to the \code{dat$data$deployments} dataframe if \code{split = TRUE}.}

\item{reorder}{Reorder the columns indicated in \code{cam_type} or
\code{rec_type} at the beginning of the table?}

\item{add_rowid}{Should row IDs be added to the observations df?
If yes, row names in the form of "ID_xx" are added to the the dataframe.}
}
\value{
An object of the same type as the original input,
but where \code{dat$data$deployments} and \code{dat$data$observations} have been
cleaned as described above.
}
\description{
Cleans data by:
\itemize{
\item formatting cameras and records tables: casting specified columns
\item splitting records data in records and cameras (if needed)
\item if \code{only_shared_cam} is \code{TRUE}: selecting the subset of
cameras present in both records and cameras tables
\item if \code{reorder} is \code{TRUE}: moving columns in \code{rec_type} and \code{cam_type}
to the beginning of the table.
}
}
\examples{
# Create synthetic dataset
records <- data.frame(foo = 1:6,
                      species = c("pigeon", "mouse", "pigeon", "mouse", "mouse", "pigeon"),
                      date = c("2022-01-01", "2022-03-01", 
                               "2022-01-02", "2022-01-12", "2022-01-22",
                               "2022-01-03"),
                      time = c("10:22:01", "22:12:01",
                               "11:54:33", "07:14:38", "18:01:34", 
                               "12:11:34"),
                      camera = c("A", "A", "B", "B", "B", "C"))
cameras <- data.frame(camera = c("A", "B", "C"),
                      lat = c("20.12", "20.22", "22.34"),
                      lon = c("33.44", "33.45", "33.42"))
dat <- list(data = list(observations = records,
                        deployments = cameras))
                        
# Clean data
rec_type <- list(date = list("as.Date",
                             format = "\%Y-\%m-\%d"),
                 time = "times")
cam_type <- list(lat = "as.numeric",
                 lon = "as.numeric")

# Clean data converts columns to the appropriate types 
# and reorders columns
clean_data(dat,
           rec_type = rec_type,
           cam_type = cam_type)
}
\seealso{
\href{https://lisanicvert.github.io/camtrapviz/articles/read-and-clean-data.html}{\code{vignette("read-and-clean-data", package = "camtrapviz")}}
}
