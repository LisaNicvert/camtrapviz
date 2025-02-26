% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data_wrangling.R
\name{summarize_species}
\alias{summarize_species}
\title{Summarize species}
\usage{
summarize_species(
  df,
  spp_col,
  cam_col = NULL,
  obstype_col = NULL,
  count_col = NULL,
  ncam = NULL,
  by_cam = FALSE,
  keep_all_camera_levels = FALSE,
  dfcam = NULL,
  cam_col_dfcam = ifelse(is.null(dfcam), NULL, cam_col),
  duration_col_dfcam = ifelse(is.null(dfcam), NULL, "sampling_length"),
  NA_count_placeholder = NA
)
}
\arguments{
\item{df}{the observation dataframe to summarize}

\item{spp_col}{Name of the species column}

\item{cam_col}{Name of the column containing camera codes (optional if
\code{by_cam} is \code{FALSE})}

\item{obstype_col}{Name of the observation type column (optional)}

\item{count_col}{Name of the column containing species count (optional)}

\item{ncam}{Number of cameras to take into account when computing
the proportion of cameras the species was ween on. If \code{NULL},
defaults to the number of cameras present in the \code{df} (not needed
if \code{cam_col} is not provided).}

\item{by_cam}{Should the values be summarized by camera? If yes,
there will be one row per cameras-species}

\item{keep_all_camera_levels}{If there is a camera on which no species were
seen, should it be present in the output? Not needed
if \code{cam_col} is not provided.}

\item{dfcam}{Dataframe containing information about the cameras
sampling length. If it is provided, then \code{cam_col_dfcam} and
\code{duration_col_dfcam} must be in its column names.}

\item{cam_col_dfcam}{Column name containing cameras names in \code{dfcam}}

\item{duration_col_dfcam}{Column name containing sampling duration
in \code{dfcam}}

\item{NA_count_placeholder}{Value with which to replace NAs present
in the column containing counts. Defaults to NA (i.e. values are not
replaced).}
}
\value{
A table summarizing species information with the following columns:
\itemize{
\item Species (named like \code{spp_col}): species identity
(same as the \code{spp_col} input column)
\item Observation type (present only if \code{obstype_col} is not \code{NULL} and named like
\code{obstype_col}): observation type (same as the \code{obstype_col} input column)
\item \code{sightings}: number of rows where the species was photographed.
\item \code{individuals}: count of individuals observed on all
pictures (using the input \code{count_col} column).
If \code{count_col} is \code{NULL}, it contains the same values as \code{sightings}.
If there are NAs in the input  \code{count_col}, they will propagate in \code{individuals}
(unless a value is specified in \code{NA_count_placeholder}).
}

If \code{by_cam} is \code{FALSE}, the following rows are also present:
\itemize{
\item \code{n_cameras} (present only if \code{cam_col} is not \code{NULL}) : the number of cameras the species was seen on.
\item \code{prop_cam} (present only if \code{cam_col} is not \code{NULL}): the proportion of cameras the species was seen on.
If \code{ncam} is provided, then it uses \code{ncam} as the total number of cameras.
}

If \code{by_cam} is \code{TRUE}, the following rows are also present:
\itemize{
\item \code{sightings_prop}: the proportion of sightings represented by the species at the camera.
\item \code{individuals_prop}: the proportion of individuans represented by the species at the camera.
}

If \code{by_cam} is \code{TRUE} and \code{dfcam} is provided,
the following rows are also present:
\itemize{
\item \code{sightings_RAI}: relative abundance index for species' sightings
at each camera. It is computed as the number of sightings over the sampling duration
(it represents the number of sightings per time unit).
\item \code{individuals_RAI}: the same as \code{sightings_RAI}, but computed as the
number of individuals over the sampling duration.
\item Sampling duration (named like \code{duration_col_dfcam}): sampling
duration for each camera
}

Finally, if \code{keep_all_camera_levels} is \code{TRUE}, a final column named
\code{empty} is added to indicate which cameras were empty (have no data).
}
\description{
Summarize species information from a data table
}
\examples{
df <- data.frame(species = c("zebra", "cat", "cat", "cow", NA, NA),
                 type = c("animal", "animal", "animal", "animal", "human", "blank"),
                 camera = c("C1", "C1", "C2", "C3", "C3", "C4"),
                 count = c(1, 1, 3, 50, 1, NA))
# Summarize species across all cameras
summarize_species(df, 
                  spp_col = "species", cam_col = "camera",
                  obstype_col = "type",
                  count_col = "count",
                  NA_count_placeholder = 1)
# Summarize per species and cameras
summarize_species(df, 
                  spp_col = "species", cam_col = "camera",
                  obstype_col = "type",
                  count_col = "count",
                  by_cam = TRUE,
                  NA_count_placeholder = 1)
# Add camera sampling length to get the RAI
cam_sampling <- data.frame(camera = c("C1", "C2", "C3", "C4"),
                           sampling_duration = c(100, 1, 10, 10))
summarize_species(df, 
                  spp_col = "species", cam_col = "camera",
                  obstype_col = "type",
                  count_col = "count",
                  by_cam = TRUE,
                  dfcam = cam_sampling,
                  duration_col_dfcam = "sampling_duration",
                  NA_count_placeholder = 1)
}
\seealso{
\href{https://lisanicvert.github.io/camtrapviz/articles/summarize.html}{\code{vignette("summarize", package = "camtrapviz")}},
\code{\link[=summarize_cameras]{summarize_cameras()}}
}
