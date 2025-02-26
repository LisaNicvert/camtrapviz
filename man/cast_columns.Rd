% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_format_data.R
\name{cast_columns}
\alias{cast_columns}
\title{Cast columns types}
\usage{
cast_columns(
  df,
  cast_type,
  dfname = substitute(df),
  vec_name = substitute(cast_type)
)
}
\arguments{
\item{df}{A dataframe containing the columns specified in
the names of \code{cast_type}.}

\item{cast_type}{A named list containing the name of the
function to cast between types.
The list's names are the names of the columns to cast in \code{df}.
Elements of this list can be:
\itemize{
\item a character giving a valid function name to call
\item a list with the first element being the function to call (character)
and additional arguments to the function call (that can be named as
the names of the functions' arguments).
}}

\item{dfname}{Name of the dataframe to display in the error message.
It is useful when this function is used inside other functions
for a better error message.}

\item{vec_name}{Name of the vector of columns to display in the error message.
It is useful when this function is used inside other functions
for a better error message.}
}
\value{
the original dataframe with the specified columns
casted with the type indicated in \code{cast_type}.
}
\description{
Converts columns to a given format.
}
\examples{
df <- data.frame(num = 1:10,
                 char = letters[1:10],
                 date = rep("12/24/2020", 10))
cast <- list(num = "as.character",
             char = "as.factor",
             date = list("as.Date", 
                         format = "\%m/\%d/\%Y"))
dfcast <- cast_columns(df, cast)
}
