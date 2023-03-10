% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server_functions.R
\name{get_named_vector}
\alias{get_named_vector}
\title{Get named vector}
\usage{
get_named_vector(df, col, widget_values)
}
\arguments{
\item{df}{The dataframe. Must have a column named widget
and a column named like col.}

\item{col}{The column of the dataframe to extract}

\item{widget_values}{The widgets to get the vector for}
}
\value{
A named vector containing the values of col,
names are the widget names
}
\description{
Get the named vector from a dataframe
}
