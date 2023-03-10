% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server_functions.R
\name{get_separator}
\alias{get_separator}
\title{Get separator for a file}
\usage{
get_separator(line, default = ",")
}
\arguments{
\item{line}{A line from a file}

\item{default}{The default separator to use in case none work}
}
\value{
The detected separator (looks for comma, semicolon and tab)
}
\description{
Detects the separator from one line of a file.
}
