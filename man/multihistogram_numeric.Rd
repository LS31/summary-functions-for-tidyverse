% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_graphs.R
\name{multihistogram_numeric}
\alias{multihistogram_numeric}
\title{Creates a ggplot histogram for multiple numeric variables.}
\usage{
multihistogram_numeric(x, ...)
}
\arguments{
\item{x}{A tibble.}

\item{...}{Comma separated list of unquoted expressions. You can treat variable names like they are positions. Use positive values to select variables; use negative values to drop variables. (This uses dplyr::select.)}
}
\value{
A ggplot.
}
\description{
By default, analyses all numeric variables. You can select
specific variables using all syntax avaiable when using dplyr::select.
}
