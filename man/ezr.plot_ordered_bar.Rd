% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr_plot_ordered_bar.R
\name{ezr.plot_ordered_bar}
\alias{ezr.plot_ordered_bar}
\title{BarChart}
\usage{
ezr.plot_ordered_bar(dataset, column, vertical = TRUE, max_number = 10,
  label = "percent", axis = "count", title = NULL)
}
\arguments{
\item{dataset}{dataset}

\item{column}{field plotting}

\item{vertical}{TRUE or FALSE.}

\item{max_number}{The maximum number that you want to plot.  Default is 10}

\item{label}{Default is 'percent'.  Other valid value is 'count'}

\item{axis}{Default is 'count'.  Other valid value is 'percent'.}
}
\description{
Plots Counts and %s of a categorical value
}
\examples{
ezr.plot_ordered_bar(diamonds, 'color', vertical = FALSE)
}
