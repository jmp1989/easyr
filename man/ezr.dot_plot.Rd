% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr_dot_plot.R
\name{ezr.dot_plot}
\alias{ezr.dot_plot}
\title{Dot Plot for Ranking}
\usage{
ezr.dot_plot(dataset, x_axis, y_axis, title = NULL)
}
\arguments{
\item{dataset}{Dataframe}

\item{x_axis}{This is the value that will be on the x axis.   It should be continous}

\item{y_axis}{This is the value that will be on the y axis.  Ideally it is categorical.  There should be a limited number of values}

\item{title}{Default is NULL}
}
\value{
returns a nice dot plot which is useful for visualizing rankings
}
\description{
Creates a dot plot.   Useful for comparing a numerical value where ranking matters.
}
