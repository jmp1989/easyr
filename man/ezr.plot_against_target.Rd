% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr_plot_against_target.R
\name{ezr.plot_against_target}
\alias{ezr.plot_against_target}
\title{Plot Number Against  Binary Target}
\usage{
ezr.plot_against_target(dataset, predictor, binary_target,
  style = "equal", n_breaks = 10, fixed_breaks = NULL,
  return_as_1plot = TRUE, add_text = TRUE, default_bar_color = FALSE,
  include_response_rate = TRUE, higher_morelikely = TRUE,
  round_breaks = 0)
}
\arguments{
\item{dataset}{dataframe}

\item{predictor}{numerical value}

\item{binary_target}{a column that only has 0s and 1s.}

\item{style}{Values are 'fixed','equal','quantile','pretty','percentile'.  Default is EQUAL with n_breaks=10}

\item{n_breaks}{Default is 10.  How many bins you want.}

\item{fixed_breaks}{If style=FIXED then you want a vector of values such as seq(0,1000,100)}

\item{return_as_1plot}{Default is TRUE. If TRUE, this returns a singple plot.  Otherwise returns list of plots}

\item{add_text}{Future work...add text to bar plots.}

\item{default_bar_color}{TRUE or FALSE.  Default is FALSE.  This means plot comes out as black.  If True then color is orange.}

\item{include_response_rate.}{Default TRUE.  Add the response rate plot to the capture rate plot}

\item{higher_morelikely.}{Default is TRUE.  Higher prediction value = more likely to be a 1.  Needs to be set in order for gainslift to be sorted correctly.}
}
\value{
Returns
}
\description{
Plot a column against a binary target.
}
\details{
Generates %s by bin style plot
Counts by Bin Style plot
Cumulative plot
Density Plot

Useful for understanding how a value relates to a target.
}
