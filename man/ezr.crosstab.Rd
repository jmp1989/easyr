% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr.crosstab.R
\name{ezr.crosstab}
\alias{ezr.crosstab}
\title{Title  Cross Tab (Table)}
\usage{
ezr.crosstab(dataset, row_field, column_field,
  percent_denominator = "row", rounding_digits = 0,
  position = "front", add_totals = TRUE, numerical_breaks = 5,
  round_breaks = 0, style = "equal")
}
\arguments{
\item{dataset}{Dataframe}

\item{row_field}{Ideally place the grouping variable here.}

\item{column_field}{Ideally place the target or outcome interested in comparinghere.}

\item{percent_denominator}{Valid values are 'row','all','col'.  What is the demoninator for percents?  Row is default so place target in column.}

\item{rounding_digits}{Default is 0 so whole percents are shown}

\item{position}{Default is front.  Determines what is placed in the ()s}

\item{add_totals}{Add the totals?  Default is true.}

\item{numerical_breaks}{N breaks for numerical values}

\item{round_breaks}{Digits to round numerical values to.}

\item{style}{Valid values are 'equal','pretty','quantile','percentile'.  Default is 'equal'}
}
\value{
Returns the cross tab results...
}
\description{
Generate a cross table of the fields you are interested in.  Input of fields should be with names in quotes or as a result of a dplyr pipe similar to mtcars %>% select(vs).  dataset$field is not supported
}
\examples{
 ezr.crosstab(mtcars, mtcars \%>\% select(cyl), mtcars \%>\% select(vs))
ezr.crosstab(mtcars, 'cyl', 'vs')
}
