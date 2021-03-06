% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr_null_countby_row.R
\name{ezr.null_countby_row}
\alias{ezr.null_countby_row}
\title{Counts of NULLs or NULL+Alt.Value by row}
\usage{
ezr.null_countby_row(dataset, alternative_value = NULL,
  add_column_to_df = TRUE)
}
\arguments{
\item{alternative_value}{If you want to additionally count for a value that is valid as null, such as -1.  This value is counted across ALL columns...Future update will check for specific columns only.  Default is NULL}

\item{add_column_to_df}{Default is TRUE.  Adds back a 'nulls_in_row' column.}
}
\value{
Either a dataframe of original + new column or just the results
}
\description{
Counts of null per row or nulls per row + alt value.
}
