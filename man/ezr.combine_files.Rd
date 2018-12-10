% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ezr_combine_files.R
\name{ezr.combine_files}
\alias{ezr.combine_files}
\title{Title Concatenate Mutliple CSVs or Pipe txts into same dataframe}
\usage{
ezr.combine_files(directory = getwd(), pattern = "*.csv",
  keep_label_file_name_origin = FALSE, type = "csv")
}
\arguments{
\item{directory}{Default directory is working directory.   This is where all your files are.}

\item{pattern}{What is the pattern of the files that you want to load?  Ideally only these files are in the folders.  This makes it easy to match with.  Enter *.csv for all csvs or *.txt for all pipe delimited txt}

\item{keep_label_file_name_origin}{Default is FALSE.  This preserves the file name as a column in the file so you can know where it came from}

\item{type}{default is 'csv' (lowercase).  Other valid value is 'txt'.  Will assume txt is a pipe delimited file.}
}
\value{
Returns a concatenated dataframe of all the files
}
\description{
Title Concatenate Mutliple CSVs or Pipe txts into same dataframe
}