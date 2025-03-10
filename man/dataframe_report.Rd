% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataframe_report.R
\name{dataframe_report}
\alias{dataframe_report}
\title{dataframe_report}
\usage{
dataframe_report(df, miss_values = NULL)
}
\arguments{
\item{df}{The required target dataframe from which some basic information on observed data
can be gathered.}

\item{miss_values}{An optional vector with numeric and character values that define in addition to \code{NA} and \code{NaN}, other values
considered as missing. Examples might be a vector with "na", "N/A", \code{999}.}
}
\value{
Returning a named list with dataframes named:
\enumerate{
   \item "missing",
   \item "outliers"
   \item "datatypes"
}
}
\description{
Function provides a simple report on a dataframe destined to for
regression modeling, plotting, or other data investigations.

Function returns a list of dataframes showing rows of outliers, missing
data, and types of variables from the target dataframe.
}
\examples{
library(data.table)
library(RregressPkg)
set.seed(123)
dt <- data.table(
  id = 1:100,
  category = sample(c("A","B","C",NA), 100, replace = TRUE),
  value = c(rnorm(97), -10, 100, NA),
  date = c(seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 99), NaN),
  text = sample(c("Lorem","Ipsum","Dolor","Sit",NA), 100, replace = TRUE)
)
report_lst <- RregressPkg::dataframe_report(dt)

}
