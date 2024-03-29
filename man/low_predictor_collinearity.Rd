% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/low_predictor_collinearity.R
\name{low_predictor_collinearity}
\alias{low_predictor_collinearity}
\title{Function provides a way to identify a set of predictors with pairwise low collinearity among themselves.}
\usage{
low_predictor_collinearity(df = NULL, cor = NULL, threshold = 0.75)
}
\arguments{
\item{df}{An optional numeric data frame of predictor variables without NA values}

\item{cor}{An optional matrix of cross correlations among the predictor variables}

\item{threshold}{A numeric that sets the minimum correlation between pairs of predictors to run through the algorithm.}
}
\value{
Returning a named list with:
\enumerate{
\item "predictors" A character vector with the names of predictors with pairwise low collinearity among themselves.
\item "correlations" The correlation matrix with just the selected predictors.
\item "max_correlation" The maximum correlation among all pairs of the selected predictors.
}
}
\description{
With the submission of a data frame of raw predictor values or correlation matrix among predictors, the function
removes the minimum number of predictors to ensure that all correlations are below a certain threshold.
}
\details{
Function was inspired by "Applied Predictive Modeling", Kuhn, Johnson, Page 47.

Note that predictors of the data frame must all be numeric without NA values.

The function's algorithm follows the following steps:
\enumerate{
\item Create a starting list of the all the candidate predictors.
\item Create a second list of pairs of predictors with correlations above a given threshold and order the correlations
from high to low.
\item For each pair of predictors (call them A and B) in the ordered list, determine the average correlation between
predictor A and the other predictors.  Do the same for predictor B.
\item If A has a larger absolute average correlation, remove it from the ordered list and from the start list created in step 1;
otherwise remove predictor B.
\item Repeat steps 3-4 through the entire ordered list of correlations defined in step 2, removing potential predictors from the
starting list created in step 1.
\item The predictors left in the starting list are identified as having a low level of collinearity.
}
}
\author{
Rick Dean
}
