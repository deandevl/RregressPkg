% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ols_predict_calc.R
\name{ols_predict_calc}
\alias{ols_predict_calc}
\title{Function computes response values for a certain set of constant predictor values}
\usage{
ols_predict_calc(
  df = NULL,
  formula_obj = NULL,
  predictor_vals_df = NULL,
  interval = "confidence",
  confid_level = 0.95
)
}
\arguments{
\item{df}{A data frame with columns for observed response and predictors}

\item{formula_obj}{A formula object following the rules of \code{stats::lm()} construction.
For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula}

\item{predictor_vals_df}{A data frame of target predictor values to use for estimating the responses
and associated confidence intervals.}

\item{interval}{A string that sets the type of confidence interval.  Acceptable values
are "confidence" or "prediction".}

\item{confid_level}{A numeric that defines the confidence level for estimating confidence
intervals. The default is 0.95.}
}
\value{
Returning a data frame with the OLS response values and their corresponding
upper and lower confidence intervals for both "fitted value" and "predicted value". Also
the standard errors used in the CI estimation.
}
\description{
The function requires a data frame with columns for observed response and predictors,
a formula object (class = "formula"), and a data frame of target predictor values. From the
predictor values estimates of the response are calculated along with their corresponding
"confidence" or "prediction" intervals.
}
\author{
Rick Dean
}
