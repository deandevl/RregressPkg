% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ols_calc.R
\name{ols_calc}
\alias{ols_calc}
\title{Function computes linear OLS regression parameter estimates}
\usage{
ols_calc(
  df = NULL,
  X_mt = NULL,
  Y_mt = NULL,
  formula_obj = NULL,
  confid_level = 0.95,
  na_omit = FALSE
)
}
\arguments{
\item{df}{A data frame with columns for observed response and predictors.}

\item{X_mt}{In lieu of both \code{df} and \code{formula_obj}, a matrix of predictor values can be
submitted along with \code{Y_mt}.}

\item{Y_mt}{In lieu of both \code{df} and \code{formula_obj}, a single column matrix of response
values can be submitted along with \code{X_mt}.}

\item{formula_obj}{A formula object following the rules of \code{stats::lm()} construction.
For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.}

\item{confid_level}{A numeric that defines the confidence level for estimating confidence
intervals. The default is 0.95.}

\item{na_omit}{A logical which if TRUE will omit rows that have NA values.}
}
\value{
Returning a named list of data frames with OLS coefficient estimates, residual
statistics, ANOVA of the regression along with residuals, fitted values, and R^2.
}
\description{
The function requires a data frame with columns for observed response and predictors,
and a formula object (class = dQuote{formula}) that describes the OLS model of interest.
Alternatively, the function will accept a matrix \code{X_mt} and one column matrix \code{Y_mt} for the
predictor and response values respectively.
}
\author{
Rick Dean
}
