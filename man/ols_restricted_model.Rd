% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ols_restricted_model.R
\name{ols_restricted_model}
\alias{ols_restricted_model}
\title{Function computes the linear F test and the LM test for comparing restricted
with unrestricted OLS models.}
\usage{
ols_restricted_model(
  df = NULL,
  ur_formula_obj = NULL,
  r_formula_obj = NULL,
  confid_level = 0.95
)
}
\arguments{
\item{df}{A data frame with columns for observed response and predictors}

\item{ur_formula_obj}{The unrestricted formula object following the rules of \code{stats::lm()} construction.
For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.}

\item{r_formula_obj}{The restricted formula object following the rules of \code{stats::lm()} construction.}

\item{confid_level}{A numeric decimal that sets the confidence level for computing the
critical values. The default is .95.}
}
\value{
Returns a named list with a data frame of computed F/LM statistics along
with OLS coefficient estimates for each model.
}
\description{
Given formulas for the the unrestricted and restricted OLS models,
function returns a list with a data.frame of F-value/LM-value statistics, along
with OLS coefficient estimates for both models.

Reference "Introductory Econometrics, A Modern Approach, Sixth Edition", Chapter 4,
Section 4-5 Testing Multiple Linear Restrictions: The F Test, page 127, by Jeffrey Wooldridge.
}
\author{
Rick Dean
}
