% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_influence.R
\name{plot_influence}
\alias{plot_influence}
\title{Function performs measures of influence to the submitted OLS model}
\usage{
plot_influence(
  df = NULL,
  formula_obj = NULL,
  id_col = NULL,
  influence_meas = "cook",
  label_threshold = 3,
  label_color = "red",
  title = NULL,
  subtitle = NULL,
  x_title = "Observation ID",
  y_title = "Influence Value",
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_alpha = 1,
  pts_size = 1,
  show_major_grids = TRUE,
  show_minor_grids = TRUE
)
}
\arguments{
\item{df}{A data frame with columns for observed response and predictors}

\item{formula_obj}{A formula object following the rules of \code{stats::lm()} construction.
For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.}

\item{id_col}{An optional argument that names the column from data frame \code{df} providing
each observation with a unique identification value.  If this argument is NULL then
data frame row numbers are used for identification. Unless you have less than 30 observations,
it is best to stay with row numbers and modify the \code{x_limits} and \code{x_major_breaks}
arguments.}

\item{influence_meas}{A string that defines the type of influence measure to apply. Acceptable
values include "internal", "external", "dffits", and "cook" for
internal/external studentized residuals, difference in fits, and Cook's distance respectively.}

\item{label_threshold}{A numeric that sets the measurement threshold beyond which observations
will be labeled with their id.}

\item{label_color}{A string that sets the label/point color for observations whose absolute
measurement is greater than the \code{label_threshold}.}

\item{title}{A string that sets the plot title.}

\item{subtitle}{A string that sets the plot subtitle.}

\item{x_title}{A string that sets the observed response x axis title. If NULL then
the x axis title does not appear. The default is "Observation ID".}

\item{y_title}{A string that sets the fitted response y axis title. If NULL then
the y axis title does not appear. The default is "Influence Value".}

\item{rot_y_tic_label}{A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.}

\item{x_limits}{A numeric 2 element vector that sets the minimum and maximum for the x axis.}

\item{x_major_breaks}{A numeric vector or function that defines the exact major tic locations along the x axis.}

\item{y_limits}{A numeric 2 element vector that sets the minimum and maximum for the y axis.
Use \code{NA} to refer to the existing minimum and maximum.}

\item{y_major_breaks}{A numeric vector or function that defines the exact major tic locations along the y axis.}

\item{y_minor_breaks}{A numeric vector or function that defines the exact minor tic locations along the y axis.}

\item{axis_text_size}{A numeric that sets the font size along the axis'. Default is 11.}

\item{pts_color}{A string that sets the color of the points.}

\item{pts_fill}{A string that sets the fill color of the points.}

\item{pts_shape}{A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.}

\item{pts_stroke}{A numeric that sets the drawing width for a point shape.}

\item{pts_alpha}{A numeric value that sets the alpha level of \code{pts_color}.}

\item{pts_size}{A numeric value that sets the size of the points.}

\item{show_major_grids}{A logical that controls the appearance of major grids.}

\item{show_minor_grids}{A logical that controls the appearance of minor grids.}
}
\value{
Function returns a named list with both a data frame of influence measures
\dQuote{influence_df} for each observation along with a scatter plot of observations versus
influence measure \dQuote{influence_plot}.
}
\description{
Function offers three different measures of influence including
internal/external studentized residuals, and Cook's distance.
Both a data frame and a plot of each observations influence on the OLS estimate are
returned to help identify data points as possible outliers and/or high leverage.
}
\author{
Rick Dean
}
