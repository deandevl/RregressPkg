% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_predictor.R
\name{plot_predictor}
\alias{plot_predictor}
\title{Function plots one predictor's effect on the response variable from an OLS model}
\usage{
plot_predictor(
  df = NULL,
  formula_obj = NULL,
  predictor_vals_df = NULL,
  interval = "confidence",
  confid_level = 0.95,
  CI_show = T,
  CI_fill = "steelblue2",
  CI_alpha = 0.4,
  title = NULL,
  subtitle = NULL,
  center_titles = F,
  x_title = "Predictor",
  y_title = "Response",
  rot_y_tic_label = F,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  axis_text_size = 11,
  show_pts = T,
  pts_color = "black",
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_size = 1,
  pts_line_alpha = 1,
  connect = T,
  line_size = 1,
  line_color = "black",
  connect_linetype = "solid",
  do_x_title = T,
  do_y_title = T
)
}
\arguments{
\item{df}{A data frame with columns for observed response and predictors}

\item{formula_obj}{A formula object following the rules of \code{stats::lm()} construction.
For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.}

\item{predictor_vals_df}{A single column data frame with values for a single predictor
where its effect is of interest.}

\item{interval}{A string that sets the type of confidence interval.  Acceptable values
are dQuote{confidence} or dQuote{prediction}.}

\item{confid_level}{A numeric that defines the confidence level for estimating confidence
intervals. The default is 0.95.}

\item{CI_show}{A logical which if TRUE will plot the confidence interval around the fitted responses.}

\item{CI_fill}{A string that sets the fill color for the confidence interval.}

\item{CI_alpha}{A numeric that set the alpha color level for the confidence interval.}

\item{title}{A string that sets the plot title.}

\item{subtitle}{A string that sets the plot subtitle.}

\item{center_titles}{A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.}

\item{x_title}{A string that sets the x axis title.}

\item{y_title}{A string that sets the y axis title.}

\item{rot_y_tic_label}{A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.}

\item{x_limits}{Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
and maximum for the x axis. Use NA to refer to the existing minimum and maximum.}

\item{x_major_breaks}{Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
the exact major tic locations along the x axis.}

\item{y_limits}{A numeric 2 element vector that sets the minimum and  maximum for the y axis.
Use \code{NA} to refer to the existing minimum and maximum.}

\item{y_major_breaks}{A numeric vector or function that defines the exact major tic locations along the y axis.}

\item{axis_text_size}{A numeric that sets the font size along the axis'. Default is 11.}

\item{show_pts}{A logical which if FALSE will plot only the lines if \code{connect} is TRUE.}

\item{pts_color}{A string that sets the color of the points.}

\item{pts_fill}{A string that sets the fill color of the points.}

\item{pts_shape}{A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.}

\item{pts_stroke}{A numeric that sets the drawing width for a point shape.}

\item{pts_size}{A numeric value that sets the size of the points.}

\item{pts_line_alpha}{A numeric value that sets the alpha level of points and connected line.}

\item{connect}{A logical which if \code{TRUE} then points will be connected with a line.}

\item{line_size}{A numeric value that sets the thickness of lines if \code{connect} is TRUE.}

\item{line_color}{A string that sets the color of the lines if \code{connect} is TRUE.}

\item{connect_linetype}{A string that sets line type \code{twodash, solid, longdash, dotted, dotdash,
dashed, blank} if \code{connect} is TRUE.}

\item{do_x_title}{A logical that controls the appearance of the x axis title.}

\item{do_y_title}{A logical that controls the appearance of the y axis title.}
}
\value{
Function returns a list with the predictor's effect response values and associated confidence intervals
along with a ggplot2 object.
}
\description{
Function holds other predictor variables at their mean value and plots
the response variable's values over selected values of a target predictor.
"Confidence" or "prediction" type intervals can also be displayed around the
response values.
}
\author{
Rick Dean
}
