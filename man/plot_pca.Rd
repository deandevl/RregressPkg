% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_pca.R
\name{plot_pca}
\alias{plot_pca}
\title{Function produces several ggplot2 based plots to review principal components analysis (PCA).}
\usage{
plot_pca(
  df = NULL,
  measures = NULL,
  center = FALSE,
  scale. = FALSE,
  tol = NULL,
  rank. = NULL,
  pca_pair = c("PC1", "PC2"),
  pca_values = "loading",
  aes_fill = NULL,
  aes_label = NULL,
  title = NULL,
  subtitle = NULL,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  pts_color = "black",
  pts_fill = "white",
  pts_alpha = 1,
  pts_size = 1,
  figure_width = 10,
  header_font_sz = 9,
  show_meas_table = TRUE,
  display_plot = TRUE
)
}
\arguments{
\item{df}{The data frame containing rows of observations across columns of numeric measurements.}

\item{measures}{A vector of column names from code{df} to used in the pca.}

\item{center}{A logical indicating whether the variables should be shifted to zero centered.}

\item{scale.}{A logical indicating whether the variables should be scaled to have unit variance before the
analysis takes place.}

\item{tol}{A value indicating the magnitude below which components should be omitted. Components
are omitted if their standard deviations are less than or equal to \code{tol} times
the standard deviation of the first component.}

\item{rank.}{A number specifying the maximal rank, i.e. maximal number of principal components to be used.
If NULL then the length of the \code{measures} argument.}

\item{pca_pair}{A string vector that names the pair of components of interest. Acceptable values
are "PC1", "PC2", "PC3", ...}

\item{pca_values}{A string that sets the type of PCA values to display. Acceptable values are "loading" or
"correlation".}

\item{aes_fill}{A string that sets the variable name from \code{df} for the aesthetic mapping for fill.}

\item{aes_label}{A string that sets the variable name from \code{df} for the aesthetic mapping for labeling
observations. If labeling then the package \code{ggrepel} is required.}

\item{title}{A string that sets the plot title.}

\item{subtitle}{A string that sets the plot subtitle.}

\item{x_limits}{Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
and maximum for the x axis. Use NA to refer to the existing minimum and maximum.}

\item{x_major_breaks}{Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
the exact major tic locations along the x axis.}

\item{y_limits}{A numeric 2 element vector that sets the minimum and  maximum for the y axis.
Use \code{NA} to refer to the existing minimum and maximum.}

\item{y_major_breaks}{A numeric vector or function that defines the exact major tic locations along the y axis.}

\item{pts_color}{A string that sets the color of the points.}

\item{pts_fill}{A string that sets the fill color of the points.}

\item{pts_alpha}{A numeric value that sets the alpha level of \code{pts_fill}.}

\item{pts_size}{A numeric value that sets the size of the points.}

\item{figure_width}{An numeric that sets the width of the overall figure in inches.}

\item{header_font_sz}{A numeric that defines the font size (in pixels) of table's headers.}

\item{show_meas_table}{A logical that if TRUE will display the table of loadings/correlations.}

\item{display_plot}{A logical that if TRUE will display the multiple-paneled plot}
}
\value{
Returning a named list with:
\enumerate{
\item "pca" -- A list object of of class \code{prcomp} containing the results of the completed PCA.
\item "percent_var" -- A numeric vector showing the percent of variance for each component.
\item "samp_plot" -- A ggplot scatter plot object of samples across an x-y pair of principal components.
\item "loadings_plot" -- A ggplot plot object of the loadings or correlations of the measurements with an x-y
pair of principal components.
\item "loadings_table_grob" -- A table showing the measurement loadings or correlations across all the principal components.
\item "figure_gtable" -- A multi-paneled TableGrob object that assembles "samp_plot", "loadings_plot", and
"loadings_table_grob" into one figure.
}
}
\description{
With the submission of a data frame with measurements and samples, the function calls \code{stats::prcomp} to
perform a pca of the data.  As an example, a regression application could consist of a set of measurements for the columns
and samples for the rows.  The function returns:
}
\details{
\enumerate{
\item A pca class object returned from \code{stats::prcomp()}
\item A ggplot2 scatter plot object of samples across an x-y pair of principal components.
\item A ggplot2 circle plot object of the loadings or correlations of the measurements with an x-y pair of principal components.
\item A TableGrob object that assembles 2, 3 above and a measurement table into one figure.
}
}
\author{
Rick Dean
}
