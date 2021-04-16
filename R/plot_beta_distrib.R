#' Function estimates the parameters of the beta distribution to replicate
#'  a set of data based on their population mean and standard deviation.
#'
#' @description The function estimates the shape parameters of the beta
#'  distribution using the method of moments involving the submitted mean
#'  and standard deviation.  The returned data set reflects the [0,1] range
#'  of the beta distributions rescaled to the user's min/max units. A histogram
#'  ggplot2 plot of the estimated distribution is also returned.
#'
#' @param n An integer that sets the number of beta distribution points to
#'  estimate using stats::rbeta().
#' @param mean A numeric that sets the population mean of the observed data.
#' @param sd A numeric that sets the population standard deviation of the data.
#' @param min_val A numeric that sets the population's minimal value.
#' @param max_val A numeric that sets the population's maximum value.
#' @param digits An integer that sets the number of digits to round the returned
#'  data.
#' @param seed An integer used by stats::rbeta() in setting the random seed.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param bins An integer that sets the number of bins for the histogram. Default is 100.
#' @param bar_fill A string that sets the fill color for the bars.
#' @param bar_color A string that sets the outline color for the bars.
#' @param bar_alpha A numeric that set the alpha component to \code{bar_color}.
#' @param bar_size A numeric that sets the outline thickness of the bars.
#'
#' @importFrom RplotterPkg create_histogram_plot
#' @importFrome data.table data.table
#' @import ggplot2
#'
#' @return A list with the shape parameters used to estimate the beta distribution
#'  along with numeric vectors of estimated data in both unscaled and scaled format. Also
#'  a ggplot2 histogram of the distribution is also provided.
#'
#' @author Rick Dean
#'
#' @export
plot_beta_distrib <- function(
  n = NULL,
  mean = NULL,
  sd = NULL,
  min_val = 0,
  max_val = 1,
  digits = 3,
  seed = NULL,
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_title = NULL,
  bins = 100,
  bar_fill = NA,
  bar_color = "black",
  bar_alpha = 0.4,
  bar_size = 1.0
  ) {

  range_val <- max_val - min_val
  m <- (mean - min_val) / range_val
  s <- sd / range_val

  d <- ((m * (1-m))/s^2)-1
  shape_1 <- m * d
  shape_2 <- (shape_1 * (1 - m))/m

  if(!is.null(seed)){
    set.seed(seed)
  }
  data_unscaled <- stats::rbeta(n = n, shape1 = shape_1, shape2 = shape_2)
  data_scaled <- min_val + data_unscaled * range_val

  if(!is.null(digits)) {
    data_unscaled <- round(data_unscaled, digits = digits)
    data_scaled <- round(data_scaled, digits = digits)
  }

  df_scaled = data.frame(x = data_scaled)

  histo_plot <- RplotterPkg::create_histogram_plot(
    df = df_scaled,
    aes_x = "x",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    bins = bins,
    bar_fill = bar_fill,
    bar_color = bar_color,
    bar_alpha = bar_alpha,
    bar_size = bar_size,
    rot_y_tic_label = T
  )

  return(list(
    shape_1 = shape_1,
    shape_2 = shape_2,
    unscaled =  data_unscaled,
    scaled = data_scaled,
    histo_plot = histo_plot
  ))
}
