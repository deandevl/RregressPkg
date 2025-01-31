#' @title plot_beta_distrib
#'
#' @description
#' Function estimates the parameters of the beta distribution to replicate
#'  a set of data based on their population mean and standard deviation.
#'
#'  The function estimates the shape parameters of the beta
#'  distribution using the method of moments involving the submitted mean
#'  and standard deviation.  The returned data set reflects the 0 to 1 range
#'  of the beta distributions rescaled to the user's min/max units. A histogram
#'  ggplot2 plot of the estimated distribution is also returned.
#'
#' @param n An integer that sets the number of beta distribution points to
#'  estimate using \code{stats::rbeta()}.
#' @param mean A numeric that sets the population mean of the observed data.
#' @param sd A numeric that sets the population standard deviation of the data.
#' @param min_val A numeric that sets the population's minimal value.
#' @param max_val A numeric that sets the population's maximum value.
#' @param digits An integer that sets the number of digits to round the returned
#'  data.
#' @param seed An integer used by \code{stats::rbeta()} in setting the random seed.
#' @param title A string that sets the overall title.
#' @param subtitle A string that sets the overall subtitle.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param bins An integer that sets the number of bins for the histogram. Default is 100.
#' @param binwidth A numeric that sets the number of bins based on this value.  If the histogram x axis
#'  should depict a date variable then 'binwidth' is the number of days and if a time variable then 'binwidth' is
#'  the number of seconds.
#' @param bin_breaks A numeric vector that sets the number of bins by giving the bin boundaries explicitly.
#' @param bin_class A character string that sets the number of bins by selecting one of three types of formulas. Acceptable values are
#'  "Sturges", "Scott", or "FD".
#' @param bar_fill A string that sets the fill color attribute for the bars.
#' @param bar_color A string that sets the outline color attribute for the bars.
#' @param bar_alpha A numeric that set the alpha component attribute to 'bar_color'.
#' @param bar_lwd A numeric that sets the outline thickness attribute of the bars.
#' @param y_limits A numeric 2 element vector or function that sets the minimum and maximum for the y axis.  Use NA to
#'  refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that sets the major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that sets the minor tic locations along the y axis.
#' @param y_labels A character vector or function giving y axis tic labels.  Must be the same length as 'y_major_breaks'.
#'
#' @return A list with the shape parameters used to estimate the beta distribution
#'  along with numeric vectors of estimated data in both unscaled and scaled format. Also
#'  a ggplot2 histogram of the distribution is also provided.
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' therms_dt <- data.table::as.data.table(RregressPkg::Therms18) |>
#' _[, .(na.omit(fttrump))] |>
#'   data.table::setnames(old = "V1",new = "Rating")
#'
#' beta_est_lst <- RregressPkg::plot_beta_distrib(
#'   n = nrow(therms_dt),
#'   mean = mean(therms_dt$Rating),
#'   sd = sd(therms_dt$Rating),
#'   min_val = 0,
#'   max_val = 100,
#'   seed = 8675309,
#'   x_title = "Trump Rating",
#'   y_title = "Count",
#'   bar_fill = "blue",
#'   bar_alpha = 0.5,
#'   y_limits = c(0, 600),
#'   y_major_breaks = seq(from = 0, to = 600, by = 50)
#' )
#' a_plot <- beta_est_lst$histo_plot
#'
#' @importFrom RplotterPkg create_histogram_plot
#' @importFrom data.table data.table
#' @import ggplot2
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
  binwidth = NULL,
  bin_breaks = NULL,
  bin_class = NULL,
  bar_fill = NA,
  bar_color = "black",
  bar_alpha = 0.4,
  bar_lwd = 1.0,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver()
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
    binwidth = binwidth,
    bin_breaks = bin_breaks,
    bin_class = bin_class,
    bar_fill = bar_fill,
    bar_color = bar_color,
    bar_alpha = bar_alpha,
    bar_lwd = bar_lwd,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    y_minor_breaks = y_minor_breaks,
    y_labels = y_labels,
    rot_y_tic_label = TRUE
  )

  return(list(
    shape_1 = shape_1,
    shape_2 = shape_2,
    unscaled =  data_unscaled,
    scaled = data_scaled,
    histo_plot = histo_plot
  ))
}
