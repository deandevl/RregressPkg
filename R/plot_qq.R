#' @title plot_qq
#'
#' @description
#' Function plots a quantile-quantile plot (QQ-plot) for comparing an
#' observed distribution with a normal distribution.
#'
#' @param df A data frame with a column of numeric observations and optionally a column for
#'  observation identifications.
#' @param numeric_col A string that names the numeric column from 'df' for QQ plotting.
#' @param id_col An optional argument that names the column from 'df' providing
#'  each observation with a unique identification value.  If this argument is NULL then
#'  row numbers of 'df' are used for identification.
#' @param standardize A logical which if \code{TRUE} will standardize the observations under \code{df$numeric_col}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the observed response x axis title.
#' @param y_title A string that sets the fitted response y axis title.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of \code{df$numeric_col}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use \code{NA} to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of \code{df$numeric_col}, a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of \code{df$numeric_col}, a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as 'x_major_breaks', that labels the major tics.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as 'y_major_breaks', that labels the major tics.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_alpha A numeric value that sets the alpha level of \code{pts_color}.
#' @param pts_size A numeric value that sets the size of the points.
#' @param plot_ref_line A logical which if \code{TRUE} will plot a reference line.
#' @param ref_line_color A string that sets the color of the reference line.
#' @param ref_line_width A numeric that sets the reference line width.
#' @param plot_ci_lines A logical which if \code{TRUE} will plot upper/lower confidence lines.
#' @param ci_lines_color A string that sets the color of the confidence lines.
#' @param ci_lines_width A numeric that sets the confidence lines width.
#' @param ci_line_type A string that sets confidence lines line type. Acceptable values are \code{twodash, solid,
#'  longdash, dotted, dotdash, dashed}
#' @param labels_n A numeric that sets the number of extreme values to label. If
#'  \code{NULL} then no points are labeled.
#' @param label_color A string that sets the label color.
#'
#' @return Returning a ggplot2 object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' set.seed(20200825)
#' sample_obs_df <- data.frame(
#'   vals = stats::rnorm(20, 10, 3)
#' )
#'
#' a_plot <- RregressPkg::plot_qq(
#'   df = sample_obs_df,
#'   numeric_col = "vals",
#'   title = "QQ plot of Random Observations",
#'   subtitle = "Sample is from a normal distribution",
#'   x_title = "Theoretical Normal Quantiles",
#'   y_title = "Sample Quantiles",
#'   rot_y_tic_label = TRUE,
#'   ci_lines_color = "blue",
#'   ci_line_type = "dashed",
#'   pts_size = 4,
#'   labels_n = 3
#' )
#'
#' @importFrom data.table data.table
#' @importFrom data.table setorder
#' @importFrom RplotterPkg create_scatter_plot
#' @import ggplot2
#'
#' @export
plot_qq <- function(
  df = NULL,
  numeric_col = NULL,
  id_col = NULL,
  standardize = FALSE,
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_title = NULL,
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_alpha = 1.0,
  pts_size = 1,
  plot_ref_line = TRUE,
  ref_line_color = "red",
  ref_line_width = 1,
  plot_ci_lines = TRUE,
  ci_lines_color = "black",
  ci_lines_width = 1,
  ci_line_type = "solid",
  labels_n = NULL,
  label_color = "red"
){
  id <- x <- y <- obs_median <- obs_sd <- upper <- lower <- abs_y <- NULL

  dt <- data.table(
    y = df[[numeric_col]]
  )
  if(!is.null(id_col)){
    dt[, id := df[[id_col]]]
  }else {
    dt[, id := 1:nrow(dt)]
  }

  if(standardize){
    dt[, y := scale(y)]
  }

  data.table::setorder(dt, y)
  obs_quantiles <- (rank(dt$y) - 0.5)/nrow(dt)

  normal_quantiles <- stats::qnorm(obs_quantiles, 0, 1)
  dt[, x := normal_quantiles]

  a_plot <- RplotterPkg::create_scatter_plot(
    df = dt,
    aes_x = "x",
    aes_y = "y",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    rot_y_tic_label = rot_y_tic_label,
    x_limits = x_limits,
    x_major_breaks = x_major_breaks,
    x_minor_breaks = x_minor_breaks,
    x_labels = x_labels,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    y_minor_breaks = y_minor_breaks,
    y_labels = y_labels,
    axis_text_size = axis_text_size,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_shape = pts_shape,
    pts_stroke = pts_stroke,
    pts_line_alpha = pts_alpha,
    pts_size = pts_size
  )

  robust_line_df <- data.frame(
    obs_sd = stats::IQR(dt$y)/1.349,
    obs_median = stats::median(dt$y)
  )
  if(plot_ref_line){
    a_plot <- a_plot +
      geom_abline(
        data = robust_line_df,
        aes(
          intercept = obs_median,
          slope = obs_sd,
        ),
        color = ref_line_color,
        linewidth = ref_line_width
      )
  }

  if(plot_ci_lines){
    se_z <- function(z,n){
      return(sqrt(stats::pnorm(z) * (1 - stats::pnorm(z))/n)/stats::dnorm(z))
    }
    ci_x <- seq(-2.2, 2.2, length.out = 300)
    ci_y <- robust_line_df$obs_median + ci_x * robust_line_df$obs_sd
    robust_se <- robust_line_df$obs_sd * se_z(z = ci_x, n = nrow(dt))
    robust_upper <- ci_y + 2 * robust_se
    robust_lower <- ci_y - 2 * robust_se

    ci_df <- data.frame(
      x = ci_x,
      upper = robust_upper,
      lower = robust_lower
    )
    a_plot <- a_plot +
      geom_line(data = ci_df, aes(x = ci_x, y = upper), color = ci_lines_color, linewidth = ci_lines_width, linetype = ci_line_type) +
      geom_line(data = ci_df, aes(x = ci_x, y = lower), color = ci_lines_color, linewidth = ci_lines_width, linetype = ci_line_type)
  }

  if(!is.null(labels_n)){
    extreme_dt <- data.table::copy(dt)

    extreme_dt[,abs_y := abs(y - (robust_line_df$obs_median + x * robust_line_df$obs_sd))]

    data.table::setorder(extreme_dt, -abs_y)

    labels_dt <- extreme_dt[1:labels_n]
    a_plot <- a_plot +
      ggplot2::geom_text(
        data = labels_dt,
        aes(
          label = id,
          hjust = -0.2,
          vjust = -0.4
        ),
        color = label_color
      )
  }

  return(a_plot)
}
