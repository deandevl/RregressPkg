#' @title plot_fit_residuals
#'
#' @description
#' Function creates a scatter plot of the fitted versus residual values.
#'
#' Function returns a ggplot2 scatter plot object which can be further modified.
#'
#' For the standardized residuals, the raw residuals are divided by their standard deviation.
#'   The square root of the absolute value of each standardized residual is then plotted against their
#'   respective fitted value.
#'
#' @param fitted_v A required numeric vector of fitted values.
#' @param residual_v A required numeric vector of corresponding residual values.
#' @param id_v An optional numeric/string vector that labels the fit/residual pairs.
#'  If this argument is \code{NULL} then the fit/residual pairs are numbered for identification.
#' @param residual_standardized A logical which if \code{TRUE} will divide the raw residuals by their
#'  estimated standard deviation. The square root of the absolute value of each standardized
#'  residual is then plotted against their respective fitted value.
#' @param label_threshold A numeric that sets the residual threshold beyond which observations
#'  will be labeled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  residual is greater than the 'label_threshold'.
#' @param label_sd A numeric that sets the number times +/- residual standard deviations to plot as a
#'  pair of horizontal dotted lines. Typical values could be 1 or 2 standard deviations.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the x axis title. If \code{NULL} (the default) then the
#'   x axis title does not appear.
#' @param y_title A string that sets the y axis title. If \code{NULL} then the y axis title does not appear.
#' @param rot_y_tic_label A logical which if \code{TRUE} rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of 'fitted_v', a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of 'fitted_v', a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of 'fitted_v', a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as 'x_major_breaks', that labels the major tics.
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as 'y_major_breaks', that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_alpha A numeric value that sets the alpha level of 'pts_color'.
#' @param pts_size A numeric value that sets the size of the points.
#' @param trend_line A logical which if \code{TRUE} plots a polynomial based trend line across the residuals.
#' @param trend_line_color A string that sets the color of the trend line.
#' @param trend_line_width A numeric that sets the width of the trend line.
#' @param zero_line A logical which if \code{TRUE} plots the zero horizontal reference line.
#' @param zero_line_color A string that sets the color of the zero horizontal reference line.
#' @param zero_line_width A numeric that sets the width of the zero horizontal reference line.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#'
#' @return Function returns a ggplot2 object of fitted vs residual values.
#'
#' @examples
#' library(wooldridge)
#' library(ggplot2)
#' library(data.table)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' hprice1_dt <- data.table::as.data.table(wooldridge::hprice1) |>
#' _[,.(price, lotsize, sqrft, bdrms)]
#'
#' housing_price_lm <- price ~ lotsize + sqrft + bdrms
#' housing_price_ols <- RregressPkg::ols_calc(
#'   df = hprice1_dt,
#'   formula_obj = housing_price_lm
#' )
#' a_plot <- RregressPkg::plot_fit_residuals(
#'   fitted_v = housing_price_ols$fitted_vals,
#'   residual_v = housing_price_ols$residual_vals,
#'   subtitle = "Data from housing prices",
#'   x_title = "Fitted",
#'   y_title = "Residuals",
#'   trend_line = FALSE,
#'   zero_line_color = "darkorange",
#'   zero_line_width = 0.8,
#'   label_threshold = 100,
#'   label_sd = 1.0
#' )
#'
#' @importFrom data.table data.table
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#'
#' @export
plot_fit_residuals <- function(
  fitted_v,
  residual_v,
  id_v = NULL,
  residual_standardized = FALSE,
  label_threshold = NULL,
  label_color = "red",
  label_sd = NULL,
  title = NULL,
  subtitle = NULL,
  x_title = NULL,
  y_title = NULL,
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
  x_minor_breaks = waiver(),
  x_labels = waiver(),
  x_log10 = FALSE,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  y_labels = waiver(),
  y_log10 = FALSE,
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_alpha = 1.0,
  pts_size = 1,
  trend_line = TRUE,
  trend_line_color = "blue",
  trend_line_width = 1.0,
  zero_line = TRUE,
  zero_line_color = "red",
  zero_line_width = 1.4,
  show_major_grids = TRUE,
  show_minor_grids = TRUE
){
  residual <- id <- NULL

  if(is.null(id_v)){
    id_v <- 1: length(fitted_v)
  }

  plot_dt <- data.table (
    id = id_v,
    fit = fitted_v,
    residual = residual_v
  )
  residuals_sd <- stats::sd(residual_v)
  if(residual_standardized){
    stand_resid <- residual_v / residuals_sd
    plot_dt[, residual := sqrt(abs(stand_resid))]
    if(!is.null(label_threshold)){
      stand_resid <- label_threshold / residuals_sd
      label_threshold <- sqrt(abs(stand_resid))
    }
  }

  fit_residual_plot <- RplotterPkg::create_scatter_plot(
    df = plot_dt,
    aes_x = "fit",
    aes_y = "residual",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    rot_y_tic_label = rot_y_tic_label,
    x_limits = y_limits,
    x_major_breaks = y_major_breaks,
    x_minor_breaks = y_minor_breaks,
    x_labels = y_labels,
    x_log10 = y_log10,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    y_minor_breaks = y_minor_breaks,
    y_labels = y_labels,
    y_log10 = y_log10,
    axis_text_size = axis_text_size,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_shape = pts_shape,
    pts_stroke = pts_stroke,
    pts_line_alpha = pts_alpha,
    pts_size = pts_size,
    show_major_grids = show_major_grids,
    show_minor_grids = show_minor_grids
  )

  if(!residual_standardized){
    if(zero_line){
      fit_residual_plot <- fit_residual_plot +
        ggplot2::geom_hline(yintercept = 0, color = zero_line_color, linewidth = zero_line_width)
    }
    if(!is.null(label_sd)){
      fit_residual_plot <- fit_residual_plot +
        ggplot2::geom_hline(yintercept = label_sd * residuals_sd, color = zero_line_color, linewidth = zero_line_width, linetype = "longdash")
      fit_residual_plot <- fit_residual_plot +
        ggplot2::geom_hline(yintercept = -label_sd * residuals_sd, color = zero_line_color, linewidth = zero_line_width, linetype = "longdash")
    }
  }

  if(!is.null(label_threshold)){
    label_data <- plot_dt[abs(residual) >= label_threshold]
    fit_residual_plot <- fit_residual_plot +
      ggplot2::geom_point(data = label_data, color = label_color, size = 2.5) +
      ggplot2::geom_text(
        data = label_data,
        aes(
          label = id,
          hjust = -0.2,
          vjust = -0.4
        ),
        color = label_color
      )
  }

  if(trend_line){
    trend_model <- stats::loess(residual ~ fit, data = plot_dt)
    fit_residual_plot <- fit_residual_plot +
      ggplot2::geom_line(aes(x = trend_model$x, y = trend_model$fitted), color = trend_line_color, linewidth = trend_line_width)
  }

  return(fit_residual_plot)
}
