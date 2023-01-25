#' Function creates a scatter plot of the OLS fitted versus residual values.
#'
#' Function returns a ggplot2 scatter plot object which can be further modified.
#'
#' For the standardized residuals, the raw residuals are divided by their standard deviation.
#'   The square root of the absolute value of each standardized residual is then plotted against their
#'   respective fitted value.
#'
#' @param df A data frame containing values for both the predictor values and the associated
#'  response variable.
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param residual_standardized A logical which if TRUE will divide the raw residuals by their
#'  estimated standard deviation. The square root of the absolute value of each standardized
#'  residual is then plotted against their respective fitted value.
#' @param id_col An optional argument that names the column from data frame \code{df} providing
#'  each observation with a unique identification value.  If this argument is NULL then
#'  data frame row numbers are used for identification.
#' @param label_threshold A numeric that sets the residual threshold beyond which observations
#'  will be labeled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  residual is greater than the \code{label_threshold}.
#' @param label_sd A numeric that sets the number times +/- residual standard deviations to plot as a
#'  pair of horizontal dotted lines. Typical values could be 1 or 2 standard deviations.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the x axis title. If NULL (the default)  then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. If NULL then the y axis title does not appear.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of \code{x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of \code{x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param x_minor_breaks Depending on the class of \code{x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact minor tic locations along the x axis.
#' @param x_labels A character vector with the same length as \code{x_major_breaks}, that labels the major tics.
#' @param x_log10 A logical which if \code{TRUE} will use a log10 scale for the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param y_labels A character vector with the same length as \code{y_major_breaks}, that labels the major tics.
#' @param y_log10 A logical which if \code{TRUE} will use a log10 scale for the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_alpha A numeric value that sets the alpha level of \code{pts_color}.
#' @param pts_size A numeric value that sets the size of the points.
#' @param trend_line A logical which if TRUE plots a polynomial based trend line across the residuals.
#' @param trend_line_color A string that sets the color of the trend line.
#' @param trend_line_size A numeric that sets the width of the trend line.
#' @param zero_line_color A string that sets the color of the zero horizontal reference line.
#' @param zero_line_size A numeric that sets the width of the zero horizontal reference line.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#'
#' @importFrom data.table data.table
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggrepel geom_text_repel
#'
#' @return Function returns a ggplot2 object of fitted vs residual values.
#'
#' @author Rick Dean
#'
#' @export
plot_fit_residuals <- function(
  df = NULL,
  formula_obj = NULL,
  residual_standardized = FALSE,
  id_col = NULL,
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
  trend_line_color = "red",
  trend_line_size = 1.0,
  zero_line_color = "blue",
  zero_line_size = 1.4,
  show_major_grids = TRUE,
  show_minor_grids = TRUE
){

  ols_lst <- RregressPkg::ols_calc(
    df = df,
    formula_obj = formula_obj
  )
  
  if(!is.null(id_col)){
    id_v <- df[[id_col]]
  }else {
    id_v <- 1: nrow(df)
  }

  plot_dt <- data.table (
    id = id_v,
    fit = ols_lst$fitted_vals
  )
  
  if(residual_standardized){
    rse <- ols_lst$rse * sqrt(1 - diag(ols_lst$hat_mt))
    stand_resid <- ols_lst$residual_vals / rse
    plot_dt[, residuals := sqrt(abs(stand_resid))]
    if(!is.null(label_threshold)){
      stand_resid <- label_threshold / rse
      label_threshold <- sqrt(abs(stand_resid))
    }
  }else {
    plot_dt[, residuals := ols_lst$residual_vals]
  }
  
  fit_residual_plot <- RplotterPkg::create_scatter_plot(
    df = plot_dt,
    aes_x = "fit",
    aes_y = "residuals",
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
    fit_residual_plot <- fit_residual_plot +
      ggplot2::geom_hline(yintercept = 0, color = zero_line_color, size = zero_line_size)

    if(!is.null(label_sd)){
      residuals_sd <- sd(ols_lst$residual_vals)
      fit_residual_plot <- fit_residual_plot +
        ggplot2::geom_hline(yintercept = residuals_sd, color = zero_line_color, size = zero_line_size, linetype = "longdash")
      fit_residual_plot <- fit_residual_plot +
        ggplot2::geom_hline(yintercept = -residuals_sd, color = zero_line_color, size = zero_line_size, linetype = "longdash")
    }
  }
  
  if(!is.null(label_threshold)){
    label_data <- plot_dt[abs(residuals) >= label_threshold]
    fit_residual_plot <- fit_residual_plot +
      ggplot2::geom_point(data = label_data, color = label_color, size = 2.5) +
      ggrepel::geom_text_repel(data = label_data, aes(label = id), color = label_color)
  }

  if(trend_line){
    trend_model <- stats::loess(residuals ~ fit, data = plot_dt)
    fit_residual_plot <- fit_residual_plot +
      ggplot2::geom_line(aes(x = trend_model$x, y = trend_model$fitted), color = trend_line_color, size = trend_line_size)
  }
  
  return(fit_residual_plot)
}
