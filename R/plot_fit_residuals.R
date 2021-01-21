#' Function creates a scatter plot of the OLS fitted versus residual values.
#'
#' @description The fitted values are along the x axis and the residuals are along the y axis.
#'  Function returns a ggplot2 object which can be further modified. The input consist of
#'  a data frame containing columns for the dependent and independent variables.
#'
#' @param data_df A data frame containing values for both the observed independent
#'  and dependent variables.
#' @param dep_str A string that names the column in \code{data_df} containing the dependent
#'  variable.
#' @param standardized_resid A logical which if TRUE plots the standardized residuals.
#' @param standardized_threshold A numeric that sets the threshold for labeling \code{standardized_resid}
#'  with a row number. The value is usually between 2 to 3.
#' @param standardized_color A string that sets the text color for standardized residual label.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the observed response x axis title.
#' @param y_title A string that sets the fitted response y axis title.
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
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#'
#' @importFrom data.table data.table
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#'
#' @return Function returns a named list with ols estimates and
#'  a ggplot2 object of fitted vs residual values.
#'
#' @author Rick Dean
#'
#' @export
plot_fit_residuals <- function(
  data_df = NULL,
  dep_str = NULL,
  standardized_resid = FALSE,
  standardized_threshold = NULL,
  standardized_color = "red",
  title = NULL,
  subtitle = NULL,
  x_title = "Fitted Values",
  y_title = "Residual Values",
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
  show_minor_grids = TRUE,
  do_x_title = TRUE,
  do_y_title = TRUE
){
  ols_regress_lst <- RregressPkg::ols_regress_calc(
    data_df = data_df,
    dep_str = dep_str
  )

  fit_residuals <- ols_regress_lst$resid
  if(standardized_resid){
    fit_residuals <- ols_regress_lst$stand_residuals
  }

  dt <- data.table::data.table(
    row_number = rownames(data_df),
    fit_values = ols_regress_lst$fitted_val,
    fit_residuals = fit_residuals
  )

  fit_residual_plot <- RplotterPkg::create_scatter_plot(
    df = dt,
    aes_x = "fit_values",
    aes_y = "fit_residuals",
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
    pts_shape = pts_shape,
    pts_stroke = pts_stroke,
    pts_line_alpha = pts_alpha,
    pts_size = pts_size,
    do_x_title = do_x_title,
    do_y_title = do_y_title,
    show_major_grids = show_major_grids,
    show_minor_grids = show_minor_grids
  )

  fit_residual_plot <- fit_residual_plot +
    ggplot2::geom_hline(yintercept = 0, color = zero_line_color, size = zero_line_size)

  if(trend_line){
    trend_model <- stats::loess(fit_residuals ~ fit_values, data = dt)
    fit_residual_plot <- fit_residual_plot +
      ggplot2::geom_line(aes(x = trend_model$x, y = trend_model$fitted), color = trend_line_color, size = trend_line_size)
  }

  if(standardized_resid & !is.null(standardized_threshold)){
    fit_residual_plot <- fit_residual_plot +
      ggrepel::geom_text_repel(data = dt[abs(fit_residuals) >= standardized_threshold], aes(label = row_number), color = standardized_color, size = 3.5)
  }
  return(
    list(
      ols = ols_regress_lst,
      plot = fit_residual_plot
    )
  )
}
