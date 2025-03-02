#' @title plot_model_check
#'
#' @description
#' Function provides multi-plots to check model assumptions of heteroscedasticity, normally distributed
#'  residuals and leverage of each datum.
#'
#' Function plots fitted values versus residuals, fitted values versus response
#'  values, a QQ plot of the standardized residuals, and histogram of residual values.
#'
#' For the QQ plot only standardized residuals are computed without taking the square root of the absolute value.
#'
#' @param fitted_v A required numeric vector of fitted values.
#' @param response_v A required numeric vector of observed response/dependent values.
#' @param residual_v A required numeric vector of corresponding residual values.
#' @param id_v An optional numeric/string vector that labels the fit/residual pairs. If this argument is \code{NULL} then
#'  the fit/residual pairs are numbered for identification.
#' @param residual_label_threshold A numeric that sets the residual threshold beyond which observations
#'  will be labeled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  residual is greater than the 'label_threshold'.
#' @param label_sd A numeric that sets the number times +/- residual standard deviations to plot as a
#'  pair of horizontal dotted lines. Typical values could be 1 or 2 standard deviations.
#' @param title A string that sets the plot title.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_alpha A numeric value that sets the alpha level of 'pts_color'.
#' @param pts_size A numeric value that sets the size of the points.
#' @param histo_fill A string that sets the histogram fill color.
#' @param histo_alpha A numeric that sets the histogram fill alpha.
#' @param zero_line A logical which if \code{TRuE} plots the zero horizontal reference line..
#' @param zero_line_color A string that sets the zero line color.
#' @param trend_line A logical which if \code{TRUE} plots a polynomial based trend line across the residuals.
#' @param trend_line_color A string that sets the color of the trend line.
#'
#' @return A multi-paneled ggplot2 object with various plots for checking
#'   linear model assumptions.
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' library(gtable)
#' library(ggplotify)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' sample_size = 500
#' x <- runif(n = sample_size) * 5
#' y <- 3 + 5 * x^2 + rnorm(n = sample_size, mean = 0, sd = 5)
#' sim_df <- data.frame(x,y)
#'
#' fit_lm <- lm(y ~ x, data = sim_df)
#'
#' sim_ols <- RregressPkg::ols_calc(
#'   df = sim_df,
#'   formula_obj = fit_lm
#' )
#'
#' a_plot <- RregressPkg::plot_model_check(
#'   fitted_v = sim_ols$fitted_vals,
#'   response_v = sim_df$y,
#'   residual_v = sim_ols$residual_vals,
#'   histo_fill = "blue",
#'   histo_alpha = 0.5,
#'   zero_line_color = "darkorange"
#' )
#'
#' @importFrom data.table data.table
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom RplotterPkg multi_panel_grid
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#'
#' @export
plot_model_check <- function(
  fitted_v,
  response_v,
  residual_v,
  id_v = NULL,
  residual_label_threshold = NULL,
  label_color = "red",
  label_sd =NULL,
  title = NULL,
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_alpha = 1.0,
  pts_size = 1,
  histo_fill = "white",
  histo_alpha = 1.0,
  zero_line = TRUE,
  zero_line_color = "red",
  trend_line = TRUE,
  trend_line_color = "blue"
){
  plot_fit_resid <- RregressPkg::plot_fit_residuals(
    fitted_v = fitted_v,
    residual_v = residual_v,
    id_v = id_v,
    label_threshold = residual_label_threshold,
    label_color = label_color,
    label_sd = label_sd,
    x_title = "Fitted",
    y_title = "Residuals",
    axis_text_size = axis_text_size,
    rot_y_tic_label = TRUE,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_alpha = pts_alpha,
    pts_size = pts_size,
    trend_line = trend_line,
    trend_line_color = trend_line_color,
    zero_line = zero_line,
    zero_line_color = zero_line_color,
    zero_line_width = 1.0
  )

  fit_response_df <- data.frame(
    fit = fitted_v,
    response = response_v
  )

  plot_fit_response <- RplotterPkg::create_scatter_plot(
    df = fit_response_df,
    aes_x = "fit",
    aes_y = "response",
    x_title = "Fitted",
    y_title = "Response",
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_line_alpha = pts_alpha,
    pts_size = pts_size,
    axis_text_size = axis_text_size,
    rot_y_tic_label = TRUE
  ) +
    ggplot2::geom_abline(color = zero_line_color)


  if(is.null(id_v)){
    id_v <- 1: length(fitted_v)
  }

  residuals_sd <- stats::sd(residual_v)
  stand_resid_v <- residual_v / residuals_sd

  df_qq <- data.frame(
    id = id_v,
    stand_resid = stand_resid_v
  )

  plot_qq <- RregressPkg::plot_qq(
    df = df_qq,
    numeric_col = "stand_resid",
    id_col = "id",
    x_title = "Theoretical Quantiles",
    y_title = "Standardized Residuals",
    axis_text_size = axis_text_size,
    rot_y_tic_label = TRUE,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_alpha = pts_alpha,
    pts_size = pts_size,
    ref_line_color = zero_line_color,
    ref_line_width = 0.5,
    ci_line_type = "dashed",
    labels_n = 3
  )

  residuals_df <- data.frame(residual = residual_v)
  plot_residual_histogram <- RplotterPkg::create_histogram_plot(
    df = residuals_df,
    aes_x = "residual",
    x_title = "Residuals",
    y_title = "Count",
    bar_fill = histo_fill,
    bar_alpha = histo_alpha
  )

  layout <- list(
    plots = list(plot_fit_resid, plot_fit_response, plot_qq, plot_residual_histogram),
    rows = c(1, 2, 1, 2),
    cols = c(1, 1, 2, 2)
  )

  multi_plot_check <- RplotterPkg::multi_panel_grid(
    layout = layout,
    title = title,
    do_grid = TRUE
  )

  return(multi_plot_check)
}




