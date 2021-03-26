#' Function provides multi-plots to check model assumptions of heteroscedasticity, normally distributed
#'  residuals and leverage of each datum.
#'
#' @description Function plots fitted values versus residuals, fitted values versus standardized
#'  residuals, a QQ plot of the standardized residuals, and Cook's distance for a check on datum
#'  leverage.
#'
#' @details For the standardized residuals, the raw residuals are divided by their standard deviation.
#'   The square root of the absolute value of each standardized residual is then plotted against their
#'   respective fitted value.
#'
#'   For the QQ plot only standardized residuals are computed without taking the square root of the absolute value.
#'
#' @param df A data frame containing values for both the predictor values and the associated
#'  response variable.
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param id_col An optional argument that names the column from data frame \code{df} providing
#'  each observation with a unique identification value.  If this argument is NULL then
#'  data frame row numbers are used for identification.
#' @param residual_label_threshold A numeric that sets the residual threshold beyond which observations
#'  will be labeled with their id.
#' @param leverage_label_threshold A numeric that sets the leverage threshold beyond which observations
#'  will be labeled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  residual is greater than the \code{label_threshold}.
#' @param label_sd A numeric that sets the number times +/- residual standard deviations to plot as a
#'  pair of horizontal dotted lines. Typical values could be 1 or 2 standard deviations.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the subtitle of the figure.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_alpha A numeric value that sets the alpha level of \code{pts_color}.
#' @param pts_size A numeric value that sets the size of the points.
#' @param trend_line A logical which if TRUE plots a polynomial based trend line across the residuals.
#' @param trend_line_color A string that sets the color of the trend line.
#' @param trend_line_size A numeric that sets the width of the trend line.
#' @param zero_line_color A string that sets the color of the zero horizontal reference line.
#' @param zero_line_size A numeric that sets the width of the zero horizontal reference line.
#' @param display_plot A logical that if TRUE will display the plot
#'
#' @importFrom data.table data.table
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggrepel geom_text_repel
#'
#' @return A grob object.
#'
#' @author Rick Dean
#'
#' @export
plot_residuals_check <- function(
  df = NULL,
  formula_obj = NULL,
  id_col = NULL,
  residual_label_threshold = NULL,
  leverage_label_threshold = NULL,
  label_color = "red",
  label_sd =NULL,
  title = NULL,
  subtitle = NULL,
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_alpha = 1.0,
  pts_size = 1,
  trend_line = TRUE,
  trend_line_color = "red",
  trend_line_size = 1.0,
  zero_line_color = "blue",
  zero_line_size = 1.4,
  display_plot = TRUE
){
  plot_fit_resid <- RregressPkg::plot_fit_residuals(
    df = df,
    formula_obj = formula_obj,
    id_col = id_col,
    label_threshold = residual_label_threshold,
    label_color = label_color,
    label_sd = label_sd,
    y_title = "Residuals",
    axis_text_size = axis_text_size,
    rot_y_tic_label = TRUE,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_alpha = pts_alpha,
    pts_size = pts_size,
    trend_line = trend_line,
    trend_line_color = trend_line_color,
    zero_line_color = zero_line_color,
    zero_line_size = zero_line_size
  )

  plot_fit_stand_residual<- RregressPkg::plot_fit_residuals(
    df = df,
    formula_obj = formula_obj,
    residual_standardized = TRUE,
    id_col = id_col,
    label_threshold = residual_label_threshold,
    label_color = label_color,
    y_title = "sqrt(Standardized Residuals)",
    axis_text_size = axis_text_size,
    rot_y_tic_label = TRUE,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_alpha = pts_alpha,
    pts_size = pts_size,
    trend_line = trend_line,
    trend_line_color = trend_line_color
  )

  if(!is.null(id_col)){
    id_v <- df[[id_col]]
  }else {
    id_v <- 1: nrow(df)
  }

  ols_lst <- RregressPkg::ols_calc(
    df = df,
    formula_obj = formula_obj
  )

  rse <- ols_lst$rse * sqrt(1 - diag(ols_lst$hat_mt))
  stand_resid_v <- ols_lst$residual_vals / rse

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
    labels_n = 3
  )

  influence_lst <- RregressPkg::plot_influence(
    df = df,
    formula_obj = formula_obj,
    id_col = id_col,
    label_threshold = leverage_label_threshold,
    axis_text_size = axis_text_size,
    rot_y_tic_label = TRUE,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_alpha = pts_alpha,
    pts_size = pts_size,
  )

  layout <- list(
    plots = list(plot_fit_resid, plot_fit_stand_residual, plot_qq, influence_lst$plot),
    rows = c(1, 2, 1, 2),
    cols = c(1, 1, 2, 2)
  )

  RplotterPkg::multi_panel_grid(
    layout = layout,
    col_widths = c(5,5),
    row_heights = c(5,5),
    title = title,
    subtitle = subtitle,
    display_plot = display_plot
  )
}




