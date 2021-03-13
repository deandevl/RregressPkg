#' Function plots one predictor's effect on the response variable from an OLS model
#'
#' @description Function holds other predictor variables at their mean value and plots
#'  the response variable's values over selected values of a target predictor.
#'  "Confidence" or "prediction" type intervals can also be displayed around the
#'  response values.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param predictor_vals_df A single column data frame with values for a single predictor
#'  where its effect is of interest.
#' @param interval A string that sets the type of confidence interval.  Acceptable values
#'  are dQuote{confidence} or dQuote{prediction}.
#' @param confid_level A numeric that defines the confidence level for estimating confidence
#'  intervals. The default is 0.95.
#' @param CI_show A logical which if TRUE will plot the confidence interval around the fitted responses.
#' @param CI_fill A string that sets the fill color for the confidence interval.
#' @param CI_alpha A numeric that set the alpha color level for the confidence interval.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits Depending on the class of \code{aes_x}, a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use NA to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of \code{aes_x}, a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param show_pts A logical which if FALSE will plot only the lines if \code{connect} is TRUE.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_size A numeric value that sets the size of the points.
#' @param pts_line_alpha A numeric value that sets the alpha level of points and connected line.
#' @param connect A logical which if \code{TRUE} then points will be connected with a line.
#' @param line_size A numeric value that sets the thickness of lines if \code{connect} is TRUE.
#' @param line_color A string that sets the color of the lines if \code{connect} is TRUE.
#' @param connect_linetype A string that sets line type \code{twodash, solid, longdash, dotted, dotdash,
#'  dashed, blank} if \code{connect} is TRUE.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#'
#' @importFrom data.table data.table
#' @importFrom data.table setDT
#' @importFrom RplotterPkg create_scatter_plot
#' @import ggplot2
#'
#' @return Function returns a list with the predictor's effect response values and associated confidence intervals
#' along with a ggplot2 object.
#'
#' @author Rick Dean
#'
#' @export
plot_predictor <- function(
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
  pts_line_alpha = 1.0,
  connect = T,
  line_size = 1,
  line_color = "black",
  connect_linetype = "solid",
  do_x_title = T,
  do_y_title = T
){
  dt <- data.table::setDT(df)

  variables <- all.vars(terms(formula_obj, data = df))
  response_name <- variables[[1]]
  predictor_name <- colnames(predictor_vals_df)

  out_vars <- c(response_name, predictor_name)
  X_dt <- dt[, !..out_vars]
  X_means_dt <- X_dt[,lapply(X_dt,mean)]
  predictor_vals_df <- cbind(predictor_vals_df, X_means_dt)

  confidence_df <- RregressPkg::ols_predict_calc(
    df = df,
    formula_obj = formula_obj,
    predictor_vals_df = predictor_vals_df,
    interval = interval,
    confid_level = confid_level
  )

  predictor_effect_df <- cbind(predictor_vals_df, confidence_df)

  effect_plot <- RplotterPkg::create_scatter_plot(
    df = predictor_effect_df,
    aes_x = predictor_name,
    aes_y = "fit",
    title = title,
    subtitle = subtitle,
    center_titles = center_titles,
    x_title = x_title,
    y_title = y_title,
    rot_y_tic_label = rot_y_tic_label,
    x_limits = x_limits,
    x_major_breaks = x_major_breaks,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    axis_text_size = axis_text_size,
    show_pts = show_pts,
    pts_color = pts_color,
    pts_fill = pts_fill,
    pts_shape = pts_shape,
    pts_stroke = pts_stroke,
    pts_size = pts_size,
    pts_line_alpha = pts_line_alpha,
    connect = connect,
    line_size = line_size,
    line_color = line_color,
    connect_linetype = connect_linetype,
    do_x_title = do_x_title,
    do_y_title = do_y_title
  )

  if(CI_show){
    effect_plot <- effect_plot +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = CI_fill, alpha = CI_alpha)
  }

  return(list(
    effect_plot = effect_plot,
    predictor_effect_df = predictor_effect_df
  ))
}
