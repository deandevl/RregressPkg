#' Function creates a scatter plot of observation's row number versus its influence measure
#' in the OLS estimate.
#'
#' @description Given the predictor and response data that defines an OLS model, an
#' estimate of each observation's influence on that model is plotted.  The measures of
#' influence include internal/external studentized residuals, difference in fits, and
#' Cook's distance.  The plot can be used to identify possible outlier data points.
#'
#' @param data_df A data frame containing values for both the observed predictors
#'  and response variable.
#' @param resp_str A string that names the column in \code{data_df} containing the response
#'  variable.
#' @param influence_meas A string that defines the type of influence measure to apply. Acceptable
#'  values include \dQuote{internal}, \dQuote{external}, \dQuote{dffits}, and \dQuote{cook} for
#'  internal/external studentized residuals, difference in fits, and Cook's distance respectively.
#' @param label_threshold A numeric that sets the measurement threshold beyond which observations
#'  will be labelled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  measurement is greater than the \code{label_threshold}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the observed response x axis title.
#' @param y_title A string that sets the fitted response y axis title.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param y_limits A numeric 2 element vector that sets the minimum and  maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param y_minor_breaks A numeric vector or function that defines the exact minor tic locations along the y axis.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_shape A numeric integer that sets the shape of the points. Typical values are 21 \dQuote{circle},
#'  22 \dQuote{square}, 23 \dQuote{diamond}, 24 \dQuote{up triangle}, 25 \dQuote{down triangle}.
#' @param pts_stroke A numeric that sets the drawing width for a point shape.
#' @param pts_alpha A numeric value that sets the alpha level of \code{pts_color}.
#' @param pts_size A numeric value that sets the size of the points.
#' @param show_major_grids A logical that controls the appearance of major grids.
#' @param show_minor_grids A logical that controls the appearance of minor grids.
#' @param do_x_title A logical that controls the appearance of the x axis title.
#' @param do_y_title A logical that controls the appearance of the y axis title.
#'
#' @importFrom data.table data.table
#' @importFrom RplotterPkg create_scatter_plot
#'
#' @return Function returns a ggplot2 object of observations versus influence measures
#'  along with a data frame with columns for the observation id and influence measure.
#'
#' @author Rick Dean
#'
#' @export
plot_obs_influence <- function(
  data_df = NULL,
  resp_str = NULL,
  influence_meas = "external",
  label_threshold = 3,
  label_color = "red",
  title = NULL,
  subtitle = NULL,
  x_title = "Observation ID",
  y_title = "Influence Value",
  rot_y_tic_label = FALSE,
  y_limits = NULL,
  y_major_breaks = waiver(),
  y_minor_breaks = waiver(),
  axis_text_size = 11,
  pts_color = "black",
  pts_fill = "white",
  pts_shape = 21,
  pts_stroke = 1,
  pts_alpha = 1.0,
  pts_size = 1,
  show_major_grids = TRUE,
  show_minor_grids = TRUE,
  do_x_title = TRUE,
  do_y_title = TRUE
){
  ols_regress_lst <- RregressPkg::ols_regress_calc(
    data_df = data_df,
    dep_str = resp_str
  )

  influence_vals <- ols_regress_lst$influence_df$Student_Del_Res
  if(influence_meas == "internal"){
    influence_vals <- ols_regress_lst$influence_df$Student_Res
  }else if(influence_meas == "dffits"){
    influence_vals <- ols_regress_lst$influence_df$Dffits
  }else if(influence_meas == "cook"){
    influence_vals <- ols_regress_lst$influence_df$Cook
  }

  dt <- data.table::data.table(
    row_number = 1:nrow(data_df),
    influence_vals = influence_vals
  )

  influence_plot <- RplotterPkg::create_scatter_plot(
    df = dt,
    aes_x = "row_number",
    aes_y = "influence_vals",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    rot_y_tic_label = rot_y_tic_label,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    y_minor_breaks = y_minor_breaks,
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

  label_data <- dt[abs(influence_vals) >= label_threshold]
  influence_plot <- influence_plot +
    ggplot2::geom_point(data = label_data, color = label_color, size = 2.5) +
    ggrepel::geom_text_repel(data = label_data, aes(label = row_number), color = label_color)

  return(
    list(
      ols = ols_regress_lst,
      plot = influence_plot
    )
  )
}



