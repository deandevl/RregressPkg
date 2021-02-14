#' Function performs measures of influence to the submitted OLS model
#'
#' @description Function offers three different measures of influence including
#'  internal/external studentized residuals, difference in fits, and Cook's distance.
#'  Both a data frame and a plot of each observations influence on the OLS estimate are
#'  returned to help identify data points as possible outliers and/or high leverage.
#'
#' @param x Either an object of class "lm" from an OLS estimate or a data frame containing
#'  values for the predictor variables and the response variable along with optionally a column
#'  for observation identification.
#' @param obser_limit An integer that sets the maximum number of observations to be considered.
#'  The default is 500.
#' @param resp_col A string that names the column from data frame \code{x} containing the response
#'  values.
#' @param id_col An optional argument that names the column from data frame \code{x} providing
#'  each observation with a unique identification value.  If this argument is NULL then
#'  data frame row numbers are used for identification. Unless you have less than 30 observations,
#'  it is best to stay with row numbers and modify the \code{x_limits} and \code{x_major_breaks}
#'  arguments.
#' @param influence_meas A string that defines the type of influence measure to apply. Acceptable
#'  values include \dQuote{internal}, \dQuote{external}, \dQuote{dffits}, and \dQuote{cook} for
#'  internal/external studentized residuals, difference in fits, and Cook's distance respectively.
#' @param label_threshold A numeric that sets the measurement threshold beyond which observations
#'  will be labeled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  measurement is greater than the \code{label_threshold}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the observed response x axis title.
#' @param y_title A string that sets the fitted response y axis title.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for enhanced readability.
#' @param x_limits A numeric 2 element vector that sets the minimum and maximum for the x axis.
#' @param x_major_breaks A numeric vector or function that defines the exact major tic locations along the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
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
#' @importFrom data.table setDT
#' @importFrom RplotterPkg create_scatter_plot
#'
#' @return Function returns a named list with both a data frame of influence measures
#'  \dQuote{influence_df} for each observation along with a scatter plot of observations versus
#'  influence measure \dQuote{influence_plot}.
#'
#' @author Rick Dean
#'
#' @export
plot_influence <- function(
  x = NULL,
  obser_limit = 500,
  resp_col = NULL,
  id_col = NULL,
  influence_meas = "cook",
  label_threshold = 3,
  label_color = "red",
  title = NULL,
  subtitle = NULL,
  x_title = "Observation ID",
  y_title = "Influence Value",
  rot_y_tic_label = FALSE,
  x_limits = NULL,
  x_major_breaks = waiver(),
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
  dt <- NULL
  ols <- NULL
  if("lm" %in% class(x)){
    dt <- data.table::setDT(x$model)
  }else if("data.frame" %in% class(x)) {
    dt <- data.table::setDT(x)
  }else {
    stop('The "x" argument must be either an object of class "lm" or "data.frame" ')
  }

  Y <- as.matrix(dt[, ..resp_col])
  out_columns <- resp_col
  if(!is.null(id_col)){
    out_columns <- c(out_columns, id_col)
  }
  X <- as.matrix(dt[, !..out_columns])
  ols <- RregressPkg::ols_matrix_calc(X = X, Y = Y)

  influence_vals <- NULL

  if(influence_meas == "internal"){
    # compute studentized residuals or internally studentized residuals
    Hat_ii <- diag(ols$Hat)
    influence_vals <- (ols$Resid / sqrt(ols$mse * (1 - Hat_ii)))[,1]
  }else if(influence_meas == "external"){
    # compute the studentized deleted residuals or externally studentized residuals
    Hat_ii <- diag(ols$Hat)
    student_residuals <- ols$Resid / sqrt(ols$mse * (1 - Hat_ii))
    p <- ols$k + 1
    numer <- ols$n - p - 1
    denom <- ols$n - p - student_residuals^2
    influence_vals <- (student_residuals * sqrt(numer/denom))[,1]
  }else if(influence_meas == "dffits"){
    # compute the Difference in Fits(DFFITS)
    n <- ols$n
    if(n > obser_limit){
      n <- obser_limit
    }
    Hat_ii <- diag(ols$Hat)
    influence_vals <- vector(mode="numeric", length = n)
    Inter_v <- c(Inter = 1)
    X_I <- cbind(Inter_v,ols$X)
    for(i in 1:n){
      X_i <- ols$X[-i,,drop=F]
      Y_i <- ols$Y[-i,,drop=F]
      ols_i <- RregressPkg::ols_matrix_calc(X_i,Y_i)
      Fitted_i <- X_I %*% ols_i$Coef
      Dif <- ols$Fitted_val - Fitted_i
      Denom <- sqrt(ols_i$mse * Hat_ii)
      influence_vals[[i]] <- (Dif/Denom)[i,]
    }
  }else if(influence_meas == "cook"){
    # compute Cook's distance measure
    p <- ols$k + 1
    influence_vals <- vector(mode="numeric", length = ols$n)
    Hat_ii <- diag(ols$Hat)
    for(i in 1:ols$n){
      Residual <- ((ols$Y - ols$Fitted_val)^2)/(p * ols$mse)
      Leverage <- Hat_ii/(1 - Hat_ii)^2
      influence_vals[[i]] <- (Residual * Leverage)[i,]
    }
  }

  # create a plot object of the influence values
  if(!is.null(id_col)){
    id_v <- unlist(dt[, ..id_col])
    influence_dt <- data.table::data.table(
      id = factor(id_v, levels = id_v)
    )
  }else {
    id_v <- 1: nrow(dt)
    influence_dt <- data.table::data.table(
      id = id_v
    )
  }
  influence_dt[, influence_vals := influence_vals]

  influence_plot <- RplotterPkg::create_scatter_plot(
    df = influence_dt,
    aes_x = "id",
    aes_y = "influence_vals",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    rot_y_tic_label = rot_y_tic_label,
    x_limits = x_limits,
    x_major_breaks = x_major_breaks,
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

  label_data <- influence_dt[abs(influence_vals) >= label_threshold]
  influence_plot <- influence_plot +
    ggplot2::geom_point(data = label_data, color = label_color, size = 2.5) +
    ggrepel::geom_text_repel(data = label_data, aes(label = id), color = label_color)

  return(
    list(
      influence = influence_dt,
      plot = influence_plot
    )
  )
}
