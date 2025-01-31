#' Function performs measures of influence to the submitted OLS model
#'
#' Function offers three different measures of influence including
#'  internal/external studentized residuals, and Cook's distance.
#'  Both a data frame and a plot of each observations influence on the OLS estimate are
#'  returned to help identify data points as possible outliers and/or high leverage.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: \code{y ~ log(a) + b + I(b^2)}.
#' @param id_col An optional argument that names the column from data frame 'df' providing
#'  each observation with a unique identification value.  If this argument is \code{NULL} then
#'  data frame row numbers are used for identification. Unless you have less than 30 observations,
#'  it is best to stay with row numbers and modify the 'x_limits' and 'x_major_breaks'
#'  arguments.
#' @param influence_meas A string that defines the type of influence measure to apply. Acceptable
#'  values include "internal", "external", "dffits", and "cook" for
#'  internal/external studentized residuals, difference in fits, and Cook's distance respectively.
#' @param label_threshold A numeric that sets the measurement threshold beyond which observations
#'  will be labeled with their id.
#' @param label_color A string that sets the label/point color for observations whose absolute
#'  measurement is greater than the 'label_threshold'.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_title A string that sets the observed response x axis title. If \code{NULL} then
#'  the x axis title does not appear. The default is "Observation ID".
#' @param y_title A string that sets the fitted response y axis title. If \code{NULL} then
#'  the y axis title does not appear. The default is "Influence Value".
#' @param rot_y_tic_label A logical which if \code{TRUE} rotates the y tic labels 90 degrees for enhanced readability.
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
#'
#' @return Function returns a named list with both a data frame of influence measures
#'  \dQuote{influence_df} for each observation along with a scatter plot of observations versus
#'  influence measure \dQuote{influence_plot}.
#'
#' @examples
#' library(wooldridge)
#' library(ggplot2)
#' library(data.table)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' rdchem_dt <- data.table::as.data.table(wooldridge::rdchem) |>
#' _[, .(rdintens, sales, profmarg)]
#'
#' formula_obj <- rdintens ~ sales + profmarg
#' rdchem_influence_lst <- RregressPkg::plot_influence(
#'   df = rdchem_dt,
#'   formula_obj = formula_obj,
#'   influence_meas = "cook",
#'   label_threshold = 3.0,
#'   title = "Cook's Distance for Data Point Influence",
#'   subtitle = "Source: Wooldridge::rdchem",
#'   rot_y_tic_label = TRUE
#' )
#' a_plot <- rdchem_influence_lst$plot
#'
#' @import ggplot2
#' @importFrom RplotterPkg create_scatter_plot
#'
#' @export
plot_influence <- function(
  df = NULL,
  formula_obj = NULL,
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
  show_minor_grids = TRUE
){
  # compute the OLS
  X_mt <- stats::model.matrix(formula_obj, data = df)
  Y_mt <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))
  n <- nrow(X_mt)
  k <- ncol(X_mt)
  qr_lst <- base::qr(X_mt)
  Qf_mt <- base::qr.Q(qr_lst) # n x k
  Hat_ii <- diag(Qf_mt %*% t(Qf_mt)) # n x 1
  fitted_mt <- (Qf_mt %*% t(Qf_mt) %*% Y_mt) # n x 1
  resid_mt <- base::qr.resid(qr_lst, Y_mt) # n x 1
  sse <- (t(resid_mt) %*% resid_mt)[1,1]
  mse <- sse/(n - k)

  if(influence_meas == "internal"){
    # compute studentized residuals or internally studentized residuals
    influence_vals <- resid_mt / sqrt(mse * (1 - Hat_ii))
  }else if(influence_meas == "external"){
    # compute the studentized deleted residuals or externally studentized residuals
    student_residuals <- resid_mt / sqrt(mse * (1 - Hat_ii))
    numer <- n - k - 1
    denom <- n - k - student_residuals^2
    influence_vals <- student_residuals * sqrt(numer/denom)
  }else if(influence_meas == "dffits"){
    # compute the Difference in Fits(DFFITS)
    influence_vals <- vector(mode="numeric", length = n)
    for(i in 1:n){
      X_i_mt <- X_mt[-i,,drop=F]
      Y_i_mt <- Y_mt[-i,,drop=F]
      qr_i_lst <- base::qr(X_i_mt)
      coef_i_mt <- (base::qr.coef(qr_i_lst, Y_i_mt))
      fitted_i_mt <- X_mt %*% coef_i_mt
      dif_fit_i_mt <- fitted_mt - fitted_i_mt
      resid_i_mt <- base::qr.resid(qr_i_lst, Y_i_mt)
      sse_i <- (t(resid_i_mt) %*% resid_i_mt)[1,1]
      mse_i <- sse_i/(n - k - 1)
      denom <- sqrt(mse_i * Hat_ii)
      influence_vals[[i]] <- (dif_fit_i_mt/denom)[i,]
    }
    influence_vals <- as.data.frame(influence_vals)
  }else if(influence_meas == "cook"){
    # compute Cook's distance measure
    influence_vals <- vector(mode="numeric", length = n)

    for(i in 1:n){
      Residual <- ((resid_mt)^2)/(k * mse)
      Leverage <- Hat_ii/(1 - Hat_ii)^2
      influence_vals[[i]] <- (Residual * Leverage)[i,]
    }

    influence_vals <- as.data.frame(influence_vals)
  }

  # create a plot object of the influence values
  id <- NULL
  if(!is.null(id_col)){
    id_v <- df[[id_col]]
    influence_df <- data.frame(
      id = factor(id_v, levels = id_v)
    )
  }else {
    id_v <- 1: nrow(df)
    influence_df <- data.frame(
      id = id_v
    )
  }

  colnames(influence_vals) <- "influence_vals"
  influence_df <- cbind(influence_df, influence_vals)

  influence_plot <- RplotterPkg::create_scatter_plot(
    df = influence_df,
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
    show_major_grids = show_major_grids,
    show_minor_grids = show_minor_grids
  )

  label_data_df <- subset(influence_df, abs(influence_vals) >= label_threshold)

  if(nrow(label_data_df) > 0){
    influence_plot <- influence_plot +
      ggplot2::geom_point(data = label_data_df, color = label_color, size = 2.5) +
      ggplot2::geom_text(
        data = label_data_df,
        aes(
          label = id,
          hjust = -0.2,
          vjust = -0.4
        ),
        color = label_color
      )
  }

  return(
    list(
      influence = influence_df,
      plot = influence_plot
    )
  )
}
