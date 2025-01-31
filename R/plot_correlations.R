#' @title plot_correlations
#'
#' @description
#' Function computes simple Pearson correlations between variables of a data frame/matrix along with a heatmap plot.
#'
#' The data frame/matrix should have observations along the rows without NA values
#'  and the columns of variables are assumed to be numeric. The ggplot2 plot object takes the form
#'  of a color coded heatmap of the upper triangular portion of a symmetric correlation matrix.
#'
#' @param x A numeric data frame or matrix with variable columns and their observational values along the rows.
#' @param create_plot A logical that if \code{TRUE} will create a heatmap plot of the correlations.
#' @param cluster_corr A logical that if \code{TRUE} will cluster the correlations across the heatmap scales.
#' @param tile_border_color A numeric that sets the heatmap tile's border color.
#' @param label_sz A numeric that sets the size of the label.
#' @param label_color A string that sets the label's color.
#' @param label_fontface A string that sets the label's font face. Acceptable values are "plain", "bold",
#'  "italic", "bold.italic".
#' @param label_alpha A numeric that sets the label's alpha value.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param rot_x_tic_angle A numeric that sets the angle of the x axis tic labels.
#' @param scale_limits A numeric vector of two elements giving the min/max values for the correlation scale. The default is
#'  \code{c(-1.0, 1.0)}
#' @param scale_colors A character vector of two elements giving the low/high colors that are interpolated across 'scale_limits'.
#' @param mid_scale_color A character string that sets the color for the mid value of 'scale_limits'.
#' @param legend_title A character string that sets the legend's title.
#' @param show_legend A logical that controls the appearance of the legend.
#'
#' @return A named list with a data frame of correlations between the variables and a ggplot2 plot object.
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' midwest_dt <- data.table::as.data.table(ggplot2::midwest)  |>
#' _[, .(popdensity, percwhite, percblack, popadults, perchsd,
#'       percollege, percprof, percbelowpoverty, percchildbelowpovert,
#'       percadultpoverty, percelderlypoverty, inmetro)
#' ]
#'
#' midwest_corr_lst <- RregressPkg::plot_correlations(
#'   x = midwest_dt,
#'   title = "Correlations of Midwest Demographic Variables",
#'   subtitle = "Source: ggplot2::midwest",
#'   label_sz = 6
#' )
#' a_plot <- midwest_corr_lst$corr_plot
#'
#' @importFrom RplotterPkg create_heatmap
#' @import ggplot2
#'
#' @export
plot_correlations <- function(
  x = NULL,
  create_plot = TRUE,
  cluster_corr = FALSE,
  tile_border_color = "white",
  label_sz = 11,
  label_color = "black",
  label_fontface = "plain",
  label_alpha = 1.0,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  axis_text_size = 11,
  rot_x_tic_angle = 40,
  scale_limits = c(-1.0, 1.0),
  scale_colors = c("blue","green"),
  mid_scale_color = "white",
  legend_title = ggplot2::waiver(),
  show_legend = TRUE
){
  n <- nrow(x)

  if(!anyNA(x)){
    # compute variable means
    means_v <- apply(X = x, MARGIN = 2, FUN = function(x){
      mean(x)
    })

    # create a means for each column of variables
    M <- matrix(means_v, nrow = 1)
    means_mt <- matrix(data = 1, nrow = n) %*% M

    # subtract the means from data
    x_mt <- as.matrix(x)
    diff_mt <- x_mt - means_mt

    # create covariance matrix
    var_cor_mt <- t(diff_mt) %*% diff_mt * (n - 1)^-1

    # compute variables sd and sd cross products
    var_mt <- diag(var_cor_mt)
    sd_mt <- sqrt(var_mt)
    sd_product_mt <- sd_mt %*% t(sd_mt)

    # divide var-cor matrix by sd cross products
    corr_mt <- round(var_cor_mt / sd_product_mt, digits = 2)

    # create plot
    if(create_plot){
      if(cluster_corr){
        hc <- stats::hclust(stats::as.dist(1 - corr_mt))
        corr_mt <- corr_mt[hc$order, hc$order]
      }
      corr_mt[lower.tri(corr_mt)] <- NA
      corr_mt <- t(corr_mt)

      dn_lst <- dimnames(corr_mt)
      labels_df <- expand.grid(dn_lst, KEEP.OUT.ATTRS = FALSE)

      missing_mat <- is.na(corr_mt)
      data_v <- corr_mt[!missing_mat]
      labels_df <- labels_df[!missing_mat, ]

      values_df <- stats::setNames(data.frame(data_v), "value")
      df <- cbind(labels_df, values_df)

      heatmap_plot <- RplotterPkg::create_heatmap(
        df = df,
        aes_x = "Var1",
        aes_y = "Var2",
        aes_fill = "value",
        aes_label = "value",
        tile_color = tile_border_color,
        label_sz = label_sz,
        label_color = label_color,
        label_fontface = label_fontface,
        label_alpha = label_alpha,
        title = title,
        subtitle = subtitle,
        center_titles = center_titles,
        x_title = NULL,
        y_title = NULL,
        rot_x_tic_angle = rot_x_tic_angle,
        rot_y_tic_label = TRUE,
        axis_text_size = axis_text_size,
        show_legend = show_legend
      ) +
        ggplot2::scale_fill_gradient2(
          low = scale_colors[1],
          high = scale_colors[2],
          mid = mid_scale_color,
          midpoint = 0,
          limit = scale_limits,
          name = legend_title
        )
      return(list(
        corr = corr_mt,
        corr_plot = heatmap_plot
      ))
    }else{
      return(corr_mt)
    }
  }else {
    stop("Numeric data frame/matrix must not have NA values")
  }
}
