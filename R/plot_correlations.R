#' Function produces a heatmap plot of a correlation matrix.
#'
#' @description With the submission of a correlation matrix, the function returns a ggplot2 plot object that takes the form
#'  of a color coded heatmap of the upper triangular portion of a symmetric correlation matrix.
#'
#' @param corr A square, symmetric, named, numeric matrix of correlation values between -1 to 1 in value. This is typically
#'  the Value from \code{stats::cor()}.
#' @param cluster_corr A logical that if TRUE will cluster the correlations across the heatmap scales.
#' @param tile_border_color A numeric that sets the heatmap tile's border color.
#' @param label_sz A numeric that sets the size of the label.
#' @param label_color A string that sets the label's color.
#' @param label_fontface A string that sets the label's font face. Acceptable values are \dQuote{plain}, \dQuote{bold},
#'  \dQuote{italic}, \dQuote{bold.italic}.
#' @param label_alpha A numeric that sets the label's alpha value.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param axis_text_size A numeric that sets the font size along the axis'. Default is 11.
#' @param rot_x_tic_angle A numeric that sets the angle of the x axis tic labels.
#' @param scale_limits A numeric vector of two elements giving the min/max values for the correlation scale. The default is
#'  \code{c(-1.0, 1.0)}
#' @param scale_colors A character vector of two elements giving the low/high colors that are interpolated across \code{scale_limits}.
#' @param mid_scale_color A character string that sets the color for the mid value of \code{scale_limits}.
#' @param legend_title A character string that sets the legend's title.
#' @param show_legend A logical that controls the appearance of the legend.
#'
#' @importFrom RplotterPkg create_heatmap
#' @import ggplot2
#'
#' @return A ggplot2 plot object.
#'
#' @author Rick Dean
#'
#' @export
plot_correlations <- function(
  corr,
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
  corr <- round(corr,2)
  if(cluster_corr){
    hc <- stats::hclust(as.dist(1 - corr))
    corr <- corr[hc$order, hc$order]
  }
  corr[lower.tri(corr)] <- NA
  corr <- t(corr)

  dn_lst <- dimnames(corr)
  labels_df <- expand.grid(dn_lst, KEEP.OUT.ATTRS = FALSE)

  missing_mat <- is.na(corr)
  data_v <- corr[!missing_mat]
  labels_df <- labels_df[!missing_mat, ]

  values_df <- setNames(data.frame(data_v), "value")
  df <- cbind(labels_df, values_df)

  heatmap_plot <- RplotterPkg::create_heatmap(
    df = df,
    aes_x = "Var1",
    aes_y = "Var2",
    aes_fill = "value",
    aes_label = "value",
    tile_color = tile_border_color,
    tile_square = TRUE,
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
  )

  heatmap_plot <- heatmap_plot +
    ggplot2::scale_fill_gradient2(
      low = scale_colors[1],
      high = scale_colors[2],
      mid = mid_scale_color,
      midpoint = 0,
      limit = scale_limits,
      name = legend_title
    )
  return(heatmap_plot)
}
