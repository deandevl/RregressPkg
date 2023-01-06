#' Function produces a ggplot2 based matrix scatter plot.
#'
#' Function produces a scatter plot of all combinations of a submitted
#'  data frame's variables.
#'
#' @param df The target data frame from which the scatter plots are layed out in a
#'  matrix like fashion.
#' @param title A string that sets the overall title.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for
#'  enhanced readability.
#' @param plot_dim A numeric that sets the overall width/height of the plot in inches.
#' @param display_plot A logical that if TRUE will display the plot
#'
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom grid unit
#' @import ggplot2
#'
#' @return A grob object. Note that if \code{display_plot} is set to \code{FALSE}, then
#'  to produce graphical output you must use \code{grid::grid.draw()} on the returned
#'  grob object.
#'
#' @author Rick Dean
#'
#' @export
plot_matrix_scatter <- function(
  df,
  title = NULL,
  rot_y_tic_label = FALSE,
  plot_dim = 5.5,
  display_plot = TRUE
){
  var_names <- colnames(df)
  n <- length(var_names) - 1
  vars_comb <- utils::combn(var_names, 2)
  plot_sz <- plot_dim/n

  # Are we doing a title
  if(!is.null(title)){
    title_grob <- grid::textGrob(
      label = title,
      gp = grid::gpar(col = "black", fontsize = 18, fontface = 2L)
    )
  }
  #define row heights
  heights_v <- rep(plot_sz, n)
  heights_units <- rep("in", n)

  if(!is.null(title)){
    heigths_v <- c(1.2, rep(plot_sz, n))
    heights_units <- c("cm", rep("in", n))
  }

  # create a gtable
  gt_n_n <- gtable::gtable(
    widths = grid::unit(x = rep(plot_sz, n), units = "in"),
    heights = grid::unit(heights_v, units = heights_units)
  )
  #gtable::gtable_show_layout(gt_n_n)  # for debug purposes

  top <- 0
  # add title to gt_n_n
  if(!is.null(title)){
    gt_n_n <- gtable::gtable_add_grob(
      x = gt_n_n,
      grobs = title_grob,
      t = 1,
      l = 1,
      r = n
    )
    top = 1
  }

  idx <- 1L
  for(i in 1:n){
    for(ii in i:n){
      if(i == ii){
        title_grob <- grid::textGrob(
          label = var_names[[i]],
          gp = grid::gpar(col = "black", fontsize = 18, fontface = 2L)
        )
        gt_n_n <- gtable::gtable_add_grob(
          x = gt_n_n,
          grobs = title_grob,
          t = top + i,
          l = ii,
          r = ii
        )
      }else {
        x <- vars_comb[1,idx]
        y <- vars_comb[2,idx]

        a_plot <- RplotterPkg::create_scatter_plot(
          df = df,
          aes_x = x,
          aes_y = y,
          x_title = x,
          y_title = y,
          rot_y_tic_label = rot_y_tic_label
        )

        plot_grob <- ggplot2::ggplotGrob(a_plot)

        gt_n_n <- gtable::gtable_add_grob(
          x = gt_n_n,
          grobs = plot_grob,
          t = top + i,
          l = ii,
          r = ii
        )

        idx <- idx + 1
      }
    }
  }
  if(display_plot){
    grid::grid.newpage()
    grid::grid.draw(gt_n_n )
  }

  return(gt_n_n )
}
