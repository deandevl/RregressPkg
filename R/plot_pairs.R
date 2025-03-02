#' @title plot_pairs
#'
#' @description
#' Function produces a ggplot2 based scatter plot of all pairs of a dataframe's variables.
#'
#' In creating the 'var_name_scaling' list, if we are looking at some regression model,
#'   then suggest listing the response variable last.
#'
#' @param df The required data frame from which the scatter plots' values are derived and laid out in a
#'  matrix like fashion.
#' @param var_name_scaling A required named list where the name is the variable name from 'df'
#'   and the value is a vector defining the corresponding major axis scaling breaks. If the value
#'   is \code{NULL}, then ggplot2 figures out the variable's scaling. The vector can be numeric/dates or
#'   anything acceptable in scaling a ggplot2 axis.
#' @param title A string that sets the overall title.
#' @param title_sz A numeric that sets the title's font size. Default is 14.
#' @param axis_text_sz A numeric that sets the font size along the axis'. Default is 6.
#' @param rot_y_tic_label A logical which if TRUE rotates the y tic labels 90 degrees for
#'  enhanced readability.
#' @param plot_dim A numeric that sets the overall width/height of the plot in centimeters.
#'
#' @return a ggplot2 plot object
#'
#' @examples
#' library(ggplot2)
#' library(ggplotify)
#' library(gtable)
#' library(data.table)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' # Advertising media
#' var_name_scaling_lst <- list(
#' TV = NULL,
#' radio = NULL,
#' newspaper = NULL,
#' sales = NULL
#' )
#'
#' a_plot <- RregressPkg::plot_pairs(
#'   df = RregressPkg::advertising,
#'   var_name_scaling = var_name_scaling_lst,
#'   title = "Advertising Predictors of Sales",
#'   rot_y_tic_label = TRUE
#' )
#'
#' # Timber volume
#' data(trees)
#'
#' var_name_scaling_lst <- list(
#' Girth = seq(5, 25, 5),
#' Height = seq(60, 90, 5),
#' Volume = NULL
#' )
#' a_plot <- RregressPkg::plot_pairs(
#'   df = trees,
#'   var_name_scaling = var_name_scaling_lst,
#'   title = "Cherry Trees"
#' )
#'
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_grob
#' @import ggplot2
#' @importFrom ggplotify as.ggplot
#'
#' @export
plot_pairs <- function(
  df,
  var_name_scaling,
  title = NULL,
  title_sz = 14,
  axis_text_sz = 6,
  rot_y_tic_label = FALSE,
  plot_dim = 12
){
  if(is.null(df)){
    stop("A dataframe is required and must not be NULL")
  }
  if(is.null(var_name_scaling)){
    stop("The parameter 'var_name_scaling' must be a named list of variable scaling and not NULL")
  }

  var_names <- names(var_name_scaling)
  n <- length(var_names) - 1
  vars_comb <- utils::combn(var_names, 2)
  plot_sz <- plot_dim/n

  var_limits <- vector(mode = "list", length = length(var_names))
  names(var_limits) <- var_names
  for(name in var_names){
    if(is.null(var_name_scaling[[name]])){
      var_name_scaling[[name]] <- ggplot2::waiver()
    }else{
      var_limits[[name]] <- c(min(var_name_scaling[[name]]), max(var_name_scaling[[name]]))
    }
  }

  # Are we doing a title
  if(!is.null(title)){
    title_grob <- grid::textGrob(
      label = title,
      gp = grid::gpar(col = "black", fontsize = title_sz, fontface = 2L)
    )
  }
  #define row heights
  heights_v <- rep(plot_sz, n)
  heights_units <- rep("cm", n)

  if(!is.null(title)){
   heights_v <- c(1, heights_v)
   heights_units <- c("cm", heights_units)
  }

  # create a gtable
  gt_n_n <- gtable::gtable(
    widths = grid::unit(x = rep(plot_sz, n), units = "cm"),
    heights = grid::unit(heights_v, units = heights_units)
  )

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
  #gtable::gtable_show_layout(gt_n_n)  # for debug purposes

  idx <- 1L
  for(i in 1:n){
    for(ii in i:n){
      x <- vars_comb[1,idx]
      y <- vars_comb[2,idx]

      a_plot <- RplotterPkg::create_scatter_plot(
        df = df,
        aes_x = x,
        aes_y = y,
        x_limits = var_limits[[x]],
        x_major_breaks = var_name_scaling[[x]],
        y_limits = var_limits[[y]],
        y_major_breaks = var_name_scaling[[y]],
        x_title = x,
        y_title = y,
        axis_text_size = axis_text_sz,
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

  a_plot <- ggplotify::as.ggplot(gt_n_n)
  return(a_plot)
}
