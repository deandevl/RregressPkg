#' @title plot_pca
#'
#' @description
#' Function produces several ggplot2 based plots to review principal components analysis (PCA).
#'
#' With the submission of a dataframe with measurements and samples, the function calls \code{stats::prcomp} to
#' perform a PCA of the data.  As an example, a regression application could consist of a set of measurements for the columns
#' and samples for the rows.  The function returns:
#'
#'   1. A pca class object returned from \code{stats::prcomp()}
#'
#'   2. A vector of component percentages.
#'
#'   3. A ggplot2 scatter plot object of samples across an x-y pair of principal components.
#'
#'   4. A ggplot2 circle plot object of the loadings or correlations of the measurements with an x-y pair of principal components.
#'
#'   5. A ggplot2 table of the loadings.
#'
#'   6. A ggplot2 object that assembles 3, 4, and 5 above into one figure.
#'
#' @param df The data frame containing rows of observations across columns of numeric measurements.
#' @param measures A vector of column names from 'df' to used in the PCA.
#' @param center A logical indicating whether the variables should be shifted to zero centered.
#' @param scale. A logical indicating whether the variables should be scaled to have unit variance before the
#'  analysis takes place.
#' @param tol A value indicating the magnitude below which components should be omitted. Components
#'  are omitted if their standard deviations are less than or equal to 'tol' times
#'  the standard deviation of the first component.
#' @param rank. A number specifying the maximal rank, i.e. maximal number of principal components to be used.
#'  If NULL then the length of the 'measures' argument.
#' @param pca_pair A string vector that names the pair of components of interest. Acceptable values
#'   are "PC1", "PC2", "PC3", ...
#' @param pca_values A string that sets the type of PCA values to display. Acceptable values are "loading" or
#'  "correlation".
#' @param aes_fill A string that sets the variable name from 'df' for the aesthetic mapping for fill.
#' @param aes_label A string that sets the variable name from 'df' for the aesthetic mapping for labeling
#'  observations.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param x_limits Depending on the class of 'measures', a numeric/Date/POSIXct 2 element vector that sets the minimum
#'  and maximum for the x axis. Use \code{NA} to refer to the existing minimum and maximum.
#' @param x_major_breaks Depending on the class of 'measures', a numeric/Date/POSIXct vector or function that defines
#'  the exact major tic locations along the x axis.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use \code{NA} to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param pts_color A string that sets the color of the points.
#' @param pts_fill A string that sets the fill color of the points.
#' @param pts_alpha A numeric value that sets the alpha level of 'pts_fill'.
#' @param pts_size A numeric value that sets the size of the points.
#' @param figure_width An numeric that sets the width of the overall figure in inches.
#' @param header_font_sz A numeric that defines the font size (in pixels) of table's headers.
#' @param show_meas_table A logical that if \code{TRUE} will display the table of loadings/correlations.
#'
#' @return Returning a named list with:
#' \enumerate{
#'  \item "pca" -- A list object of of class \code{prcomp} containing the results of the completed PCA.
#'  \item "percent_var" -- A numeric vector showing the percent of variance for each component.
#'  \item "samp_plot" -- A ggplot scatter plot object of samples across an x-y pair of principal components.
#'  \item "loadings_plot" -- A ggplot plot object of the loadings or correlations of the measurements with an x-y
#'   pair of principal components.
#'  \item "loadings_table_plot" -- A table showing the measurement loadings or correlations across all the principal components.
#'  \item "figure_plot" -- A multi-paneled ggplot object that assembles "samp_plot", "loadings_plot", and
#'   "loadings_table_plot" into one figure.
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom gtable gtable_add_grob
#' @importFrom gtable gtable
#' @importFrom RplotterPkg create_scatter_plot
#' @importFrom RplotterPkg create_table_graphic
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#' library(gtable)
#' library(ggplotify)
#' library(RplotterPkg)
#' library(RregressPkg)
#'
#' measurements <- colnames(RregressPkg::pilots)[2:7]
#' pilots_pca_lst <- RregressPkg::plot_pca(
#'   df = RregressPkg::pilots,
#'   measures = measurements,
#'   center = TRUE,
#'   scale. = TRUE,
#'   rank. = 4,
#'   aes_fill = "Group",
#'   pts_size = 2,
#'   x_limits = c(-4, 2),
#'   x_major_breaks = seq(-4, 2, 1),
#'   title = "Principal Components of Pilots and Apprentices",
#'   subtitle = "6 tested attributes from 20 pilots and 20 apprentices"
#' )
#' pca <- pilots_pca_lst$pca
#' pca_percent <- pilots_pca_lst$percent_var
#' figure_plot <- pilots_pca_lst$figure_plot
#'
#' pca
#'
#' summary(pca)
#'
#' @export
plot_pca <- function(
  df = NULL,
  measures = NULL,
  center = FALSE,
  scale. = FALSE,
  tol = NULL,
  rank. = NULL,
  pca_pair = c("PC1", "PC2"),
  pca_values = "loading",
  aes_fill = NULL,
  aes_label = NULL,
  title = NULL,
  subtitle = NULL,
  x_limits = NULL,
  x_major_breaks = waiver(),
  y_limits = NULL,
  y_major_breaks = waiver(),
  pts_color = "black",
  pts_fill = "white",
  pts_alpha = 1.0,
  pts_size = 1,
  figure_width = 10, # in inches
  header_font_sz = 9, # in pixels
  show_meas_table = TRUE
){
    x <- y <- x1 <- y1 <- x2 <- y2 <- NULL

    dt <- data.table::as.data.table(df)
    measures_dt <- dt[, measures, with=FALSE]
    pca <- stats::prcomp(measures_dt, center = center, scale. = scale., tol = tol, rank. = rank.)
    percent_var <- pca$sdev^2/sum(pca$sdev^2)*100

    if(pca_values == "loading"){
      pca_values_df <-  round(as.data.frame(pca$rotation), digits = 2)
    }else if(pca_values == "correlation"){
      pca_values_df <- round(as.data.frame(stats::cor(measures_dt, pca$x)), digits = 2)
    }

    plot_w_h <- figure_width/2

    titles <- list()
    # Are we doing a title
    if(!is.null(title)){
      titles[["a_title"]] <- grid::textGrob(label = title, gp = grid::gpar(col = "black", fontsize = 18, fontface = 2L))
    }
    # Are we doing a subtitle
    if(!is.null(subtitle)){
      titles[["a_subtitle"]] <- grid::textGrob(label = subtitle, gp = grid::gpar(col = "black", fontsize = 14))
    }

    #create a scatter plot of observations
    samples_dt <- as.data.table(pca$x)
    if(!is.null(aes_fill)){
      samples_dt[, (aes_fill) := dt[[aes_fill]]]
    }

    samp_plot <- RplotterPkg::create_scatter_plot(
      df = samples_dt,
      aes_x = pca_pair[1],
      aes_y = pca_pair[2],
      aes_fill = aes_fill,
      x_limits = x_limits,
      x_major_breaks = x_major_breaks,
      y_limits = y_limits,
      y_major_breaks = y_major_breaks,
      pts_color = pts_color,
      pts_fill = pts_fill,
      pts_line_alpha = pts_alpha,
      pts_size = pts_size
    )

    if(!is.null(aes_label)){
      samp_plot <- samp_plot +
        ggplot2::geom_text(
          data = samples_dt,
          aes(
            label = !!sym(aes_label),
            hjust = -0.2,
            vjust = -0.4
          )
        )
    }

    # create a circle plot of pca loadings
    # define a circle function to plot
    circle <- function(center = c(0,0), npoints = 100){
      r <- 1
      tt <- seq(0, 2 * pi, length = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[1] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    corcir <- circle()
    arrows <- data.frame(
      x1 = rep(0, nrow(pca_values_df)),
      y1 = rep(0, nrow(pca_values_df)),
      x2 = pca_values_df[[pca_pair[1]]],
      y2 = pca_values_df[[pca_pair[2]]]
    )

    loadings_plot <- ggplot2::ggplot() +
      ggplot2::geom_path(data = corcir, aes(x = x, y = y), color = "gray65") +
      ggplot2::geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), color = "gray65") +
      ggplot2::geom_point(data = pca_values_df, aes(x = !!rlang::sym(pca_pair[1]), y = !!rlang::sym(pca_pair[2]))) +
      ggplot2::geom_text(
        data = pca_values_df,
        mapping = aes(x = !!rlang::sym(pca_pair[1]), y = !!rlang::sym(pca_pair[2]),
                      label = rownames(pca_values_df), vjust = -0.4)) +
      geom_hline(yintercept = 0, color = "black") +
      ggplot2::geom_vline(xintercept = 0, color = "black") +
      labs(x = pca_pair[1], y = pca_pair[2]) +
      ggplot2::scale_x_continuous(
        limits = c(-1,1),
        breaks = seq(-1, 1, .2)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-1,1),
        breaks = seq(-1, 1, .2)
      ) +
      ggplot2::theme(
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", color = "gray")
      )

    # create loadings_table_grob
    if(show_meas_table){
      pca_values_copy <- pca_values_df
      pca_values_mt <- t(as.matrix(pca_values_copy))
      table_plot_df <- as.data.frame(pca_values_mt)
      loadings_table_grob <-  RplotterPkg::create_table_graphic(
        df = table_plot_df,
        table_width = figure_width,
        head_font_sz = header_font_sz,
        return_gtable = TRUE
      )
      loadings_table_plot <- ggplotify::as.ggplot(loadings_table_grob)
    }
    col_widths <- c(plot_w_h, plot_w_h)

    row_heights <- c(rep(.26, length(titles)))
    row_heights <- c(row_heights, plot_w_h)

    if(show_meas_table){
      loadings_table_height <- 0.5 + 0.04 + 0.2 * nrow(pca_values_df) # heading + line segment + data rows in inches
      row_heights <- c(row_heights, loadings_table_height)
    }

    # create figure_gtable gtable
    figure_gtable <- gtable::gtable(
      name = "figure_gtable",
      widths = grid::unit(x = col_widths, units = "in"),
      heights = grid::unit(x = row_heights, units = "in")
    )

    # add titles to figure_gtable
    idx <- 1
    for(item in titles){
      figure_gtable <- gtable::gtable_add_grob(
        x = figure_gtable,
        grobs = item,
        t = idx,
        l = 1,
        r = 2
      )
      idx <- idx + 1
    }

    # add plots to figure_gtable
    # add samp_plot
    figure_gtable <- gtable::gtable_add_grob(
      x = figure_gtable,
      grobs = ggplot2::ggplotGrob(samp_plot),
      t = length(titles) + 1,
      l = 1
    )

    #add loadings_plot to figure_gtable
    figure_gtable <- gtable::gtable_add_grob(
      x = figure_gtable,
      grobs = ggplot2::ggplotGrob(loadings_plot),
      t = length(titles) + 1,
      l = 2
    )

    # add loadings_table_grob to figure_gtable
    if(show_meas_table){
      figure_gtable <- gtable::gtable_add_grob(
        x = figure_gtable,
        grobs = loadings_table_grob,
        t = length(titles) + 2,
        l = 1,
        r = 2
      )
    }

    # add row spacing
    figure_gtable <- gtable::gtable_add_row_space(
      x = figure_gtable,
      height = grid::unit(0.2, "in")
    )
    figure_plot <- ggplotify::as.ggplot(figure_gtable)

    return(list(
      pca = pca,
      percent_var = percent_var,
      samp_plot = samp_plot,
      loadings_plot = loadings_plot,
      loadings_table_plot = loadings_table_plot,
      figure_plot = figure_plot
    ))
}
