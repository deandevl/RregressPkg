#' Function plots counts of missing values for a data frame's variables along with possible replacement.
#'
#' @description A ggplot2 bar plot of numeric missing value counts is produced along with the option to replace the values.
#' TODO: Be able to count/replace non-numeric missing values of a data frame.
#'
#' @details If the argument \code{replace_fun} is \code{NULL} then only a bar chart showing the missing value count for each variable is
#'  returned.
#'
#' @param df The source data frame with numeric and character variables.
#' @param variables A character vector of numeric variable names from \code{df} to be included in the plot and possible value replacement.
#' @param replace_fun A character string or function that sets the aggregate function for replacing missing values.
#'  Acceptable values are \dQuote{mean}, \dQuote{median}, \dQuote{locf} (last observation carried forward),
#'  \dQuote{nocb} (next observation carried backward). The parameter can also be a user defined function that
#'  accepts a vector of non-missing values for a column (as determined by \code{miss_values}) and returns a single
#'  replacement value. See an example in the \code{demos} folder.
#' @param miss_values A vector with numeric and character values that define in addition to \code{NA} and \code{NaN}, other values
#'  considered as missing. Examples might be a vector with \dQuote{na}, \dQuote{N/A}, \code{999}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the \code{title} and \code{subtitle}.
#' @param x_title A string that sets the x axis title.
#' @param y_title A string that sets the y axis title.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic label. When x tic labels are long,
#' @param bar_fill A string that sets the fill color for the bars.
#' @param bar_color A string that sets the outline color for the bars.
#' @param bar_alpha A numeric that sets the alpha component to \code{bar_color}.
#' @param bar_size A numeric that sets the outline thickness of the bars.
#' @param bar_width A numeric that sets the width of the bars.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param order_bars A string which will order the bars in a specific direction. Acceptable values are \dQuote{asc} or \dQuote{desc}
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_size A numeric that sets the size of the bar label
#' @param bar_label_color A string that sets the color of the bar labels
#'
#' @importFrom data.table as.data.table
#' @importFrom RplotterPkg create_bar_plot
#' @import ggplot2
#'
#' @return Returning a named list with:
#' \enumerate{
#'  \item \dQuote{missing_plot} -- a ggplot2 plot object where additional aesthetics may be added.
#'  \item \dQuote{replacement_df} -- a data.table copy of \code{df} with missing values replaced
#'   if \code{replace_fun} is not \code{NULL}.
#' }
#'
#' @author Rick Dean
#'
#' @export
plot_replace_missing <- function(
  df,
  variables = NULL,
  replace_fun = NULL,
  miss_values = NULL,
  title = NULL,
  subtitle = NULL,
  center_titles = FALSE,
  x_title = "Variables",
  y_title = "Missing Counts",
  rot_x_tic_angle = 0,
  bar_fill = "gray",
  bar_color = "black",
  bar_alpha = 1.0,
  bar_size = 1.0,
  bar_width = NULL,
  y_limits = NULL,
  y_major_breaks = waiver(),
  do_coord_flip = FALSE,
  order_bars = NULL,
  bar_labels = FALSE,
  bar_label_size = 6,
  bar_label_color = "black"
){

  dt <- data.table::as.data.table(df)

  if(is.null(miss_values)) {
    miss_values <- c(NA, NaN)
  }else{
    miss_values <- c(miss_values, NA, NaN)
  }

  missing_sums <- c()
  for(a_col in variables){
    missing_t_f <- data.table::fifelse(
      test = dt[, get(a_col)] %in% miss_values, yes = TRUE, no = FALSE
    )

    missing_sums <- append(missing_sums, sum(missing_t_f))

    not_missing_v <- dt[, get(a_col)][!missing_t_f]

    if(!is.null(replace_fun)) {
      if(!is.function(replace_fun)){
        if(replace_fun == "median") {
          median_val <- median(not_missing_v)
          replaced_v <- data.table::fifelse(!missing_t_f, as.character(dt[, get(a_col)]),  as.character(median_val))
          dt[, (a_col) := as.numeric(replaced_v)]
        }else if(replace_fun == "mean"){
          mean_val <- mean(not_missing_v)
          replaced_v <- data.table::fifelse(!missing_t_f, as.character(dt[, get(a_col)]),  as.character(mean_val))
          dt[, (a_col) := as.numeric(replaced_v)]
        }else if(replace_fun == "locf"){
          x <- dt[, get(a_col)]
          missing_na_v <- data.table::fifelse(
            test = x %in% miss_values, yes = as.numeric(NA), no = x
          )
          missing_no_na_v <- data.table::nafill(x = missing_na_v, type = "locf")
          dt[, (a_col) := missing_no_na_v]
        }else if(replace_fun == "nocb"){
          x <- dt[, get(a_col)]
          missing_na_v <- data.table::fifelse(
            test = x %in% miss_values, yes = as.numeric(NA), no = x
          )
          missing_no_na_v <- data.table::nafill(x = missing_na_v, type = "nocb")
          dt[, (a_col) := missing_no_na_v]
        }
      }else {
        replace_val <- replace_fun(not_missing_v)
        replaced_v <- data.table::fifelse(!missing_t_f, as.character(dt[, get(a_col)]),  as.character(replace_val))
        dt[, (a_col) := as.numeric(replaced_v)]
      }
    }
  }

  sums_missing_df <- data.frame(
    variable = as.factor(variables),
    missing_sum = missing_sums
  )

  aplot <- RplotterPkg::create_bar_plot(
    df = sums_missing_df,
    aes_x = "variable",
    aes_y = "missing_sum",
    title = title,
    subtitle = subtitle,
    x_title = x_title,
    y_title = y_title,
    center_titles = center_titles,
    rot_x_tic_angle = rot_x_tic_angle,
    rot_y_tic_label = TRUE,
    bar_fill = bar_fill,
    bar_color = bar_color,
    bar_alpha = bar_alpha,
    bar_size = bar_size,
    bar_width = bar_width,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    do_coord_flip = do_coord_flip,
    order_bars = order_bars,
    bar_labels = bar_labels,
    bar_label_size = bar_label_size,
    bar_label_color = bar_label_color
  )

  return(
    list(
      missing_plot = aplot,
      replacement_df = dt
    )
  )
}
