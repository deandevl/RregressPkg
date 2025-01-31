#' @title plot_replace_missing
#'
#' @description
#' Function plots counts of missing values for a data frame's variables along with possible replacement.
#'
#' A ggplot2 bar plot of numeric missing value counts is produced along with the option to replace the values.
#' TODO: Be able to count/replace non-numeric missing values of a data frame.
#'
#'  If the argument 'replace_fun' is \code{NULL} then only a bar chart showing the missing value count for each variable is
#'  returned.
#'
#' @param df The source data frame with numeric and character variables.
#' @param variables A character vector of numeric variable names from 'df' to be included in the plot and possible value replacement.
#' @param replace_fun A character string or function that sets the aggregate function for replacing missing values.
#'  Acceptable values are "mean", "median", "locf" (last observation carried forward),
#'  "nocb" (next observation carried backward). The parameter can also be a user defined function that
#'  accepts a vector of non-missing values for a column (as determined by 'miss_values') and returns a single
#'  replacement value. See an example below.
#' @param miss_values A vector with numeric and character values that define in addition to \code{NA} and \code{NaN}, other values
#'  considered as missing. Examples might be a vector with "na", "N/A", \code{999}.
#' @param title A string that sets the plot title.
#' @param subtitle A string that sets the plot subtitle.
#' @param center_titles A logical which if \code{TRUE} centers both the 'title' and 'subtitle'.
#' @param x_title A string that sets the x axis title. The default is "Variables". If NULL then the x axis title does not appear.
#' @param y_title A string that sets the y axis title. The default is "Missing Counts". If NULL then the y axis title does not appear.
#' @param rot_x_tic_angle A numeric that sets the angle of rotation for the x tic label. When x tic labels are long,
#' @param bar_fill A string that sets the fill color for the bars.
#' @param bar_color A string that sets the outline color for the bars.
#' @param bar_alpha A numeric that sets the alpha component to 'bar_color'.
#' @param bar_lwd A numeric that sets the outline thickness of the bars.
#' @param bar_width A numeric that sets the width of the bars.
#' @param y_limits A numeric 2 element vector that sets the minimum and maximum for the y axis.
#'  Use NA to refer to the existing minimum and maximum.
#' @param y_major_breaks A numeric vector or function that defines the exact major tic locations along the y axis.
#' @param do_coord_flip A logical which if \code{TRUE} will flip the x and y axis'.
#' @param order_bars A string which will order the bars in a specific direction. Acceptable values are "asc" or "desc"
#' @param bar_labels A logical which if \code{TRUE} will label each bar with its value.
#' @param bar_label_sz A numeric that sets the size of the bar label
#' @param bar_label_color A string that sets the color of the bar labels
#'
#' @return Returning a named list with:
#' \enumerate{
#'  \item "missing_plot" -- a ggplot2 plot object where additional aesthetics may be added.
#'  \item "replacement_df" -- a data.table copy of 'df' with missing values replaced
#'   if 'replace_fun' is not \code{NULL}.
#' }
#'
#' @examples
#' library(ggplot2)
#' library(data.table)
#' library(mlbench)
#' library(RplotterPkg)
#'
#' data("Soybean", package = "mlbench")
#' for(i in 2:ncol(Soybean)){
#'   Soybean[,i] <- as.numeric(Soybean[,i])
#' }
#'
#' columns_of_interest <- colnames(Soybean)[2:ncol(Soybean)]
#' Soybean$date[[3]] <- NA
#' Soybean$date[[4]] <- 99
#' Soybean$leaves[[4]] <- NA
#' Soybean$leaves[[5]] <- "N/A"
#' Soybean$leaves[[6]] <- "na"
#' Soybean$leaves[[7]] <- NA
#' Soybean$leaves[[8]] <- NaN
#'
#' missing_val_fun <- function(x){
#'   xx <- as.numeric(x)
#'   return((max(xx) - min(xx))/2)
#' }
#'
#' soybean_missing_lst <- RregressPkg::plot_replace_missing(
#'   df = Soybean,
#'   variables = columns_of_interest,
#'   replace_fun = missing_val_fun,
#'   miss_values = c("N/A", "na", 99),
#'   title = "Count of Missing Values",
#'   subtitle = "mlbench::Soybean data set",
#'   x_title = "Variable",
#'   y_title = "Count of Missing Values",
#'   bar_lwd = 0.6,
#'   bar_color = "white",
#'   bar_labels = TRUE,
#'   bar_label_sz = 3,
#'   do_coord_flip = TRUE,
#'   order_bars = "asc"
#' )
#'
#' @importFrom data.table as.data.table
#' @importFrom RplotterPkg create_bar_plot
#' @import ggplot2
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
  bar_lwd = 0.7,
  bar_width = NULL,
  y_limits = NULL,
  y_major_breaks = waiver(),
  do_coord_flip = FALSE,
  order_bars = NULL,
  bar_labels = FALSE,
  bar_label_sz = 6,
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
          median_val <- stats::median(not_missing_v)
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
    bar_lwd = bar_lwd,
    bar_width = bar_width,
    y_limits = y_limits,
    y_major_breaks = y_major_breaks,
    do_coord_flip = do_coord_flip,
    order_bars = order_bars,
    bar_labels = bar_labels,
    bar_label_sz = bar_label_sz,
    bar_label_color = bar_label_color
  )

  return(
    list(
      missing_plot = aplot,
      replacement_df = dt
    )
  )
}
