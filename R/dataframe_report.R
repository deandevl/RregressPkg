#' @title dataframe_report
#'
#' @description
#' Function provides a simple report on a dataframe destined to for
#' regression modeling, plotting, or other data investigations.
#'
#' Function returns a list of dataframes showing rows of outliers, missing
#' data, and types of variables from the target dataframe.
#'
#' @param df The required target dataframe from which some basic information on observed data
#'   can be gathered.
#' @param miss_values An optional vector with numeric and character values that define in addition to \code{NA} and \code{NaN}, other values
#'   considered as missing. Examples might be a vector with "na", "N/A", \code{999}.
#'
#' @return Returning a named list with dataframes named:
#' \enumerate{
#'    \item "missing",
#'    \item "outliers"
#'    \item "datatypes"
#' }
#'
#' @examples
#' library(data.table)
#' library(RregressPkg)
#' set.seed(123)
#' dt <- data.table(
#'   id = 1:100,
#'   category = sample(c("A","B","C",NA), 100, replace = TRUE),
#'   value = c(rnorm(97), -10, 100, NA),
#'   date = c(seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = 99), NaN),
#'   text = sample(c("Lorem","Ipsum","Dolor","Sit",NA), 100, replace = TRUE)
#' )
#' report_lst <- RregressPkg::dataframe_report(dt)
#'
#' @import data.table
#' @importFrom stats na.omit
#'
#' @export
dataframe_report <- function(
  df,
  miss_values = NULL
){

  . <- row_no <- column <- missing_count <- data_type <- NULL

  if(is.null(df)){
    stop("A dataframe is a required parameter.")
  }

  dt <- data.table::as.data.table(df)

  if(is.null(miss_values)) {
    miss_values <- c(NA, NaN)
  }else{
    miss_values <- c(miss_values, NA, NaN)
  }

  # Identify number of missing values
  missing_dt <- data.table()
  for(a_col in colnames(dt)){
    missing_t_f <- data.table::fifelse(
      test = dt[, get(a_col)] %in% miss_values, yes = TRUE, no = FALSE
    )
    missing_dt[,(a_col) := sum(missing_t_f)]
  }
  missing_dt[, row_no := 1:nrow(missing_dt)]
  missing_long_dt <- data.table::melt(
    data = missing_dt,
    id.vars = "row_no",
    variable.name = "column",
    value.name = "missing_count"
  ) |>
    _[, .(column, missing_count)]

  # Identify datatypes
  datatypes_dt <- data.table()
  numeric_vars_v <- c()
  for(a_col in colnames(dt)){
    a_class <- class(dt[[a_col]])
    if(a_class == "numeric") numeric_vars_v <- c(numeric_vars_v, a_col)
    datatypes_dt[, (a_col) := a_class]
  }
  datatypes_dt[, row_no := 1:nrow(datatypes_dt)]
  datatypes_long_dt <- data.table::melt(
    data = datatypes_dt,
    id.vars = "row_no",
    variable.name = "column",
    value.name = "data_type"
  ) |>
    _[, .(column, data_type)]

  # Identify outliers
  dtt <- data.table::copy(dt)
  outliers_dt <- data.table()
  outliers_lst <- list()

  for(a_col in numeric_vars_v){
    upper <- stats::quantile(dt[[a_col]],0.75,na.rm = TRUE) + 1.5*stats::IQR(dt[[a_col]],na.rm = TRUE)
    lower <- stats::quantile(dt[[a_col]],0.25,na.rm = TRUE) - 1.5*stats::IQR(dt[[a_col]],na.rm = TRUE)

    a_dt <- dtt |>
      _[, `:=`(column = a_col, upper = upper, lower = lower)] |>
      na.omit(cols = (a_col)) |>
      _[get(a_col) > upper | get(a_col) < lower,]

    outliers_dt <- rbind(a_dt, outliers_dt)
  }

  return(
    list(
      datatypes = datatypes_long_dt,
      missing = missing_long_dt,
      outliers = outliers_dt
    )
  )
}
