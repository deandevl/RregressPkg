#' Function provides a simple report on a dataframe destined to for 
#' regression modeling, plotting, or other data investigations.
#' 
#' Function returns a named list of dataframes showing rows of outliers, missing
#' data, and types of variables from the target dataframe.
#' 
#' @param df The target dataframe from which some basic information on observed data
#'   can be gathered.
#' @param miss_values A vector with numeric and character values that define in addition to \code{NA} and \code{NaN}, other values
#'   considered as missing. Examples might be a vector with "na", "N/A", \code{999}.
#'   
#' @importFrom data.table as.data.table  
#' 
#' @return Returning a list with dataframes named:
#' \enumerate{
#'    \item "missing",
#'    \item "outliers"
#'    \item "datatypes"
#' }
#'  
#' @author Rick Dean
#'
#' @export
dataframe_report <- function(
  df,
  miss_values = NULL
){
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
  ) %>%
    .[, .(column, missing_count)]
  
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
  ) %>%
    .[, .(column, data_type)]
  
  # Identify outliers
  dtt <- data.table::copy(dt)
  outliers_dt <- data.table()
  outliers_lst <- list()
  
  for(a_col in numeric_vars_v){
    upper <- quantile(dt[[a_col]],0.75,na.rm = T) + 1.5*IQR(dt[[a_col]],na.rm = T)
    lower <- quantile(dt[[a_col]],0.25,na.rm = T) - 1.5*IQR(dt[[a_col]],na.rm = T)
   
    a_dt <- dtt %>%
      .[, `:=`(column = a_col, upper = upper, lower = lower)] %>%
      na.omit(cols = (a_col)) %>%
      .[get(a_col) > upper | get(a_col) < lower,]
    
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