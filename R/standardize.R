#' @title standardize
#'
#' @description
#' Function computes the z-scores for numeric columns of a dataframe.
#'
#' Scores are computed by dividing the raw differences from the
#' the mean by a multiple of the standard deviation.
#'
#' @param df A data frame with numeric columns to be standardized
#' @param cols A character vector of column names from \code{df} that are to be standardized.
#' @param mul An integer (typically 1 or 2) that multiplies the standard deviation
#'   as the divisor for the z-score.
#' @param do_na A logical which if \code{TRUE} removes \code{NA} values in computing
#'   the mean and standard deviation.
#'
#' @return Returns a data.table with the numeric columns standardized into z-scores.
#'
#' @importFrom data.table as.data.table
#'
#' @export
standardize <- function(
  df = NULL,
  cols = NULL,
  mul = 1,
  do_na = TRUE
) {
  get_scores <- function(x){
    if(is.numeric(x)){
      x_mean <- mean(x, na = do_na)
      x_sd <- sd(x, na.rm = do_na) * mul
      return((x - x_mean)/x_sd)
    }else {
      return(x)
    }
  }
  dt <- data.table::copy(df)
  dt_stand <- dt[, ..cols]
  return_dt <- dt[, (cols) := lapply(dt_stand, get_scores)]
  return(return_dt)
}
