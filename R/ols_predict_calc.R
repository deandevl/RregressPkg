#' Function computes response values for a certain set of predictor values
#'
#' @description Function calculates response values from an estimated OLS regression
#'  of predictor variables.  Given values for the predictors and OLS model, the function returns
#'  the corresponding predicted response values, their standard errors, and either
#'  "confidence" or "prediction" intervals.
#'
#' @param x Either an object of class "lm" from an OLS estimate or a data frame containing
#'  values for the predictor variables and the response variable.
#' @param resp_col A string that names the column from \code{x} containing the response
#'  values if \code{x} is a data frame.
#' @param predictors_df A data frame of predictor values to use for estimating the responses.
#' @param interval A string that sets the type of confidence interval.  Acceptable values
#'  are dQuote{confidence} or dQuote{prediction}.
#' @param confid_level A numeric that defines the confidence level for estimating confidence
#'  intervals. The default is 0.95.
#'
#' @importFrom data.table data.table
#' @importFrom data.table setDT
#'
#' @return Returning a named list with the OLS response values and their corresponding
#'   upper and lower confidence intervals for both "fitted value" and "predicted value". Also
#'   the standard errors used in the CI estimation.
#'
#' @author Rick Dean
#'
#' @export
ols_predict_calc <- function(
  x = NULL,
  resp_col = NULL,
  predictors_df = NULL,
  interval = "confidence",
  confid_level = 0.95
){
  if("lm" %in% class(x)){
    dt <- data.table::setDT(x$model)
  }else if("data.frame" %in% class(x)) {
    dt <- data.table::setDT(x)
  }
  X_dt <- dt[, !..resp_col]
  Y_dt <- dt[, ..resp_col]

  predictor_names <- colnames(predictors_df)
  data.table::setcolorder(X_dt, predictor_names)

  predictor_vals_mt <- as.matrix(predictors_df)

  return_dt <- data.table()

  if(interval == "confidence"){
    for(i in 1:nrow(predictor_vals_mt)) {
      # subtract constant predictor values from sample predictor values
      diff_mt <- t(apply(X = X_dt, MARGIN = 1, FUN = function(x) x - predictor_vals_mt[i,]))

      # estimate OLS
      ols <- RregressPkg::ols_matrix_calc(
        X = diff_mt,
        Y = Y_dt
      )

      # compute confidence intervals
      t_val <- stats::qt(1 - (1 - confid_level)/2.0, ols$n - ols$p)

      se <- ols$coef_se[["Interc"]]
      ci_lower <- ols$Coef[["Interc"]] - se * t_val
      ci_upper <- ols$Coef[["Interc"]] + se * t_val

      confid_dt <- data.table(
        fit = ols$Coef[["Interc"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_dt <- rbind(return_dt, confid_dt)
    }
  }else if(interval == "prediction"){
    for(i in 1:nrow(predictor_vals_mt)) {
      # subtract constant predictor values from sample predictor values
      diff_mt <- t(apply(X = X_dt, MARGIN = 1, FUN = function(x) x - predictor_vals_mt[i,]))

      # estimate OLS
      ols <- RregressPkg::ols_matrix_calc(
        X = diff_mt,
        Y = Y_dt
      )

      # compute confidence intervals
      t_val <- stats::qt(1 - (1 - confid_level)/2.0, ols$n - ols$p)

      se <- sqrt(ols$coef_se[["Interc"]]^2 + ols$mse)
      ci_lower <- ols$Coef[["Interc"]] -  se * t_val
      ci_upper <- ols$Coef[["Interc"]] +  se * t_val

      confid_dt <- data.table(
        fit = ols$Coef[["Interc"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_dt <- rbind(return_dt, confid_dt)
    }
  }

  return(return_dt)
}
