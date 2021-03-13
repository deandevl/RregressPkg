#' Function computes response values for a certain set of constant predictor values
#'
#' @description The function requires a data frame with columns for observed response and predictors,
#'  a formula object (class = dQuote{formula}), and a data frame of target predictor values. From the
#'  predictor values estimates of the response are calculated along with their corresponding
#'  "confidence" or "prediction" intervals.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula
#' @param predictor_vals_df A data frame of target predictor values to use for estimating the responses
#'  and associated confidence intervals.
#' @param interval A string that sets the type of confidence interval.  Acceptable values
#'  are dQuote{confidence} or dQuote{prediction}.
#' @param confid_level A numeric that defines the confidence level for estimating confidence
#'  intervals. The default is 0.95.
#'
#' @return Returning a data frame with the OLS response values and their corresponding
#'   upper and lower confidence intervals for both "fitted value" and "predicted value". Also
#'   the standard errors used in the CI estimation.
#'
#' @author Rick Dean
#'
#' @export
ols_predict_calc <- function(
  df = NULL,
  formula_obj = NULL,
  predictor_vals_df = NULL,
  interval = "confidence",
  confid_level = 0.95
){
  # construct a predict_mt matrix that follows the model formula
  # with a column of 0's for the intercept

  # create a response variable df with a dummy value of 0
  variables <- all.vars(terms(formula_obj, data = df))
  response_df <- data.frame(v = 0)
  colnames(response_df) <- variables[[1]]

  # create a df that binds response and predictor df's
  predictor_vals_df <- cbind(response_df, predictor_vals_df)

  # get the model.matrix using the formula and predictor_vals_df for data
  predict_mt <- stats::model.matrix(formula_obj, data = predictor_vals_df)

  # set "(Intercept)" column to 0
  predict_mt[,"(Intercept)"] <- 0

  X_mt <- stats::model.matrix(formula_obj, data = df)
  Y_mt <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))

  return_df <- data.frame()

  if(interval == "confidence"){
    for(i in 1:nrow(predict_mt)) {
      # subtract target predictor values a row at a time from sample predictor values
      X_diff_mt <- t(apply(X = X_mt, MARGIN = 1, FUN = function(x) x - predict_mt[i,]))

      ols <- RregressPkg::ols_calc(
       # df = xy_df,
        X_mt = X_diff_mt,
        Y_mt = Y_mt,
        formula_obj = formula_obj,
        confid_level = confid_level
      )
      # compute confidence intervals
      t_cv <- ols$t_critical_val
      se <- ols$coef_se_vals[["(Intercept)"]]
      ci_lower <- ols$coef_vals[["(Intercept)"]] - se * t_cv
      ci_upper <- ols$coef_vals[["(Intercept)"]] + se * t_cv

      confid_df <- data.frame(
        fit = ols$coef_vals[["(Intercept)"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_df <- rbind(return_df, confid_df)
    }
  }else if(interval == "prediction"){
    for(i in 1:nrow(predict_mt)) {
      # subtract target predictor values a row at a time from sample predictor values
      X_diff_mt <- t(apply(X = X_mt, MARGIN = 1, FUN = function(x) x - predict_mt[i,]))

      ols <- RregressPkg::ols_calc(
        X_mt = X_diff_mt,
        Y_mt = Y_mt,
        formula_obj = formula_obj,
        confid_level = confid_level
      )

      # compute prediction intervals
      t_cv <- ols$t_critical_val
      se <- sqrt(ols$coef_se_vals[["(Intercept)"]]^2 + ols$mse)
      ci_lower <- ols$coef_vals[["(Intercept)"]] -  se * t_cv
      ci_upper <- ols$coef_vals[["(Intercept)"]] +  se * t_cv

      confid_df <- data.frame(
        fit = ols$coef_vals[["(Intercept)"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_df <- rbind(return_df, confid_df)
    }
  }

  return(return_df)
}
