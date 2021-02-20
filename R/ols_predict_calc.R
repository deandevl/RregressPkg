#' Function computes response values for a certain set of constant predictor values
#'
#' @description The function requires a data frame with columns for observed response and predictors,
#'  a formula object (class = dQuote{formula}), and a data frame of predictor constants. From the
#'  predictor constants estimates of the response are calculated along with their corresponding
#'  "confidence" or "prediction" intervals.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula
#' @param predictors_df A data frame of predictor value constants to use for estimating the responses.
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
  predictors_df = NULL,
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
  predictors_df <- cbind(response_df, predictors_df)

  # get the model.matrix using the formula and predictors_df for data
  predict_mt <- stats::model.matrix(formula_obj, data = predictors_df)

  # set "(Intercept)" column to 0
  predict_mt[,"(Intercept)"] <- 0

  X <- stats::model.matrix(formula_obj, data = df)
  Y <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))

  return_df <- data.frame()

  if(interval == "confidence"){
    for(i in 1:nrow(predict_mt)) {
      # subtract constant predictor values from sample predictor values
      diff_mt <- t(apply(X = X, MARGIN = 1, FUN = function(x) x - predict_mt[i,]))
      # estimate OLS
      ols <- RregressPkg::ols_matrix_calc(
        X = diff_mt,
        Y = Y
      )

      # compute confidence intervals
      t_val <- stats::qt(1 - (1 - confid_level)/2.0, ols$n - ols$p)
      se <- ols$coef_se[["(Intercept)"]]
      ci_lower <- ols$Coef[["(Intercept)"]] - se * t_val
      ci_upper <- ols$Coef[["(Intercept)"]] + se * t_val

      confid_df <- data.frame(
        fit = ols$Coef[["(Intercept)"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_df <- rbind(return_df, confid_df)
    }
  }else if(interval == "prediction"){
    for(i in 1:nrow(predict_mt)) {
      # subtract constant predictor values from sample predictor values
      diff_mt <- t(apply(X = X, MARGIN = 1, FUN = function(x) x - predict_mt[i,]))

      # estimate OLS
      ols <- RregressPkg::ols_matrix_calc(
        X = diff_mt,
        Y = Y
      )

      # compute confidence intervals
      t_val <- stats::qt(1 - (1 - confid_level)/2.0, ols$n - ols$p)

      se <- sqrt(ols$coef_se[["(Intercept)"]]^2 + ols$mse)
      ci_lower <- ols$Coef[["(Intercept)"]] -  se * t_val
      ci_upper <- ols$Coef[["(Intercept)"]] +  se * t_val

      confid_df <- data.frame(
        fit = ols$Coef[["(Intercept)"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_df <- rbind(return_df, confid_df)
    }
  }

  return(return_df)
}
