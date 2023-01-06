#' Function computes estimated interval upper/lower values for a certain set of constant predictor values
#'
#'  The function requires a data frame with columns for observed response and predictors,
#'  a formula object (class = "formula"), and a data frame of target predictor values. From the
#'  predictor values estimates of the response are calculated along with their corresponding
#'  "confidence" or "prediction" upper/lower intervals at a specific confidence level.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula
#' @param predictor_vals_df A data frame of target predictor values to use for estimating the responses
#'  and associated confidence intervals.
#' @param interval A string that sets the type of confidence interval.  Acceptable values
#'  are "confidence" or "prediction".
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
ols_intervals <- function(
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
  Y_mt <- as.matrix(stats::model.frame(formula_obj, data = df)[[1]])

  n <- nrow(X_mt)
  k <- ncol(X_mt)
  
  t_cv <- stats::qt(1 - (1 - confid_level)/2.0, n - k - 1)
  
  return_df <- data.frame()

  if(interval == "confidence"){
    for(i in 1:nrow(predict_mt)) {
      # subtract target predictor values a row at a time from sample predictor values
      X_diff_mt <- t(apply(X = X_mt, MARGIN = 1, FUN = function(x) x - predict_mt[i,]))

      # compute the OLS coefficients
      # decompose the predictor matrix X_diff_mt
      qr_lst <- base::qr(X_diff_mt)
      
      R_mt <- base::qr.R(qr_lst) # p x p
      R_inv_mt <- solve(R_mt)   # p x p
      
      tQY_mt <- base::qr.qty(qr_lst, Y_mt)
      f_v <- tQY_mt[1:k,]           # k
      r_v <- tQY_mt[(k+1):n,]       # n - k
      
      coef_vals <- (R_inv_mt %*% f_v)[,1]  # p x 1
      sigma_sq <- (t(r_v) %*% r_v)[1,1]/(n - k)
      var_cov_coef <- R_inv_mt %*% t(R_inv_mt) * sigma_sq # p x p
      se <- sqrt(diag(var_cov_coef))[["(Intercept)"]]

      # compute confidence intervals
      ci_lower <- coef_vals[["(Intercept)"]] - se * t_cv
      ci_upper <- coef_vals[["(Intercept)"]] + se * t_cv

      confid_df <- data.frame(
        fit = coef_vals[["(Intercept)"]],
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

      # compute the OLS coefficients
      qr_lst <- base::qr(X_diff_mt)
      R_mt <- base::qr.R(qr_lst) # p x p
      
      tQY_mt <- base::qr.qty(qr_lst, Y_mt)
      f_v <- tQY_mt[1:k,]           # k
      r_v <- tQY_mt[(k+1):n,]       # n - k
      R_inv_mt <- solve(R_mt)               # p x p
      coef_vals <- (R_inv_mt %*% f_v)[,1]  # p x 1
      sigma_sq <- (t(r_v) %*% r_v)[1,1]/(n - k)
      var_cov_coef <- R_inv_mt %*% t(R_inv_mt) * sigma_sq # p x p
      resid_mt <- base::qr.resid(qr_lst, Y_mt)
      sse <- (t(resid_mt) %*% resid_mt)[1,1]
      mse <- sse/(n - 2)  # see page 49, eq 2.61, "Introd Econometrics"
      se <- sqrt(diag(var_cov_coef)[["(Intercept)"]]^2 + mse)
      # compute prediction intervals
      ci_lower <- coef_vals[["(Intercept)"]] - se * t_cv
      ci_upper <- coef_vals[["(Intercept)"]] + se * t_cv

      confid_df <- data.frame(
        fit = coef_vals[["(Intercept)"]],
        lwr = ci_lower,
        upr = ci_upper,
        se = se
      )

      return_df <- rbind(return_df, confid_df)
    }
  }

  return(return_df)
}
