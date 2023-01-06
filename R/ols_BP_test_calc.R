#' Function computes the Breusch-Pagan Test for heteroskedasticity involving OLS models
#'
#' Performs the BP-test from submitted lm formula and data frame. Function
#'  implements both the F and LM versions of the BP test.
#'
#' Background for creating this function was from "Using R for Introductory Econometrics" by
#'   Florian Heiss, page 147.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param confid_level A numeric that defines the confidence level for estimating confidence
#'  intervals. The default is 0.95.
#'
#' @return Returning a data frame with the standard F test of `lm` results and LM test and their p-values
#'
#' @author Rick Dean
#'
#' @export
ols_BP_test_calc <- function(
  df = NULL,
  formula_obj = NULL,
  confid_level = 0.95
){
  # First OLS stage
  # Perform the usual regression
  X_mt <- stats::model.matrix(formula_obj, data = df) # includes intercept
  Y_mt <- as.matrix(stats::model.frame(formula_obj, data = df)[[1]])

  n <- nrow(Y_mt) # number of observations
  k <- ncol(X_mt) - 1 # number of independent variables
  p <- n - k - 1

  # decompose the predictor matrix X_mt into Q_mt and R_mt
  qr_lst <- base::qr(X_mt)
  coef_v <- base::qr.coef(qr_lst, Y_mt)[,1]
  residual_sq_mt <- base::qr.resid(qr_lst, Y_mt)^2
  
  # Second OLS stage
  # Get the fitted values based on squared residuals of first OLS stage      
  fitted_v <- base::qr.fitted(qr_lst, residual_sq_mt)[,1]
  # corrected sum squares for model (SSM)
  ssm = sum((fitted_v - mean(residual_sq_mt[,1]))^2)
  # corrected sum of squares total (SST)
  sst <- sum((residual_sq_mt - mean(residual_sq_mt[,1]))^2)
  r_sq <- ssm / sst
  
  F_val <- (r_sq/(1 - r_sq)) * ((n - k - 1)/k)
  LM_val <- n * r_sq
  F_p_val <- 1 - stats::pf(q = F_val, df1 = k, df2 = n - k - 1)
  LM_p_val <- 1 - stats::pchisq(LM_val, k)
  
  return(
    data.frame(
       Statistic = c("F", "LM"),
       Value = c(F_val,LM_val),
       `p-Value` = c(F_p_val, LM_p_val)
    )
  )
}
