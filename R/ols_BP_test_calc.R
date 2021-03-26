#' Function computes the Breusch-Pagan Test for heteroskedasticity involving OLS models
#'
#' @description Performs the BP-test from submitted lm formula and data frame. Function
#'  implements both the F and LM versions of the BP test.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#'
#' @return Returning a data frame of F and LM and their p-values
#'
#' @author Rick Dean
#'
#' @export
ols_BP_test_calc <- function(
  df = NULL,
  formula_obj = NULL
){
   # first OLS stage
   X_mt <- stats::model.matrix(formula_obj, data = df)
   Y_mt <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))

   n <- nrow(Y_mt)
   k <- ncol(X_mt) - 1
   p <- n - k - 1

   qr_lst <- base::qr(X_mt)
   coef_v <- base::qr.coef(qr_lst, Y_mt)[,1]
   residual_sq_mt <- base::qr.resid(qr_lst, Y_mt)^2

   fitted_v <- base::qr.fitted(qr_lst, residual_sq_mt)[,1]
   ssm = sum((fitted_v - mean(residual_sq_mt[,1]))^2)
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
