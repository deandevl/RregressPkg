#' Function computes the Breusch-Pagan Test for heteroskedasticity involving OLS models
#'
#' @description Performs the BP-test from submitted lm formula and data frame.
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
   X <- stats::model.matrix(formula_obj, data = df)
   Y <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))

   ols_lst <- RregressPkg::ols_matrix_calc(X = X, Y = Y)
   resid = ols_lst$Resid^2

   ols_resid_lst <- RregressPkg::ols_matrix_calc(X = X, Y = resid)

   r_sq <- ols_resid_lst$r_squared
   n <- ols_resid_lst$n
   k <- ols_resid_lst$k

   F_val <- (r_sq/k)/((1 - r_sq)/(n - k - 1))
   LM_val <- n * r_sq
   F_p_val <- 1 - stats::pf(q = F_val, df1 = k, df2 = n - k - 1)
   LM_p_val <- 1 - stats::pchisq(LM_val, k)

   return(data.frame(
     F_Val = F_val,
     F_p = F_p_val,
     LM_Val = LM_val,
     LM_p = LM_p_val
   ))
}
