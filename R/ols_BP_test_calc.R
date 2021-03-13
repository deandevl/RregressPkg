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
   ols_lst <- RregressPkg::ols_calc(df = df, formula_obj = formula_obj)

   X_mt <- stats::model.matrix(formula_obj, data = df)
   X_df <- as.data.frame(X_mt[,2:ncol(X_mt)])

   Y_df <- as.data.frame(ols_lst$residual_vals^2)

   names(Y_df) <- all.vars(formula_obj)[[1]]
   resid_df <- cbind(Y_df,X_df)

   ols_resid_lst <- RregressPkg::ols_calc(df = resid_df, formula_obj = formula_obj)

   r_sq <- ols_resid_lst$rsquared
   n <- nrow(resid_df)
   k <- ncol(resid_df)

   F_val <- (r_sq/(k-1))/((1 - r_sq)/(n - k - 1))
   LM_val <- n * r_sq
   F_p_val <- 1 - stats::pf(q = F_val, df1 = k-1, df2 = n - k - 1)
   LM_p_val <- 1 - stats::pchisq(LM_val, k-1)

   return(data.frame(
     F_Val = F_val,
     F_p = F_p_val,
     LM_Val = LM_val,
     LM_p = LM_p_val
   ))
}
