#' Function computes the linear F test and the LM test for comparing restricted
#'  with unrestricted OLS models.
#'
#' @description Given formulas for the the unrestricted and restricted OLS models,
#'  function returns a list with a data.frame of F-value/LM-value statistics, along
#'  with OLS coefficient estimates for both models.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param ur_formula_obj The unrestricted formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param r_formula_obj The restricted formula object following the rules of \code{stats::lm()} construction.
#' @param confid_level A numeric that sets the confidence level for computing the
#'  critical values.
#'
#' @return Returns a named list with a data frame of computed F/LM statistics along
#'  with OLS coefficient estimates for each model.
#'
#' @author Rick Dean
#'
#' @export
ols_F_LM_calc <- function(
  df = NULL,
  ur_formula_obj = NULL,
  r_formula_obj = NULL,
  confid_level = .95
){
  X_ur <- stats::model.matrix(ur_formula_obj, data = df)
  Y_ur <- subset(stats::model.frame(ur_formula_obj, data = df),select = 1)

  X_r <- stats::model.matrix(r_formula_obj, data = df)
  Y_r <- subset(stats::model.frame(r_formula_obj, data = df),select = 1)

  ols_ur <- RregressPkg::ols_calc(df = df, formula_obj = ur_formula_obj)
  ols_r <- RregressPkg::ols_calc(df = df, formula_obj = r_formula_obj)

  SSR_ur <-  ols_ur$sse
  SSR_r <- ols_r$sse

  n <- nrow(Y_ur)
  k_ur <- ncol(X_ur)-1
  k_r <- ncol(X_r)-1
  q <- k_ur - k_r
  c <- (n - k_ur - 1)/q
  df_1 <- q
  df_2 <- n - k_ur - 1

  r_sq_ur <- ols_ur$rsquared
  r_sq_r <- ols_r$rsquared

  # compute the F value
  # using R-squared from both the restricted and unrestricted models
  F_val <- ((r_sq_ur - r_sq_r)/(1 - r_sq_ur)) * c
  F_crit_val <- stats::qf(p = confid_level, df1 = df_1, df2 = df_2)
  F_p_val <- 1.0 - stats::pf(q = F_val, df1 = df_1, df2 = df_2)

  # compute the LM value
  # regress the residuals from restricted model using the unrestriced model
  ols_resid <- RregressPkg::ols_calc(
    X_mt = X_ur,
    Y_mt = as.matrix(ols_r$residual_vals, ncol = 1)
  )

  # compute LM value
  LM_val <- ols_resid$rsquared * n
  LM_crit_val <- stats::qchisq(p = confid_level, df = q)
  LM_p_val <- 1.0 - stats::pchisq(q = LM_val, df = q)

  F_LM_df <- data.frame(
    Test = c("F", "LM"),
    Value = c(F_val, LM_val),
    Critical = c(F_crit_val, LM_crit_val),
    `P-value` = c(F_p_val, LM_p_val)
  )

  return(list(
    F_LM_df = F_LM_df,
    confid_level = confid_level,
    df_1 = df_1,
    df_2 = df_2,
    R2_ur = r_sq_ur,
    R2_r = r_sq_r,
    SSR_ur = SSR_ur,
    SSR_r = SSR_r,
    coef_ur_df = ols_ur$coef_df,
    coef_r_df = ols_r$coef_df
  ))
}
