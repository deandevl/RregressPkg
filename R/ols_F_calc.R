#' Function computes the linear F test based on the relative difference between
#'  the sum of squared residuals in the general (unrestricted) model and the
#'  restricted model.
#'
#' @description Given formulas for the the unrestricted and restricted OLS models,
#'  function returns a data frame with the computed F-statistic along with OLS coefficient
#'  estimates for both models.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param ur_formula_obj The unrestricted formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param r_formula_obj The restricted formula object following the rules of \code{stats::lm()} construction.
#' @param confid_level A numeric that sets the confidence level for computing the
#'  critical value.
#'
#' @return Returns a named list with a data frame of computed F statistics along
#'  with OLS coefficient estimates for each model.
#'
#' @author Rick Dean
#'
#' @export
ols_F_calc <- function(
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

  F_val <- ((r_sq_ur - r_sq_r)/(1 - r_sq_ur)) * c
  critical_val <- stats::qf(p = confid_level, df1 = df_1, df2 = df_2)
  p_val <- 1.0 - stats::pf(q = F_val, df1 = df_1, df2 = df_2)

  F_df <- data.frame(
    F_val = round(F_val,digits = 4),
    crit =  round(critical_val,digits = 4),
    p_val = p_val,
    confid_level = confid_level,
    df_1 = df_1,
    df_2 = df_2,
    R2_ur = round(r_sq_ur,digits = 3),
    R2_r = round(r_sq_r,digits = 3),
    SSR_ur = round(SSR_ur,digits = 2),
    SSR_r = round(SSR_r, digits = 2)
  )

  return(list(
    F_df = F_df,
    coef_ur_df = ols_ur$coef_df,
    coef_r_df = ols_r$coef_df
  ))
}
