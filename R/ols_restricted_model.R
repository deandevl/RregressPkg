#' @title ols_restricted_model
#'
#' @description
#' Function computes the linear F test and the LM test for comparing restricted
#'  with unrestricted OLS models.
#'
#' Given formulas for the the unrestricted and restricted OLS models,
#'  function returns a list with a dataframe of F-value/LM-value statistics, along
#'  with OLS coefficient estimates for both models.
#'
#' Reference "Introductory Econometrics, A Modern Approach, Sixth Edition", Chapter 4,
#'   Section 4-5 Testing Multiple Linear Restrictions: The F Test, page 127, by Jeffrey Wooldridge.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param ur_formula_obj The unrestricted formula object following the rules of \code{stats::lm()} construction.
#'  For example: \code{y ~ log(a) + b + I(b^2)}.
#' @param r_formula_obj The restricted formula object following the rules of \code{stats::lm()} construction.
#' @param confid_level A numeric decimal that sets the confidence level for computing the
#'  critical values. The default is .95.
#'
#' @return Returns a named list with a data frame of computed F/LM statistics along
#'  with OLS coefficient estimates for each model.
#'
#' @examples
#' library(wooldridge)
#' library(data.table)
#' library(RregressPkg)
#'
#' mlb1_dt <- data.table::as.data.table(wooldridge::mlb1) |>
#' _[, .(salary, years, gamesyr, bavg, hrunsyr, rbisyr)]
#' ur_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr)
#' r_formula_obj <- stats::as.formula(log(salary) ~ years + gamesyr)
#'
#' restricted_salary_lst <- RregressPkg::ols_restricted_model(
#'   df = mlb1_dt,
#'   ur_formula_obj = ur_formula_obj,
#'   r_formula_obj = r_formula_obj,
#'   confid_level = 0.99
#' )
#' restricted_salary_lst$F_df$F.Value
#' restricted_salary_lst$F_df$Critical
#'
#' @export
ols_restricted_model <- function(
  df = NULL,
  ur_formula_obj = NULL,
  r_formula_obj = NULL,
  confid_level = .95
){
  # return the independent variables values in unrestricted model
  X_ur_mt <- stats::model.matrix(ur_formula_obj, data = df)
  # return the dependent variable values in unrestricted model
  Y_ur_v <- stats::model.frame(ur_formula_obj, data = df)[[1]]
  # return the independent variables values in restricted model
  X_r_mt <- stats::model.matrix(r_formula_obj, data = df)
  # return the dependent variable values in restricted model
  Y_r_v <- stats::model.frame(r_formula_obj, data = df)[[1]]

  n <- length(Y_ur_v) # number of observations
  # number of independent variables in unrestricted model including intercept
  k_ur <- ncol(X_ur_mt)
  # number of independent variables in restricted model including intercept
  k_r <- ncol(X_r_mt)
  # degrees of freedom for unrestricted model
  df_ur <- n - k_ur
  # degrees of freedom for restricted model
  df_r <- n - k_r
  # number of restrictions imposed in moving from unrestricted to restricted
  q <- df_r - df_ur

  # compute the F statistic
  # see Wooldridge, Equation 4.41, of section 4-5c R-squared form of the F statistic, page 133
  # get the R-squared values of both unrestricted and restricted models by
  #   calculating the OLS coefficients for both the unrestricted and restricted models
  ols_ur <- RregressPkg::ols_calc(df = df, formula_obj = ur_formula_obj)
  ols_r <- RregressPkg::ols_calc(df = df, formula_obj = r_formula_obj)
  r_sq_ur <- ols_ur$rsquared
  r_sq_r <- ols_r$rsquared

  c <- df_ur/q # a constant
  F_val <- ((r_sq_ur - r_sq_r)/(1 - r_sq_ur)) * c

  # compute the F critical value
  F_crit_val <- stats::qf(p = confid_level, df1 = q, df2 = df_ur)
  # compute the p-value: probability of observing a value of F at least as large as F_val
  F_p_val <- 1.0 - stats::pf(q = F_val, df1 = q, df2 = df_ur)

  # compute the LM value
  # regress the residuals from restricted model using the unrestriced model
  ols_resid <- RregressPkg::ols_calc(
    X_mt = X_ur_mt,
    Y_mt = as.matrix(ols_r$residual_vals, ncol = 1)
  )

  # compute LM value
  LM_val <- ols_resid$rsquared * n
  LM_crit_val <- stats::qchisq(p = confid_level, df = q)
  LM_p_val <- 1.0 - stats::pchisq(q = LM_val, df = q)

  F_df <- data.frame(
    `F-Value` = F_val,
    Critical = F_crit_val,
    numer_df = q,
    denom_df = df_ur,
    confid_lv = paste0((1-confid_level)*100,"%"),
    `p-value` = F_p_val
  )

  LM_df <- data.frame(
    Value = LM_val,
    Critical = LM_crit_val,
    confid_lv = paste0((1-confid_level)*100,"%"),
    `p-value` = LM_p_val
  )

  return(list(
    F_df = F_df,
    LM_df = LM_df,
    confid_level = confid_level,
    numerator_df = q,
    denominator_df = df_ur,
    R2_ur = r_sq_ur,
    R2_r = r_sq_r,
    coef_ur_df = ols_ur$coef_df,
    coef_r_df = ols_r$coef_df
  ))
}
