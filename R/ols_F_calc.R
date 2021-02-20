#' Function computes the linear F test based on the relative difference between
#'  the sum of squared residuals in the general (unrestricted) model and the
#'  restricted model.
#'
#' @description
#'
#' @param df A data frame with columns for observed response and predictors
#' @param ur_formula_obj The unrestricted formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param r_formula_obj The restricted formula object following the rules of \code{stats::lm()} construction.
#' @param f_prob A numeric that sets the F probability for computing the
#'  critical value.
#'
#' @return Returns a data frame with the computed F value and associated p value along
#' with sums of the squares for each model.
#'
#' @author Rick Dean
#'
#' @export
ols_F_calc <- function(
  df = NULL,
  ur_formula_obj = NULL,
  r_formula_obj = NULL,
  f_prob = .99
){
  X_ur <- stats::model.matrix(ur_formula_obj, data = df)
  Y_ur <- subset(stats::model.frame(ur_formula_obj, data = df),select = 1)

  X_r <- stats::model.matrix(r_formula_obj, data = df)
  Y_r <- subset(stats::model.frame(r_formula_obj, data = df),select = 1)

  ols_ur <- RregressPkg::ols_matrix_calc(X = X_ur, Y = Y_ur)
  ols_r <- RregressPkg::ols_matrix_calc(X = X_r, Y = Y_r)

  SSR_ur <-  ols_ur$sse
  SSR_r <- ols_r$sse

  n <- nrow(Y_ur)
  k_ur <- ncol(X_ur)-1
  k_r <- ncol(X_r)-1
  q <- k_ur - k_r
  c <- (n - k_ur - 1)/q
  df_1 <- q
  df_2 <- n - k_ur - 1

  r_sq_ur <- ols_ur$r_squared
  r_sq_r <- ols_r$r_squared

  F_val <- ((r_sq_ur - r_sq_r)/(1 - r_sq_ur)) * c
  critical_val <- stats::qf(p = f_prob, df1 = df_1, df2 = df_2)
  p_val <- 1 - stats::pf(q = F_val, df1 = df_1, df2 = df_2)

  return(data.frame(
    F_val = round(F_val,digits = 4),
    crit =  round(critical_val,digits = 4),
    p_val = round(p_val,digits = 6),
    prob = round(f_prob,digits = 2),
    df_1 = df_1,
    df_2 = df_2,
    R2_ur = round(r_sq_ur,digits = 3),
    R2_r = round(r_sq_r,digits = 3),
    SSR_ur = round(SSR_ur,digits = 2),
    SSR_r = round(SSR_r, digits = 2)
  ))
}
