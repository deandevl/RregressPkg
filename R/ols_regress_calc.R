# Title     : ols_matrix_calc.R
# Objective : Calculate the OLS in matrix form
# Created by: Rick Dean
# Created on: 2021-01-09 10:48 AM

#' Function computes linear OLS regression parameter estimates
#'
#' @description Function will calculate regression OLS parameters with input
#'  from a data frame containing columns for the dependent and independent
#'  variables. R based matrix techniques are used in the calculations.
#'
#' @param data_df A data frame containing values for both the observed independent
#'  and dependent variables.
#' @param dep_str A string that names the column in \code{data_df} containing the dependent
#'  variable.
#'
#' @return Returning a named list of the linear OLS regression parameter estimates.
#'
#' @author Rick Dean
#'
#' @export
ols_regress_calc <- function(
  dep_str = NULL,
  data_df = NULL
){
  data_mt <- data.matrix(data_df)
  col_names <- colnames(data_df)
  dep_idx <- match(dep_str, col_names)

  X <- as.matrix(data_mt[,-dep_idx])
  Y <- as.matrix(data_mt[,dep_idx])

  n <- nrow(X)
  k <- ncol(X)
  deg_freedom <- n - k - 1

  Inter_v <- c(Inter = 1)
  X <- cbind(Inter_v,X)
  X_t <- t(X)
  X2_inverse <- solve(X_t %*% X)

  # compute coefficients
  coef <- (X2_inverse %*% X_t %*% Y)[,1]

  # compute the hat matrix
  Hat <- X %*% X2_inverse %*% X_t

  # fitted
  fitted_val <- (Hat %*% Y)
  #fitted2_val <- (X %*% coef)

  # residuals
  resid <- Y - fitted_val

  # estimated variance of resid
  resid_var <- (t(resid) %*% resid / deg_freedom)[,1]

  # standard error of residual standard error
  resid_se <- sqrt(resid_var)

  # estimated variance of coefficients
  coef_var <- resid_var * X2_inverse

  # estimated standard error of coefficients
  coef_se <- sqrt(diag(coef_var))

  # t values for coefficients
  t_values <- coef / coef_se

  # p values for coefficients
  p_values <- 2 * pt(-abs(t_values), deg_freedom)

  # estimate coefficient of determination
  r_squared <- var(fitted_val[,1])/var(Y[,1])

  # analysis of variance
  # regression sum squares
  Y_mean <- mean(Y[,1])
  ssr = sum((fitted_val[,1] - Y_mean)^2)
  # error sum squares
  sse <- sum(resid[,1]^2)
  anova_vals <- data.frame(
    Source = c("Regression", "Residual Error", "Total"),
    DF = c(k, n-k, n-1),
    SS = c(ssr, sse, ssr + sse),
    MS = c(ssr/k, sse/(n-k), NA)
  )

  # residual stats
  quantiles <- stats::quantile(resid)
  resid_stats <- data.frame(
    min = quantiles[1],
    Q1 = quantiles[2],
    median = quantiles[3],
    Q3 = quantiles[4],
    max = quantiles[5]
  )

  # compute standardized residuals
  Hat_ii <- diag(Hat)
  mse <- sse/(n-k)
  stand_residuals <- resid / sqrt(mse * (1 - Hat_ii))

  return(list(
    coef = coef,
    resid = resid[,1],
    resid_se = resid_se,
    coef_var = coef_var,
    coef_se = coef_se,
    fitted_val = fitted_val[,1],
    t_values = t_values,
    p_values = p_values,
    r_squared = r_squared,
    resid_stats = resid_stats,
    anova_vals = anova_vals,
    hat = Hat,
    stand_residuals = stand_residuals[,1]
  ))
}
