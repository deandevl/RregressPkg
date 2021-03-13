#' Function computes linear OLS regression parameter estimates
#'
#' @description The function requires a data frame with columns for observed response and predictors,
#'  and a formula object (class = dQuote{formula}) that describes the OLS model of interest.
#'  Alternatively, the function will accept a matrix \code{X} and one column matrix \code{Y} for the
#'  predictor and response values respectively.
#'
#' @param df A data frame with columns for observed response and predictors.
#' @param X_mt A matrix of predictor values as an alternative to \code{df}.
#' @param Y_mt A single column matrix of response values as an alternative to \code{df}.
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param confid_level A numeric that defines the confidence level for estimating confidence
#'  intervals. The default is 0.95.
#' @param na_omit A logical which if TRUE will omit rows that have NA values.
#'
#' @return Returning a named list of data frames with OLS coefficient estimates, residual
#'  statistics, ANOVA of the regression along with residuals, fitted values, and R^2.
#'
#' @author Rick Dean
#'
#' @export
ols_calc <- function(
  df = NULL,
  X_mt = NULL,
  Y_mt = NULL,
  formula_obj = NULL,
  confid_level = 0.95,
  na_omit = FALSE
){
  if(!is.null(df)){
    if(na_omit){
      df <- na.omit(df)
    }
    X_mt <- stats::model.matrix(formula_obj, data = df)
    Y_mt <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))
  }
  n <- nrow(Y_mt)
  k <- ncol(X_mt)
  p <- n - k  - 1

  # corrected degrees of freedom for model
  dfm <- k - 1
  # degrees of freedom for error
  dfe <- n - k

  # if("(Intercept)" %in% colnames(X_mt)){
  #   browser()
  #   dfe <- n - k - 1
  # }

  # corrected degrees of freedom total
  dft <- n - 1
  # note: dfm + dfe = dft

  # decompose the predictor matrix X_mt into Q_mt and R_mt
  qr_lst <- get_qr(X = X_mt)

  Q_mt <- qr_lst$Q # n x n

  Qf_mt <- qr_lst$Qf # n x k

  R_mt <- qr_lst$R # k x k

  # compute vectors "f" and "r"
  tQY_mt <- t(Q_mt) %*% Y_mt # n x 1

  f_v <- tQY_mt[1:k,]   # k elements
  r_v <- tQY_mt[(k + 1):n,] # n - k elements

  # estimate coefficients
  R_inv_mt <- solve(R_mt) # k x k elements
  coef_v <- (R_inv_mt %*% f_v)[,1] # k x 1 elements

  # compute fitted values vector
  fitted_v <- (Qf_mt %*% t(Qf_mt) %*% Y_mt)[,1]

  # compute residual values vector
  residual_v <- (Y_mt - fitted_v)[,1]

  # compute residual sum of sqares
  sse <- (t(r_v) %*% r_v)[1,1]
  # compute mean square error
  mse <- sse / dfe
  # residual standard error
  rse <- sqrt(mse)

  # corrected sum squares for model (SSM)
  ssm = sum((fitted_v - mean(Y_mt[,1]))^2)
  # mean of squares for model (MSM)
  msm = ssm / dfm

  # corrected sum of squares total (SST)
  sst <- sum((Y_mt - mean(Y_mt[,1]))^2)
  # note: ssm + sse = sst
  # mean of squares total (MST)
  mst <- sst / dft

  # compute variance-covariance matrix of coefficients
  var_cov_coef_mt <- R_inv_mt %*% t(R_inv_mt) * mse # p x p elements

  # compute the standard errors of coefficients
  coef_var_v <- diag(var_cov_coef_mt)
  coef_se_v <- sqrt(coef_var_v) # p x 1 elements

  # compute the t values for the coefficients along with their critical values
  t_value_v <- coef_v / coef_se_v
  t_critical_val <- stats::qt(1 - (1 - confid_level)/2.0, p)

  # compute the p values for the coefficients
  # cumulative density function of the t-distribution with dfe degrees of freedom
  # stats::pt = cumulative density function of the t-distribution
  p_value_v <- 2 * stats::pt(-abs(t_value_v), dfe)

  # compute the F value with the ratio of models with/without intercept
  # partition "f_v" and compute increase in residual sum of squares that results from dropping intercept
  f_0_v <- f_v[1]
  f_1_v <- f_v[2:k]
  sse_minus_coef_1 <- (t(f_1_v) %*% f_1_v)[1,1]

  # compute F value as the ratio of the sum of squares residuals between reduced and full models
  q <- k - 1
  F_val <- (sse_minus_coef_1/q) / mse

  # compute the F value critical value given the degrees of freedom for the two models
  F_cv <- qf(confid_level, q, dfe)

  # compute the p value for the F distribution
  F_p <- 1.0 - stats::pf(F_val, q, dfe)

  # coefficient of determination
  # proportion of variation in the y-variable that is due to variation in the x-variables
  #r_squared <- var(Fitted_val[,1])/var(Y[,1])
  r_squared <- ssm / sst

  # adjusted r_squared
  r_squared_adj <- 1 - (1 - r_squared)*(dft)/(dfe)

  # influence or "hat" matrix
  hat_mt <- Qf_mt %*% t(Qf_mt) # n x n

  # coefficients
  coef_df <- data.frame(
    Coef = colnames(X_mt),
    Value = coef_v,
    SE = coef_se_v,
    t_value = t_value_v,
    p_Value = p_value_v
  )

  # coefficient CI's
  coef_CI_df <- data.frame(
    Coef = colnames(X_mt),
    Lower = coef_v - coef_se_v * t_critical_val,
    Beta = coef_v,
    Upper = coef_v + coef_se_v * t_critical_val
  )

  # analysis of variance
  anova_df <- data.frame(
    Source = c("Regression", "Residual Error", "Total"),
    DF = c(dfm, dfe, dft),
    SS = c(ssm, sse, sst),
    MS = c(msm, mse, NA)
  )

  # residual stats
  quantiles <- stats::quantile(residual_v)
  resid_df <- data.frame(
    min = quantiles[1],
    Q1 = quantiles[2],
    median = quantiles[3],
    Q3 = quantiles[4],
    max = quantiles[5]
  )

  # F Value
  F_df <- data.frame(
    F_value = F_val,
    F_cv = F_cv,
    F_p = F_p,
    df_numerator = q,
    df_denominator = dfe
  )

  return(list(
    coef_df = coef_df,
    coef_CI_df = coef_CI_df,
    resid_df = resid_df,
    anova_df = anova_df,
    F_df = F_df,
    coef_vals = coef_v,
    coef_se_vals = coef_se_v,
    fitted_vals =  fitted_v,
    residual_vals = residual_v,
    var_cov = var_cov_coef_mt,
    rsquared = r_squared,
    r_squared_adj = r_squared_adj,
    residual_se = rse,
    t_critical_val = t_critical_val,
    hat_mt = hat_mt,
    ssm = ssm,
    sse = sse,
    msm = msm,
    mse = mse,
    rse = rse,
    n = n,
    k = k,
    p = p
  ))
}

# function for factorization of predictor matrix X via orthogonal QR decomposition
get_qr <- function(X){
  A <- X
  n <- nrow(A)
  k <- ncol(A)
  Q <- diag(n)

  for(i in 1:k){
    # extract the kth column of the matrix
    col <- A[i:n, i]
    # calculation of the norm of the column in order to create the vector r
    norm1 <- sqrt(drop(crossprod(col)))
    # calculate the reflection vector  a_r
    a_r <- col
    a_r[1] <- a_r[1] + sign(a_r[1]) * norm1
    # beta <- 2 / ||a_r||^2
    beta <- 2 / drop(crossprod(a_r))
    # update matrix Q (trailing matrix only) by Householder reflection
    Q[,i:n] <- Q[,i:n] - tcrossprod(Q[,i:n] %*% a_r, beta * a_r)
    # update matrix A (trailing matrix only) by Householder reflection
    A[i:n, i:k] <- A[i:n,i:k] - tcrossprod(beta * a_r, crossprod(A[i:n,i:k], a_r))
  }

  R <- A[1:k, ]
  R[lower.tri(R)] <- 0

  return(list(Q = Q, Qf = Q[,1:k], R = R))
}
