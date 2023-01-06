#' Function computes linear OLS regression parameter estimates
#'
#'  The function requires a data frame with columns for observed response and predictors,
#'  and a formula object (class = "formula") that describes the OLS model of interest.
#'  Alternatively, the function will accept a matrix \code{X_mt} and one column matrix \code{Y_mt} for the
#'  predictor and response values respectively.
#'
#' @param df A data frame with columns for observed response and predictors.
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param X_mt In lieu of both \code{df} and \code{formula_obj}, a matrix of predictor values can be
#'  submitted along with \code{Y_mt}.
#' @param Y_mt In lieu of both \code{df} and \code{formula_obj}, a single column matrix of response
#'  values can be submitted along with \code{X_mt}.
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
    Y_mt <- as.matrix(stats::model.frame(formula_obj, data = df)[[1]])
  }
  n <- nrow(Y_mt)
  k <- ncol(X_mt)
  p <- n - k - 1
  
  # corrected degrees of freedom for model
  dfm <- k - 1
  # degrees of freedom for error
  dfe <- n - k
  # corrected degrees of freedom total
  dft <- n - 1
  # note: dfm + dfe = dft

  # decompose the predictor matrix X_mt into Q_mt and R_mt
  qr_lst <- base::qr(X_mt)

  coef_v <- base::qr.coef(qr_lst, Y_mt)[,1]
  fitted_v <- base::qr.fitted(qr_lst, Y_mt)[,1]
  residual_mt <- base::qr.resid(qr_lst, Y_mt)
  residual_v <- residual_mt[,1]

  sse <- (t(residual_mt) %*% residual_mt)[1,1]

  R_mt <- base::qr.R(qr_lst)
  R_inv_mt <- solve(R_mt)     # k x k elements

  Qf_mt <- base::qr.Q(qr_lst)

  tQY_mt <- base::qr.qty(qr_lst, Y_mt)
  f_v <- tQY_mt[1:k,]           # k
  r_v <- tQY_mt[(k+1):n,]       # n - k

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
  var_cov_coef_mt <- (R_inv_mt %*% t(R_inv_mt)) * mse # p x p elements

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
