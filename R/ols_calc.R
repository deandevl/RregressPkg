#' Function computes linear OLS regression parameter estimates
#'
#' @description Function will calculate regression OLS parameters with input
#'  from a data frame containing columns for a response variable and predictor
#'  variables or a "lm" class object.
#'
#' @param x Either an object of class "lm" from an OLS estimate or a data frame containing
#'  values for the predictor variables and the response variable.
#' @param resp_col A string that names the column from \code{x} containing the response
#'  values if \code{x} is a data frame.
#'
#' @importFrom data.table setDT
#'
#' @return Returning a named list of data frames with OLS coefficient estimates, residual
#'  statistics, ANOVA of the regression along with residuals, fitted values, and R^2.
#'
#' @author Rick Dean
#'
#' @export
ols_calc <- function(
  x = NULL,
  resp_col = NULL
){

  if("lm" %in% class(x)){
    dt <- data.table::setDT(x$model)
  }else if("data.frame" %in% class(x)) {
    dt <- data.table::setDT(x)
  }else {
    stop('The "x" argument must be either an object of class "lm" or "data.frame" ')
  }

  # ---------------- regression with intercept----------------------
  X_dt <- dt[, !..resp_col]
  Y_dt <- dt[, ..resp_col]
  X <- as.matrix(X_dt)
  Y <- as.matrix(Y_dt)

  Interc <- 1
  X_intercept <- cbind(Interc,X)

  X_intercept_t <- t(X_intercept)
  X_intercept_inverse <- solve(X_intercept_t %*% X_intercept)

  Coef <- X_intercept_inverse %*% X_intercept_t %*% Y

  # compute the hat matrix
  Hat <- X_intercept %*% X_intercept_inverse %*% X_intercept_t

  # fitted
  Fitted_val <- (Hat %*% Y)

  # residuals
  Resid <- Y - Fitted_val

  # --------------sums of squares/mean sum of squares----------
  n <- nrow(Y)
  k <- ncol(X)
  p <- k + 1

  # corrected degrees of freedom for model
  dfm <- p - 1

  # degrees of freedom for error
  dfe <- n - p

  # corrected degrees of freedom total
  dft <- n - 1
  # note: dfm + dfe = dft

  # corrected sum squares for model (SSM)
  ssm = sum((Fitted_val[,1] - mean(Y[,1]))^2)

  # sum of squares for error (SSE)
  sse <- sum((Y[,1] - Fitted_val[,1])^2)

  # corrected sum of squares total (SST)
  sst <- sum((Y - mean(Y[,]))^2)
  # note: ssm + sse = sst

  # mean of squares for model (MSM)
  msm = ssm / dfm

  # mean of squares for error (MSE)
  mse <- sse / dfe

  # residual standard error
  rse <- sqrt(mse)

  # mean of squares total (MST)
  mst <- sst / dft

  # ------------------significance tests------------------
  # predictors variance-covariance matrix
  var_cov <- mse * X_intercept_inverse

  # estimated standard error of coefficients
  coef_se <- sqrt(diag(var_cov))

  # t statistic for coefficients with dfe degrees of freedom
  t_value <- (Coef / coef_se)[,1]
  t_critical_95 <- stats::qt(0.975, dfe)
  t_critical_99 <- stats::qt(0.995, dfe)

  # p statistic for coefficients
  # cumulative density function of the t-distribution with dfe degrees of freedom
  # stats::pt = cumulative density function of the t-distribution
  p_value <- 2 * stats::pt(-abs(t_value), dfe)

  # F statistic for regression
  # refer to the F-table with df numerator = dfm and df denominator = dfe
  F_value <- msm / mse

  # F confidence intervals
  F_interval_95 <- c(0, stats::qf(0.95, dfm, dfe))
  F_interval_99 <- c(0, stats::qf(0.99, dfm, dfe))

  # coefficient of determination
  # proportion of variation in the y-variable that is due to variation in the x-variables
  #r_squared <- var(Fitted_val[,1])/var(Y[,1])
  r_squared <- ssm / sst

  # adjusted r_squared
  r_squared_adj <- 1 - (1 - r_squared)*(n - 1)/(n - p)

  # coefficients
  coef_df <- data.frame(
    Coef = c("Intercept", colnames(X_dt)),
    Value = Coef[,1],
    SE = coef_se,
    t_value = t_value,
    p_Value = p_value
  )

  # correlations between all variables
  # includes both the response and predictors
  means_dt <- dt[, lapply(dt, function(x) mean(x))]
  means_mt <- matrix(data = 1, nrow = n) %*% as.matrix(means_dt, rownames = F)
  dt_mt <- as.matrix(dt)
  diff_mt <- dt_mt - means_mt
  var_cor_mt <- t(diff_mt) %*% diff_mt * (n - 1)^-1
  var_mt <- diag(var_cor_mt)
  sd_mt <- sqrt(var_mt)
  sd_product_mt <- sd_mt %*% t(sd_mt)
  corr_df <- as.data.frame(var_cor_mt / sd_product_mt)

  # analysis of variance
  anova_df <- data.frame(
    Source = c("Regression", "Residual Error", "Total"),
    DF = c(dfm, dfe, dft),
    SS = c(ssm, sse, sst),
    MS = c(msm, mse, NA)
  )

  # residual stats
  quantiles <- stats::quantile(Resid[,1])
  resid_df <- data.frame(
    min = quantiles[1],
    Q1 = quantiles[2],
    median = quantiles[3],
    Q3 = quantiles[4],
    max = quantiles[5]
  )

  return(list(
    X = X,
    Y = Y,
    coef_df = coef_df,
    resid_df = resid_df,
    anova_df = anova_df,
    corr_df = corr_df,
    fitted_val = Fitted_val[,1],
    resid = Resid[,1],
    var_cov = var_cov,
    ssm = ssm,
    sse = sse,
    sst = sst,
    msm = msm,
    mse = mse,
    mst = mst,
    rse = rse,
    rsquared = r_squared,
    r_squared_adj = r_squared_adj,
    n = n,
    k = k,
    p = p,
    dfm = dfm,
    dfe = dfe,
    dft = dft,
    coef = Coef[,1],
    coef_se = coef_se,
    t_value = t_value,
    p_value = p_value,
    F_value = F_value,
    t_critical_95 = t_critical_95,
    t_critical_99 = t_critical_99,
    F_interval_95 = F_interval_95,
    F_interval_99 = F_interval_99,
    hat = Hat
  ))
}

