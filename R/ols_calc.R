#' Function computes linear OLS regression parameter estimates
#'
#' @description The function requires a data frame with columns for observed response and predictors,
#'  and a formula object (class = dQuote{formula}) that describes the OLS model of interest.
#'
#' @param df A data frame with columns for observed response and predictors
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  For example: y ~ log(a) + b + I(b^2) or suppress the constant with \code{0} in the formula.
#' @param na_omit A logical which if TRUE will omit rows that have NA values. If the
#'  data contains NA's then the correlation matrix will not be computed.
#'
#' @return Returning a named list of data frames with OLS coefficient estimates, residual
#'  statistics, ANOVA of the regression along with residuals, fitted values, and R^2.
#'
#' @author Rick Dean
#'
#' @export
ols_calc <- function(
  df = NULL,
  formula_obj = NULL,
  na_omit = FALSE
){
  if(na_omit){
    df <- na.omit(df)
  }

  X <- stats::model.matrix(formula_obj, data = df)
  Y <- as.matrix(subset(stats::model.frame(formula_obj, data = df),select = 1))

  #variables <- all.vars(terms(formula_obj, data = df))

  X_t <- t(X)
  X_inverse <- solve(X_t %*% X)

  Coef <- X_inverse %*% X_t %*% Y

  # compute the hat matrix
  Hat <- X %*% X_inverse %*% X_t

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
  sst <- sum((Y - mean(Y[,1]))^2)
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
  var_cov <- mse * X_inverse

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
    Coef = rownames(Coef),
    Value = Coef[,1],
    SE = coef_se,
    t_value = t_value,
    p_Value = p_value
  )

  # correlations between all variables
  # includes both the response and predictors
  #compute the variable means
  corr_df <- NULL
  if(!anyNA(df)){
    df <- apply(X = df, MARGIN = 2, FUN = function(x){
      as.numeric(x)
    })

    means_v <- apply(X = df, MARGIN = 2, FUN = function(x){
      mean(x)
    })
    # create a means for each column of variables
    M <- matrix(means_v, nrow = 1)
    means_mt <- matrix(data = 1, nrow = n) %*% M

    # subtract the means from data
    df_mt <- as.matrix(df)
    diff_mt <- df_mt - means_mt

    # create covariance matrix
    var_cor_mt <- t(diff_mt) %*% diff_mt * (n - 1)^-1

    # compute variables sd
    var_mt <- diag(var_cor_mt)
    sd_mt <- sqrt(var_mt)
    sd_product_mt <- sd_mt %*% t(sd_mt)

    # divide by var-cor matrix by sd
    corr_df <- as.data.frame(var_cor_mt / sd_product_mt)
    variable_names_df <- data.frame(Variable = colnames(corr_df))
    corr_df <- cbind(variable_names_df, corr_df)

    #check with R's cor()
    #corr_check_df <- stats::cor(df)
  }
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

