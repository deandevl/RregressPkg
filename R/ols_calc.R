#' @title ols_calc
#'
#' @description
#' Function computes linear OLS regression parameter estimates
#'
#'  The function requires a data frame with columns for observed response and predictors,
#'  and a formula object (class = "formula") that describes the OLS model of interest.
#'  Alternatively, the function will accept a matrix 'X_mt' and one column matrix 'Y_mt' for the
#'  predictor and response values respectively.
#'
#' @param df A data frame with columns for observed response and predictors.
#' @param formula_obj A formula object following the rules of \code{stats::lm()} construction.
#'  (e.g \code{y ~ log(a) + b + I(b^2))}. A constant parameter is implicitly added.
#' @param X_mt In lieu of both 'df' and 'formula_obj', a matrix of predictor values can be
#'  submitted along with 'Y_mt'. Note that 'X_mt' must have column names. Also, to include
#'  an intercept, 'X_mt' should have a column of 1's.
#' @param Y_mt In lieu of both 'df' and 'formula_obj', a single column matrix of response
#'  values can be submitted along with 'X_mt'.
#' @param confid_level A numeric that defines the confidence level for estimating confidence
#'  intervals. The default is 0.95.
#' @param na_omit A logical which if \code{TRUE} will omit rows that have \code{NA} values.
#' @param print_detail A logical which if \code{TRUE} will print a few statistics on the model.
#'   The default is \code{FALSE}.
#'
#' @return Returning a named list of dataframes with OLS coefficient estimates, residual
#'  statistics, ANOVA of the regression along with residuals, fitted values, and R^2.
#'
#' @examples
#' library(wooldridge)
#' library(data.table)
#' library(RregressPkg)
#'
#' gpa1_dt <- data.table::as.data.table(wooldridge::gpa1) |>
#' _[, skipped := -skipped] |>
#'   _[, .(colGPA, hsGPA, ACT, skipped)]
#'
#' ols_ls <- RregressPkg::ols_calc(
#'   df = gpa1_dt,
#'   formula_obj = colGPA ~ hsGPA + ACT + skipped
#' )
#'
#' @export
ols_calc <- function(
  df = NULL,
  X_mt = NULL,
  Y_mt = NULL,
  formula_obj = NULL,
  confid_level = 0.95,
  na_omit = FALSE,
  print_detail = FALSE
){
  if(!is.null(df)){
    if(na_omit){
      df <- stats::na.omit(df)
    }
    X_mt <- stats::model.matrix(formula_obj, data = df)
    Y_mt <- as.matrix(stats::model.frame(formula_obj, data = df)[[1]])
  }else {
    if(is.null(colnames(X_mt))){
      stop("X_mt must have column names")
    }
  }
  n <- nrow(Y_mt)
  k <- ncol(X_mt)
  p <- n - k - 1

  # regression degrees of freedom
  df_regress <- k - 1
  # error degrees of freedom
  df_error <- n - k
  # total degrees of freedom
  df_total <- n - 1
  # df_total = df_regress + df_error

  # decompose the predictor matrix X_mt into Q_mt and R_mt
  qr_lst <- base::qr(X_mt)

  coef_v <- base::qr.coef(qr_lst, Y_mt)[,1]
  fitted_v <- base::qr.fitted(qr_lst, Y_mt)[,1]
  residual_mt <- base::qr.resid(qr_lst, Y_mt)
  residual_v <- residual_mt[,1]

  R_mt <- base::qr.R(qr_lst)
  R_inv_mt <- solve(R_mt)     # k x k elements

  Qf_mt <- base::qr.Q(qr_lst)

  tQY_mt <- base::qr.qty(qr_lst, Y_mt)
  f_v <- tQY_mt[1:k,]           # k
  r_v <- tQY_mt[(k+1):n,]       # n - k

  # sum of squares
  # total sum of squares (ssto)
  ssto <- sum((Y_mt - mean(Y_mt[,1]))^2)
  # regression sum of squares (ssr)
  ssr <- sum((fitted_v - mean(Y_mt[,1]))^2)
  # error sum of squares (sse)
  sse <- sum((Y_mt[,1] - fitted_v)^2)
  # ssto = ssr + sse

  # mean of squares
  # mean square regression (msr)
  msr <- ssr / df_regress
  # mean square error (mse)
  mse <- sse / df_error
  # mean of squares total (mst)
  mst <- ssto / df_total # Y data variance (sigma squared)

  # regression standard error or
  # residual standard error or
  # standard error of the regression or
  # standard error of the estimate or
  # root mean squared error
  ser <- sqrt(mse)

  # Coefficient of Determination, R-squared, Adjusted R-squared
  # sum of squares explained / sum of squares total
  # R-squared always increases(or stays the same) while ssto remains constant.
  # As more predictors are added to a multiple linear regression model, even
  # if the predictors added are unrelated to the response variable, R-squared
  # will increase.  By itself R-squared cannot be used to help us identify which
  # predictors should be included in a model and which should be excluded.
  r_squared <- ssr / ssto
  #r_squared_2 <- 1 - sse / sst
  #r_squared_3 <- var(Fitted_val[,1])/var(Y[,1])

  # adjusted r_squared
  # Does not necessarily increase as more predictors are added, and can be used
  # to help identify which predictors should be included in a model and which
  # should be excluded. When comparing two models used to predict the same response
  # variable, we generally prefer the model with the higher value of adjusted R-squared.
  r_squared_adj <- 1 - (1 - r_squared)*(df_total/df_error)

  # compute variance-covariance matrix of coefficients
  var_cov_coef_mt <- (R_inv_mt %*% t(R_inv_mt)) * mse # p x p elements

  # compute the standard errors of coefficients
  coef_var_v <- diag(var_cov_coef_mt)
  coef_se_v <- sqrt(coef_var_v) # p x 1 elements

  # compute the t values for the coefficients along with their critical values
  t_value_v <- coef_v / coef_se_v
  t_critical_val <- stats::qt(1 - (1 - confid_level)/2.0, p)

  # compute the p values for the coefficients
  # cumulative density function of the t-distribution with df degrees of freedom
  # stats::pt = cumulative density function of the t-distribution
  p_value_v <- 2 * stats::pt(-abs(t_value_v), df_error)

  # F value
  if(k > 1){
    F_val <- (r_squared/(1 - r_squared)) * ((n - k)/(k-1))

    # compute the F value with the ratio of models with/without intercept
    # partition "f_v" and compute increase in residual sum of squares that results from dropping intercept
    # f_0_v <- f_v[1]
    # f_1_v <- f_v[2:k]
    # sse_minus_coef_1 <- (t(f_1_v) %*% f_1_v)[1,1]
    #
    # # compute F value as the ratio of the sum of squares residuals between reduced and full models
    # q <- k - 1
    # F_val <- (sse_minus_coef_1/q) / mse
    #
    # # compute the F value critical value given the degrees of freedom for the two models
     F_cv <- stats::qf(confid_level, k-1, n-k-1)
    #
    # # compute the p value for the F distribution
     F_p <- 1.0 - stats::pf(F_val, k-1, n-k)
  }

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
    Source = c("Regression", "Error", "Total"),
    DF = c(df_regress, df_error, df_total),
    SS = c(ssr, sse, ssto),
    MS = c(msr, mse, mst)
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
  if(k > 1){
    F_df <- data.frame(
      F_value = F_val,
      F_cv = F_cv,
      F_p = F_p,
      df_numerator = k-1,
      df_denominator = n-k-1
    )
  }else{
    F_df <- NULL
  }
  # display to R console
  if(print_detail){
    print(formula_obj)
    cat("coef: \n")
    print(format(round(coef_v, digits = 4), nsmall=4), quote = F)
    cat("\ncoef se: \n")
    print(format(round(coef_se_v, digits = 4), nsmall=4), quote = F)
    cat("---\n")
    cat(paste0("n = ", n, ", k = ", k,
      "\nresidual se = ", round(ser,digits = 4),
      "\nR-squared = ", round(r_squared,digits = 4),
      "\nR-squaredAdj = ", round(r_squared_adj, digits = 4),
      "\n"
    ))
  }
  # For debug
  #lm_obj <- lm(formula_obj,data = df)
  #

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
    t_critical_val = t_critical_val,
    hat_mt = hat_mt,
    ssr = ssr, # regression sum of squares
    sse = sse, # error sum of squares
    ssto = ssto, # total sum of squares
    msr = msr, # mean square regression
    mse = mse, # mean square error
    ser = ser, # standard error of the regression
    n = n,
    k = k
  ))
}
