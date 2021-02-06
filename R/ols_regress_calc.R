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
#' @importFrom RplotterPkg create_table
#'
#' @return Returning a named list of data frames with coefficient estimates, residual
#'  statistics, ANOVA of the regression, along with residuals, studentized residuals,
#'  fitted values, residual standard error, and R^2.
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

  ols <- ols_stats_fun(X, Y)

  # coefficients
  coef_df <- data.table(
    Coef = c("Intercept", col_names[!col_names %in% dep_str]),
    Value = ols$coef,
    SE = ols$coef_se,
    t_Value = ols$t_values,
    p_Value = ols$p_values
  )

  # analysis of variance
  n <- nrow(Y)
  k <- ncol(X)
  deg_freedom <- n - k - 1
  anova_df <- data.frame(
    Source = c("Regression", "Residual Error", "Total"),
    DF = c(k, deg_freedom, n-1),
    SS = c(ols$ssr, ols$sse, ols$ssr + ols$sse),
    MS = c(ols$ssr/k, ols$sse/deg_freedom, NA)
  )

  # residual stats
  quantiles <- stats::quantile(ols$resid)
  resid_df <- data.frame(
    min = quantiles[1],
    Q1 = quantiles[2],
    median = quantiles[3],
    Q3 = quantiles[4],
    max = quantiles[5]
  )

  # influence measures
  # compute studentized residuals or internally studentized residuals
  hat_ii <- diag(ols$hat)

  student_residuals <- ols$resid / sqrt(ols$mse * (1 - hat_ii))

  # compute the studentized deleted residuals or externally studentized residuals
  p <- k + 1
  numer <- n - p - 1
  denom <- n - p - student_residuals^2
  student_delete_residuals <- student_residuals * sqrt(numer/denom)

  # compute the Difference in Fits(DFFITS)
  dffits <- vector(mode="numeric", length = n)
  Inter_v <- c(Inter = 1)
  X_I <- cbind(Inter_v,X)

  for(i in 1:n){
    X_i <- X[-i,,drop=F]
    Y_i <- Y[-i,,drop=F]
    ols_i <- ols_stats_fun(X_i,Y_i)

    coef_mt <- matrix(ols_i$coef, ncol = 1)
    fitted_i_mt <- X_I %*% coef_mt

    dif_mt <- as.matrix(ols$fitted_val) - fitted_i_mt
    denom_mt <- as.matrix(sqrt(ols_i$mse * hat_ii), ncol = 1)
    dffits[[i]] <- (dif_mt/denom_mt)[i,]
  }

  # compute Cook's distance measure
  cook <- vector(mode="numeric", length = n)
  for(i in 1:n){
    residual_mt <- ((Y - ols$fitted_val)^2)/(p * ols$mse)
    leverage_mt <- as.matrix(hat_ii/(1 - hat_ii)^2)
    cook[[i]] <- (residual_mt * leverage_mt)[i,]
  }

  influence_df <- data.frame(
    Observation = 1:n,
    Student_Res = student_residuals,
    Student_Del_Res = student_delete_residuals,
    Dffits = dffits,
    Cook = cook
  )

  return(list(
    coef_df = coef_df,
    resid_df = resid_df,
    anova_df = anova_df,
    influence_df = influence_df,
    resid = ols$resid[,1],
    mean_sq_error = ols$mse,
    resid_sd = ols$resid_sd,
    fitted_val = ols$fitted_val[,1],
    r_squared = ols$r_squared,
    hat = ols$hat
  ))
}

ols_stats_fun <- function(x, y){
  n <- nrow(y)
  k <- ncol(x)
  deg_freedom <- n - k - 1

  Inter_v <- c(Inter = 1)
  X <- cbind(Inter_v,x)
  X_t <- t(X)
  X2_inverse <- solve(X_t %*% X)

  # compute coefficients
  coef <- (X2_inverse %*% X_t %*% y)[,1]

  # compute the hat matrix
  hat <- X %*% X2_inverse %*% X_t

  # fitted
  fitted_val <- (hat %*% y)

  # residuals
  resid <- y - fitted_val

  # estimated variance of resid
  resid_var <- (t(resid) %*% resid / deg_freedom)[,1]

  # estimated standard deviation of resid
  resid_sd <- sqrt(resid_var)

  # estimated variance of coefficients
  coef_var <- resid_var * X2_inverse

  # estimated standard error of coefficients
  coef_se <- sqrt(diag(coef_var))

  # t values for coefficients
  t_values <- coef / coef_se

  # p values for coefficients
  p_values <- 2 * pt(-abs(t_values), deg_freedom)

  # estimate coefficient of determination
  r_squared <- var(fitted_val[,1])/var(y[,1])

  # error sum squares
  sse <- sum(resid[,1]^2)

  # mean squared error
  mse <- sse/deg_freedom

  # regression sum squares
  ssr = sum((fitted_val[,1] - mean(y[,1]))^2)

  return(
    list(
      coef = coef,
      hat = hat,
      fitted_val = fitted_val,
      resid = resid,
      resid_var = resid_var,
      resid_sd = resid_sd,
      coef_var = coef_var,
      coef_se = coef_se,
      t_values = t_values,
      p_values = p_values,
      r_squared = r_squared,
      sse = sse,
      mse = mse,
      ssr = ssr
    )
  )
}

