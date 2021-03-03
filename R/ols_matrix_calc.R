#' Function computes the OLS coefficients via matrix operations
#'
#' @description Function provides an alternative to *stats::lm()* by using
#'  matrix based arguments for predictors and response in estimating the OLS
#'  parameters.
#'
#' @param X A numeric matrix or data frame with rows of observations and their respective predictor
#'  values across the columns. If you want a constant/intercept in the model then you must add a
#'  column of one's to this matrix.
#' @param Y A numeric matrix or data frame consisting of one column of the observations response
#'  values.
#'
#' @return A named list with various OLS regression statistics
#'
#' @author Rick Dean
#'
#' @export
ols_matrix_calc <- function(X, Y) {

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  n <- nrow(Y)
  k <- ncol(X)-1
  p <- k + 1

  X_t <- t(X)
  X_inverse <- solve(X_t %*% X)

  Coef <- X_inverse %*% X_t %*% Y

  # compute the hat matrix
  Hat <- X %*% X_inverse %*% X_t

  # fitted
  Fitted_val <- (Hat %*% Y)

  # residuals
  Resid <- Y - Fitted_val

  # corrected sum squares for model (SSM)
  ssm = sum((Fitted_val[,1] - mean(Y[,1]))^2)

  # sum of squares for error (SSE)
  sse <- sum((Y[,1] - Fitted_val[,1])^2)

  # corrected sum of squares total (SST)
  sst <- sum((Y - mean(Y[,1]))^2)
  # note: ssm + sse = sst

  # mean squared error (MSE) or mean squared deviation(MSD)
  mse <- sse / (n - k - 1)

  # residual standard error
  rse <- sqrt(mse)

  # predictors variance-covariance matrix
  var_cov <- mse * X_inverse

  # estimated standard error of coefficients
  coef_se <- sqrt(diag(var_cov))

  #R^2
  r_squared <- ssm / sst

  # adjusted r_squared
  r_squared_adj <- 1 - (1 - r_squared)*(n - 1)/(n - p)

  return(
    list(
      X = X,
      Y = Y,
      Coef = Coef[,1],
      Hat = Hat,
      Fitted_val = Fitted_val[,1],
      Resid = Resid[,1],
      ssm = ssm,
      sse = sse,
      mse = mse,
      rse = rse,
      var_cov = var_cov,
      coef_se = coef_se,
      r_squared = r_squared,
      r_squared_adj = r_squared_adj,
      n = n,
      k = k,
      p = p
    )
  )
}
