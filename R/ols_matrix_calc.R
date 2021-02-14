#' Function computes the OLS coefficients via matrix operations
#'
#' @description Function provides an alternative to *stats::lm()* by using
#'  matrix based arguments for predictors and response in estimating the OLS
#'  parameters.
#'
#' @param X A numeric matrix or data frame with rows of observations and their respective predictor
#'  values across the columns.
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
  k <- ncol(X)
  p <- k + 1

  Interc <- c(Inter = 1)
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

  # corrected sum squares for model (SSM)
  ssm = sum((Fitted_val[,1] - mean(Y[,1]))^2)

  # sum of squares for error (SSE)
  sse <- sum((Y[,1] - Fitted_val[,1])^2)

  # mean squared error (MSE) or mean squared deviation(MSD)
  mse <- sse / (n - k - 1)

  # residual standard error
  rse <- sqrt(mse)

  # predictors variance-covariance matrix
  var_cov <- mse * X_intercept_inverse

  # estimated standard error of coefficients
  coef_se <- sqrt(diag(var_cov))

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
      n = n,
      k = k,
      p = p
    )
  )
}
