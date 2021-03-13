#' Function computes a simple correlation between variables of a data frame
#'
#' @description The data frame should have observations along the rows without NA values
#'  and the columns of variables are assumed to be numeric.
#'
#' @param df A data frame with columns for observed response and predictors values.
#'
#' @return A data frame of correlations between the variables.
#'
#' @author Rick Dean
#'
#' @export
compute_correlations <- function(df = NULL){
  # correlations between all variables
  n <- nrow(df)

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

    return(corr_df)
  }
}
