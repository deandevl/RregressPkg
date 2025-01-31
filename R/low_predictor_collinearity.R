#' @title low_predictor_collinearity
#'
#' @description
#' Function provides a way to identify a set of predictors with pairwise low collinearity among themselves.
#'
#' With the submission of a data frame of raw predictor values or correlation matrix among predictors, the function
#'  removes the minimum number of predictors to ensure that all correlations are below a certain threshold.
#'
#' @details
#' Function was inspired by "Applied Predictive Modeling", Kuhn, Johnson, Page 47.
#'
#' Note that predictors of the data frame must all be numeric without NA values.
#'
#' The function's algorithm follows the following steps:
#'
#' 1. Create a starting list of the all the candidate predictors.
#'
#' 2. Create a second list of pairs of predictors with correlations above a given threshold and order the correlations
#'  from high to low.
#'
#' 3. For each pair of predictors (call them A and B) in the ordered list, determine the average correlation between
#'   predictor A and the other predictors.  Do the same for predictor B.
#'
#' 4. If A has a larger absolute average correlation, remove it from the ordered list and from the start list created in step 1;
#'   otherwise remove predictor B.
#'
#' 5. Repeat steps 3-4 through the entire ordered list of correlations defined in step 2, removing potential predictors from the
#'   starting list created in step 1.
#'
#' 6. The predictors left in the starting list are identified as having a low level of collinearity.
#'
#' @param df An optional numeric data frame of predictor variables without \code{NA} values
#' @param cor An optional matrix of cross correlations among the predictor variables
#' @param threshold A numeric that sets the minimum correlation between pairs of predictors to run through the algorithm.
#'
#' @return Returning a named list with:
#' \enumerate{
#'   \item "predictors" A character vector with the names of predictors with pairwise low collinearity among themselves.
#'   \item "correlations" The correlation matrix with just the selected predictors.
#'   \item "max_correlation" The maximum correlation among all pairs of the selected predictors.
#' }
#'
#' @examples
#' library(data.table)
#' library(RregressPkg)
#'
#' bloodpress_predictors_dt <- RregressPkg::bloodpress[, !c("BP")]
#' low_collinearity_lst <- RregressPkg::low_predictor_collinearity(
#'   df = bloodpress_predictors_dt
#' )
#'
#' @import data.table
#'
#' @export
low_predictor_collinearity <- function(
  df = NULL,
  cor = NULL,
  threshold = 0.75
){

  cor_val <- cor_row <- cor_column <- NULL

  if(is.null(df) & is.null(cor)){
    stop("Either a data.frame of predictor values or their correlations must be submitted.")
  }

  # compute the correlation matrix if necessary
  if(!is.null(df)){
    predictor_names <- colnames(df)
    if(!anyNA(df)){
      # compute variable means
      means_v <- apply(X = df, MARGIN = 2, FUN = function(x){
        mean(x)
      })

      # create a means for each column of variables
      n <- nrow(df)
      M <- matrix(means_v, nrow = 1)
      means_mt <- matrix(data = 1, nrow = n) %*% M

      # subtract the means from data
      df_mt <- as.matrix(df)
      diff_mt <- df_mt - means_mt

      # create covariance matrix
      var_cor_mt <- t(diff_mt) %*% diff_mt * (n - 1)^-1

      # compute variables sd and sd cross products
      var_mt <- diag(var_cor_mt)
      sd_mt <- sqrt(var_mt)
      sd_product_mt <- sd_mt %*% t(sd_mt)

      # divide var-cor matrix by sd cross products
      corr_mt <- var_cor_mt / sd_product_mt
    }else {
      stop("Predictors must not have missing values")
    }
  }else if(!is.null(cor)){
    corr_mt <- cor
    predictor_names <- colnames(corr_mt)
  }

  corr_flat_dt <- flatten_corr(corr_mt)
  # Get the pairs of variables with absolute correlation values > threshold:
  corr_threshold_dt <- corr_flat_dt[abs(cor_val) > threshold,][order(-cor_val)]
  predictor_correlations <- NULL

  # Find the max pair among seg_correlations_threshold_df
  while(nrow(corr_threshold_dt) != 0){
    A_var <- corr_threshold_dt$cor_row[[1]]
    B_var <- corr_threshold_dt$cor_column[[1]]

    A_vars <- corr_threshold_dt[cor_row == A_var | cor_column == A_var]
    A_var_mean <- mean(A_vars$cor_val)

    B_vars <- corr_threshold_dt[cor_row == B_var | cor_column == B_var]
    B_var_mean <- mean(B_vars$cor_val)

    if(abs(A_var_mean) >= abs(B_var_mean)){
      predictor_names <- predictor_names[predictor_names != A_var]
      corr_threshold_dt <- corr_threshold_dt[cor_row != A_var & cor_column != A_var]
    }else{
      predictor_names <- predictor_names[predictor_names != B_var]
      corr_threshold_dt <- corr_threshold_dt[cor_row != B_var & cor_column != B_var]
    }
    predictor_correlations <-  corr_mt[predictor_names,predictor_names]
  }

  return(
    list(
      predictors = predictor_names,
      correlations = predictor_correlations,
      max_correlation = max(predictor_correlations[upper.tri(predictor_correlations)])
    )
  )
}

# Defines a function for flattening the upper triangular portion of the correlation matrix:
flatten_corr <- function(corr){
  corr_upper <- upper.tri(corr)
  dt <- data.table(
    cor_row = rownames(corr)[row(corr)[corr_upper]],
    cor_column = rownames(corr)[col(corr)[corr_upper]],
    cor_val = corr[corr_upper],stringsAsFactors = FALSE
  )
  return(dt)
}
