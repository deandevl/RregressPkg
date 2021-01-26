#' Function estimates the parameters of the beta distribution to replicate
#'  a set of data based on their population mean and standard deviation.
#'
#' @description The function estimates the shape parameters of the beta
#'  distribution using the method of moments involving the submitted mean
#'  and standard deviation.  The returned data set reflects the [0,1] range
#'  of the beta distributions rescaled to the user's min/max units.
#'
#' @param n An integer that sets the number of beta distribution points to
#'  estimate using stats::rbeta().
#' @param mean A numeric that sets the population mean of the observed data.
#' @param sd A numeric that sets the population standard deviation of the data.
#' @param min_val A numeric that sets the population's minimal value.
#' @param max_val A numeric that sets the population's maximum value.
#' @param digits An integer that sets the number of digits to round the returned
#'  data.
#' @param seed An integer used by stats::rbeta() in setting the random seed.
#'
#' @return A list with the shape parameters used to estimate the beta distribution
#'  along with data.frames of estimated data in both unscaled and scaled format.
#'
#' @author Rick Dean
#'
#' @export
mom_beta <- function(n, mean, sd, min_val, max_val, digits = NULL, seed = NULL) {
  range_val <- max_val - min_val
  m <- (mean - min_val) / range_val
  s <- sd / range_val

  d <- ((m * (1-m))/s^2)-1
  a <- m * d
  b <- (a * (1 - m))/m

  if(!is.null(seed)){
    set.seed(seed)
  }
  data_unscaled <- stats::rbeta(n, a, b)
  data_scaled <- min_val + data_unscaled * range_val

  if(!is.null(digits)) {
    data_unscaled <- round(data_unscaled, digits = digits)
    data_scaled <- round(data_scaled, digits = digits)
  }

  return(list(
    a_shape = a,
    b_shape = b,
    df_unscaled = data.frame(data = data_unscaled),
    df_scaled = data.frame(data = data_scaled)
  ))
}
