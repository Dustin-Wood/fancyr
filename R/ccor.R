#' Scale-Center (or Cohen) Correlations, rc
#' @description
#' Estimate correlations between variables where the scale midpoint serves as
#' the reflection point for estimating correlations.
#'
#' These are equivalent to Cohen's (1969) rc correlations,
#' which are invariant across potentially arbitrary item- or variable-reversals.
#' @param x matrix to be used for correlations
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @param margin Specify whether correlations are of rows=1 or columns = 2 (default is columns)
#' @usage
#' ccor(x, smin, smax)
#' ccor(x, smin = 1, smax = 6) #e.g., standard 1-5 Likert scale
#' @details
#' Function computes Cohen-adjusted correlations (or avg. product of standardized deviations-from-scale-center)
#'
#' Note that these are NOT the same as regular correlations of 'scale-centered' scores.  See Wood (202x) for details on how to interpret.
#' @export

ccor <- function(x, smin, smax, margin=2) {
  cx <- (x - (smax + smin)/2)/(smax-smin)*2
  zcx <- apply(cx, margin, function(x) (x/sqrt((sum(x^2)/(length(x))))))
  ccor <- t(zcx) %*% zcx / nrow(zcx) #this last bit can be used to calculate matching pretty generally
  return(ccor)
}

