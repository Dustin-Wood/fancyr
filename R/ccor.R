#' Scale-Center (or Cohen) Correlations, rc
#' @description
#' Estimate correlations between variables where the scale midpoint serves as
#' the reflection point for estimating correlations.
#'
#' These are equivalent to Cohen's (1969) rc correlations,
#' which are invariant across potentially arbitrary item- or variable-reversals.
#' @param x matrix to be used for correlations
#' @param mid Scale midpoint (default if not given = 0)
#' @param margin Specify whether correlations are of rows=1 or columns = 2 (default is columns)
#' @usage
#' ccor(x, mid = 3) #e.g., midpoint of standard 1-5 Likert scale is 3
#' @details
#' Function computes Cohen-adjusted correlations (or avg. product of standardized deviations-from-scale-center)
#' @return Scale-center correlations (\code{r_c})
#'
#' Note that these are NOT the same as regular correlations done on 'scale-centered' scores.
#' See Wood (202x) for details on how to interpret. Additionally: this script is not particularly
#' friendly toward missing values.
#' @export

ccor <- function(x, mid = 0, margin=2) {
  cx <- x - mid
  zcx <- apply(cx, margin, function(x) (x/sqrt((sum(x^2)/(length(x))))))
  ccor <- t(zcx) %*% zcx / nrow(zcx) #this last bit can be used to calculate matching pretty generally
  dimnames(ccor)<-list(colnames(x),colnames(x))
  return(ccor)
}

