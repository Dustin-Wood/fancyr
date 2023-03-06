#' Standard Errors of Retest- or Cross-Rater-Adjusted Correlation
#' @description
#' From Rene Mottus.  He writes:
#'
#' 'I got to this approach by applying some intuitions (like the mean of two
#' parallel correlations is their individual standard errors' mean divided by
#' sqrt(2), and then adjusted by the correlation) and then tinkering (the
#' initial results based on intuition alone were slightly off,
#' what needed tinkering was taking the roots and square roots at the right times).
#' It agrees well with the simulation results. If someone can tell me why then that
#' would be even better.'
#' @param rxy the geometric mean of the two cross-item, cross-rater correlations (in your case, cross-item, cross-time correlations)
#' @param rxxyy the geometric mean of the two  cross-rater correlations of the same item (in your case, retest correlations)
#' @param n number of observations associated with both rxy and rxxyy
#' @return standard error of reliability-adjusted rxy correlation
#'
#' @export


expRse <- function(rxy,rxxyy,n) {
  r.var <- function(r,n) (1-r)/(n-2)
  rxy <- abs(rxy)
  rxxyy <- abs(rxxyy)
  true.r <- abs(rxy/rxxyy)
  rxy.var <- sqrt((r.var(rxy, n)) / sqrt(2+2*rxy))
  rxxyy.var <- sqrt((r.var(rxxyy, n)) / sqrt(2+2*rxxyy))
  vars <- cbind(rxy.var, rxxyy.var)  / ( 1 + sqrt(2)*true.r - true.r)
  sqrt( rowSums((vars/cbind(rxy,rxxyy))^2) ) * true.r
}
