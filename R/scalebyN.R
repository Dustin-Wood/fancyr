#' Scale by N
#' @description
#' Estimate z-scores for column variables using N rather than standard N-1
#' @param x matrix containing set of scores to be transformed
#' @param center mean-center scores (run \code{?scale} for more information
#' @param scale divide by standard deviation (run \code{?scale} for more information
#' @return Standardized scores, calculated using N rather than N-1 for standard deviations
#' @details
#' \emph{Why do this?} Because it is easier to show that correlations are equivalent to (1)
#'  the average product of zX*zY scores (Cohen, Cohen, Aiken, & West, 2003; Eq. 2.3.1) or (2)
#'  the average squared difference of zX and zY scores (Cohen, Cohen, Aiken, & West, 2003; Eq. 2.2.4)
#'  when using z-scores computed by using N rather than N-1.  Indeed, when done so, the averages of these
#'  product decompositions become strictly equivalent.
#'
#' @export

scalebyN <- function(x, center = T, scale = T) {
  zx <- scale(x, center = center, scale = scale)
  n<-colSums(!is.na(x))
  zxN<-t((sqrt(n)*t(zx))/sqrt(n-1))
  return(zxN)
}
