#' Scale-Center Deviation Scores
#' @description
#' Transforms scores of interest from original metric to range with minimum score
#' of -1 and 1.
#' @param x matrix to be transformed
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @return Scale-centered scores on [-1,1] range.
#'
#' @export

cx <- function(x, smin, smax) {
  cx <- (x - (smax + smin)/2)/(smax-smin)*2
  return(cx)
}
