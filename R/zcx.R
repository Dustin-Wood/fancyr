#' Standardized Scale-Center Deviation Scores
#' @description
#' Transforms the scores of interest to a z-score like metric indicating
#' the number of 'standard scale-center deviations from the scale center'
#' @param x matrix to be transformed
#' @param center center point of the scale
#' @return Scale-centered scores on [-1,1] range.
#' @details Data should be prepared so that all of the person's scores are given
#' in the same row.
#'
#' \bullet The scale-centered scores do NOT have the usual interpretation of
#' [regular] z-scores as being translatable to approximate probabilities assuming
#' a normal distribution of responses.
#'
#' \bullet If the person gives the same score to all items, all \code{zcx}
#' will equal \code{1}.
#' \bullet If the person answers all items with only two responses that are equally
#' spaced above or below the scale midpoint (for instance, all responses equal
#' \code{1} & \code{5} or equal \code{2} & \code{4} when the scale midpont is \code{3},
#' then all scores will equal \code{-1} & \code {1}.
#' \bullet Note that \code{zcx} scores do NOT divide by \code{N_k-1} (where \code{N_k}
#' = number of items in profile) because a degree of freedom is not lost to estimate
#' the mean \code{\bar{x}}
#'
#'
#' @export

zcx <- function(x, center) {
  dx <- x - center
  zcx.t <- t(scale(t(cbind(dx,-dx))))[,1:ncol(dx)]
  zcx <- zcx.t * sqrt(ncol(dx)*2/(ncol(dx)*2-1))
  return(zcx)
}
