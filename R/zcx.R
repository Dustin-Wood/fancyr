#' Standardized Scale-Center Deviation Scores
#' @description
#' Transforms the scores of interest to a z-score-like metric indicating
#' the number of 'standard scale-center deviations from the scale center'.
#'
#' This can be understood as a simple alteration of the usual z-score formula,
#' where in every place that the person's mean score would be found, we replace this
#' with the scale-center point instead (e.g., \code{3} for a \code{1-5} Likert scale):
#'
#' \deqn{zcx = (x-C)/s_C}
#' @param x matrix containing set of scores to be transformed
#' @param center center point of the scale
#' @param addsc Should the person's standard center-score-deviation (\code{sc.p}) be added
#' to the returned matrix? (defaults to \code{F})
#' @return Standardized scale-centered scores.
#' @details Data should be prepared so that all of the person's scores are given
#' in the same row.
#'
#' * The scale-centered scores do NOT have the usual interpretation of
#' [regular] z-scores as being translatable to approximate probabilities assuming
#' a normal distribution of responses.
#'
#' * If the person gives the same score to all items, all \code{zcx}
#' will equal \code{1}.
#'
#'
#' * If the person answers all items with only two responses that are equally
#' spaced above or below the scale midpoint (for instance, all responses equal
#' \code{1 & 5} or equal \code{2 & 4} when the scale midpont is \code{3},
#' then all scores will equal \code{-1 & 1}.
#'
#' * Note that \code{zcx} scores do NOT divide by \code{N_k-1} (where \code{N_k}
#' = number of items in profile) because a degree of freedom is not lost to estimate
#' the person's mean response, as done in standard z-scores.
#'
#'
#' @export

zcx <- function(x, center, addsc = F) {
  cx <- x - center
  sc.p <- apply(cx, 1, function(x) sqrt(sum(x^2)/ncol(cx)))
  zcx <- cx / sc.p
  if (addsc == T) {
    zcx <- cbind(zcx,sc.p)
  }
  return(zcx)
}
