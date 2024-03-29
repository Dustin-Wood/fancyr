#' Standardized Scale-Center Deviation Scores
#' @description
#' Transforms the scores of interest to a z-score-like metric indicating
#' the number of 'standard scale-center deviations from the scale center'.
#'
#' This can be understood as a simple alteration of the usual z-score formula,
#' where in every place that the person's mean score is used within an equation,
#' we replace this with the scale-center point instead
#' (e.g., \code{3} for a \code{1-5} Likert scale):
#'
#' \deqn{zcx = (x-C)/s_C}
#' Where:
#' \deqn{s_C = sum(x-C)^2/N}
#'
#' Note that it is proper to divide the equation by \code{N} rather than
#' \code{(N-1)} because \code{C} is not an estimated parameter. So no degrees of
#' freedom are lost.
#' @param x matrix containing set of scores to be transformed
#' @param center center point of the scale
#' @param margin estimate zcx values using sc's calculated from \emph{row}(=1) or \emph{column}=2?
#' @return Standardized scale-centered scores.
#' @details Data should be prepared so that all of the person's scores are given
#' in the same row.
#'
#' * The scale-centered scores do NOT have the usual interpretation of a
#' [regular] z-scores as being translatable to approximate probabilities assuming
#' a normal distribution of responses.
#'
#' * If the person gives the same score to all items - and if this score is not
#' the scale midpoint) - all \code{zcx} values for this person will equal \code{1}.
#'
#' * If the person answers all items with only two responses that are equally
#' spaced above or below the scale midpoint (for instance, all responses equal
#' \code{1 & 5} or equal \code{2 & 4} when the scale midpont is \code{3},
#' then all scores will equal \code{-1 & 1}.
#'
#' * Note that \code{zcx} scores do NOT divide by \code{N_k-1} (where \code{N_k}
#' = number of items in profile) because a degree of freedom is not lost to estimate
#' the person's mean response, as done in standard z-scores.
#' Transforms the scores of interest to a z-score like metric indicating
#' the number of 'standard scale-center deviations from the scale center'
#'
#' * Note that this will tend to make scores more continuous EXCEPT for the scale midpoint,
#' which will be exactly \code{0} for anyone who provided that value after this transformation.
#'
#' @export

zcx <- function(x, center, margin = 1) {
  cx <- x - center
  do_zcx <- function(x) {
    sc.p <- sqrt(sum(x^2,na.rm = T)/sum(!is.na(x)))
    zcx <- x/sc.p
    return(zcx)
  }
  zcx <- apply(cx, margin, function(x) do_zcx(x) )
  return(zcx)
}

