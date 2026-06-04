#' Invert the Scale-Center (\code{cx}) Transformation
#'
#' @description
#' Transforms scores back from the \eqn{[-1, 1]} range to the original
#' response scale. Exact inverse of \code{\link{cx}}.
#'
#' @param cx Numeric vector, matrix, or data frame of scores already on
#'   \eqn{[-1, 1]}. Any value outside this range causes an error.
#' @param newmin Minimum of the original response scale (e.g., \code{1} for
#'   a 1–5 Likert item).
#' @param newmax Maximum of the original response scale (e.g., \code{5}).
#'
#' @return Numeric object of the same shape as \code{cx}, with values on
#'   \eqn{[newmin, newmax]}.
#'
#' @seealso \code{\link{cx}} for the forward transformation.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' round(uncx(cx(x, smin = 1, smax = 5), newmin = 1, newmax = 5), 10) == x
#'
#' @export
uncx <- function(cx, newmin, newmax) {
  if (any(cx > 1, na.rm = TRUE))
    stop("Values > 1 found; 'cx' input must be in [-1, 1].")
  if (any(cx < -1, na.rm = TRUE))
    stop("Values < -1 found; 'cx' input must be in [-1, 1].")
  cx * (newmax - newmin) / 2 + (newmax + newmin) / 2
}
