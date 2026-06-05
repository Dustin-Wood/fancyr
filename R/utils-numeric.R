#' Coerce a vector to plain numeric, robust to haven_labelled and factors
#'
#' Data fetched with \code{\link{fetch_survey_plus}} carries \code{haven_labelled}
#' value labels. Those columns are numeric in spirit, but recent \pkg{vctrs}
#' versions make \code{as.numeric()} on a live \code{haven_labelled} vector raise
#' "Can't convert <haven_labelled> to <double>". Stripping the class first
#' (\code{unclass()}) sidesteps the cast machinery entirely and works across
#' \pkg{haven}/\pkg{vctrs} versions. Factors are sent through their character
#' levels so numeric-coded factors recover their numbers rather than their codes.
#'
#' @param x A vector (possibly \code{haven_labelled}, factor, character, or
#'   already numeric).
#' @return A plain \code{numeric} vector the same length as \code{x}.
#' @keywords internal
#' @noRd
zapNum <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  as.numeric(unclass(x))
}
