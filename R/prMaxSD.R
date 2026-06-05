#' Estimate Proportion of Observed Standard Deviation from Max Possible
#' @description
#' For a specified set of variables (with a common range), estimate the percentage
#' of response variability that the person showed from maximum possible
#' @param data Matrix to be transformed (often will need to specify subset of larger dataframe)
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @param dir Direction argument passed to \code{apply()}: 1 = rows (default), 2 = columns
#' @details This is frequently valuable to identify people who did not
#' vary their ratings substantially - which at extreme levels is generally indicative
#' of insufficient effort responding.
#' @references Dunn, A. M., Heggestad, E. D., Shanock, L. R., & Theilgard, N. (2018). Intra-individual response variability as an indicator of insufficient effort responding: Comparison to other indicators and relationships with individual differences. Journal of Business and Psychology, 33(1), 105-121.
#' @references Wood, D., Harms, P., Lowman, G. H., & DeSimone, J. A. (2017). Response speed and response consistency as mutually validating indicators of data quality in online samples. Social Psychological and Personality Science, 8, 454-464.
#'
#' Note: prMaxSD = .25 would be observed on a 5-point scale from a person rating ALL
#' items as 50\% one number and 50\% the adjacent number (e.g., 50\% 1's and 50\% 2's, or
#' 50\% 3's and 50\% 4's).
#'     Because it seems reasonably clear that these respondents with such score variability
#' should be cut, .25 is suggested as a reasonable cut point.
#'     Note also that the code makes a minor adjustment to calculate within-person standard deviation
#' as the population estimate (using N rather than N-1) to make maximum possible = 1
#' @return estimate of the proportion of the observed standard deviation
#' of the row scores from max possible (range from 0 to 1). Rows (or columns,
#' if \code{dir = 2}) with fewer than two non-missing responses return
#' \code{NA}, since variability is undefined there; this includes all-missing
#' rows, which are common for non-respondents in a screening pass.
#' @export
#' @examples
#' # Three respondents rating 5 items on a 1-5 scale
#' mat <- matrix(c(1, 2, 3, 4, 5,   # variable responder
#'                 3, 3, 3, 3, 3,   # straight-liner
#'                 1, 5, 1, 5, 1),  # highly variable
#'               nrow = 3, byrow = TRUE)
#' prMaxSD(mat, smin = 1, smax = 5)
#'
#' # Filter out low-variability respondents (prMaxSD < .25)
#' scores <- prMaxSD(mat, smin = 1, smax = 5)
#' mat[scores >= .25, ]

prMaxSD <- function(data, smin, smax, dir=1) {
  # Coerce to a plain numeric matrix, tolerating haven_labelled / factor
  # columns (e.g. straight from fetch_survey_plus): a bare as.matrix()/apply()
  # can error on those under recent vctrs. zapNum() strips the class first.
  if (is.data.frame(data)) {
    data <- vapply(data, zapNum, numeric(nrow(data)))
    if (is.null(dim(data))) data <- matrix(data, nrow = 1)
  } else {
    data <- as.matrix(data)
  }
  sd.p <- function(x) { sd(as.matrix(x), na.rm = TRUE) }
  prMaxSD <- apply(data, dir, function(x) {
    n <- sum(!is.na(x))
    # Fewer than two responses -> variability is undefined; return NA. The
    # guard also avoids sqrt() of a negative on all-NA rows (the lone source
    # of the old "NaNs produced" warning).
    if (n < 2) return(NA_real_)
    sd.p(x) * sqrt((n - 1) / n) / ((smax - smin) / 2)
  })
  return(prMaxSD)
}
