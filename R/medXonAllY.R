#' Stability Path Decomposition Across Many Y Variables (deprecated)
#' @description
#' \strong{Deprecated.} Use \code{\link{allYstabilities}} instead. The name was
#' changed because the function no longer estimates "med X on all Y": mediators
#' are optional, and what it returns is a decomposition of stability into
#' mediated, confounded, and residual pathways.
#'
#' @details
#' This alias forwards to \code{\link{allYstabilities}} and therefore returns the
#' \strong{current long-format output}, not the wide format this function
#' returned before. Code that read \code{$summary}, \code{$Y1onX_bX1},
#' \code{$XonY2_b2X}, \code{$IndirectX_b2X1}, \code{$resStability_b21.X}, or
#' \code{$controlEffects_*} must be rewritten against \code{$paths} and
#' \code{$coefficients} regardless of which name it calls.
#'
#' @inheritParams allYstabilities
#'
#' @return The value of \code{\link{allYstabilities}}.
#'
#' @seealso \code{\link{allYstabilities}}
#'
#' @export
medXonAllY <- function(data, items, X = NULL, controls = NULL,
                       y1ind = "[T1]", y2ind = "[T2]",
                       standardize = FALSE, missing = "fiml",
                       return_estimates = TRUE) {

  .Deprecated(
    new = "allYstabilities",
    msg = paste0(
      "medXonAllY() is deprecated; use allYstabilities() instead.\n",
      "  Note the output is now LONG format: $paths and $coefficients ",
      "replace $summary,\n  $Y1onX_bX1, $XonY2_b2X, $IndirectX_b2X1, ",
      "$resStability_b21.X and $controlEffects_*."))

  allYstabilities(data = data, items = items, X = X, controls = controls,
                  y1ind = y1ind, y2ind = y2ind, standardize = standardize,
                  missing = missing, return_estimates = return_estimates)
}
