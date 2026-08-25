#' Stability Path Decomposition Across Many Y Variables
#' @description
#' Slides a single \code{\link{stabilityModel}} across a set of items, fitting
#' the same configuration of mediators (\code{X}) and controls once per item.
#' For each item, the total \code{Y1 -> Y2} stability coefficient is partitioned
#' into mediated paths (\code{Y1 -> X -> Y2}), confounded paths
#' (\code{Y1 <-> C -> Y2}), and the residual stability path.
#'
#' Both \code{X} and \code{controls} are optional vectors, so the same call
#' handles a classic single-mediator model, a multiple-mediator model, and a
#' mediator-free model that decomposes stability into confounding alone.
#'
#' @details
#' This is a convenience wrapper: it builds a \code{\link{stabilityModel}} and
#' hands it to \code{\link{modelOnAllY}}. For a different model over the same
#' kind of item set, build a \code{\link{fancyModel}} and call
#' \code{\link{modelOnAllY}} directly.
#'
#' Item columns are located by pasting \code{y1ind} and \code{y2ind} onto each
#' item base name. Items whose columns are absent, or whose model fails to
#' converge, contribute \code{NA} rows rather than being dropped, so every
#' returned data frame is rectangular across items; \code{$status} records why.
#'
#' Output is in long format: one row per item per pathway. This shape is the
#' same whether there is one mediator, several, or none, which is what allows
#' the mediator count to vary without changing the output contract. See
#' \code{\link{stabilityPaths}} for the algebra of the decomposition.
#'
#' @param data A data frame containing all variables. Item columns should carry
#'   the \code{y1ind} and \code{y2ind} suffixes (e.g. \code{"item[T1]"} and
#'   \code{"item[T2]"}).
#' @param items Character vector of item base names (without T1/T2 suffixes).
#' @param X Character vector of mediator ("experience") variable names, or
#'   \code{NULL} (default) for a model with no mediators.
#' @param controls Character vector of control variable names, linked to Y1 by
#'   undirected covariance. Defaults to \code{NULL}.
#' @param y1ind Suffix identifying T1 item columns. Defaults to \code{"[T1]"}.
#' @param y2ind Suffix identifying T2 item columns. Defaults to \code{"[T2]"}.
#' @param standardize Logical. If \code{TRUE}, z-standardize Y1, Y2, and all
#'   \code{X} and \code{controls} variables within each item's model. Makes
#'   pathway estimates comparable across items and controls. Defaults to
#'   \code{FALSE}.
#' @param missing Missing-data handling passed to \code{\link[lavaan]{sem}}.
#'   Defaults to \code{"fiml"}.
#' @param return_estimates Logical. If \code{TRUE} (default), include the full
#'   per-item result in \code{$modelEstimates}. These are what
#'   \code{\link{plotMedX}} consumes.
#'
#' @return A named list with components:
#' \item{paths}{Long data frame, one row per item per pathway, with columns
#'   \code{item}, \code{path}, \code{via}, \code{type}, \code{est}, \code{se},
#'   \code{pvalue}, \code{ci.lower}, \code{ci.upper}, \code{propTotal}. The
#'   \code{type} column is one of \code{"residual"}, \code{"mediated"},
#'   \code{"confounded"}, or \code{"total"}.}
#' \item{coefficients}{Long data frame of the underlying structural
#'   coefficients for every item, labelled with the original variable names.}
#' \item{totalStability}{Data frame of the total stability coefficient per item.}
#' \item{nobs}{Data frame of sample sizes per item.}
#' \item{status}{Data frame recording \code{"Success"} or the reason each item
#'   was skipped.}
#' \item{modelEstimates}{Named list of per-item results, or \code{NULL} if
#'   \code{return_estimates = FALSE}.}
#'
#' @seealso \code{\link{stabilityModel}} for the model specification,
#'   \code{\link{stabilityPaths}} for a single Y1/Y2 pair,
#'   \code{\link{modelOnAllY}} for the generic slider,
#'   \code{\link{plotMedX}} to draw one item's model, and
#'   \code{\link{xEffects}} to build the merged two-wave data frame.
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' C1 <- rnorm(n)
#' d <- data.frame(
#'   `a[T1]` = rnorm(n), `b[T1]` = rnorm(n), C1 = C1, check.names = FALSE
#' )
#' d$X1 <- 0.3 * d$`a[T1]` + rnorm(n)
#' d$`a[T2]` <- 0.5 * d$`a[T1]` + 0.3 * d$X1 + 0.2 * C1 + rnorm(n)
#' d$`b[T2]` <- 0.4 * d$`b[T1]` + 0.2 * d$X1 + rnorm(n)
#'
#' res <- allYstabilities(d, items = c("a", "b"), X = "X1", controls = "C1")
#' res$paths
#'
#' # mediator-free: stability decomposed into confounding alone
#' allYstabilities(d, items = c("a", "b"), controls = "C1")$paths
#'
#' @export
allYstabilities <- function(data, items, X = NULL, controls = NULL,
                            y1ind = "[T1]", y2ind = "[T2]",
                            standardize = FALSE, missing = "fiml",
                            return_estimates = TRUE) {

  spec <- stabilityModel(X = X, controls = controls)
  spec$sem_args$missing <- missing

  modelOnAllY(spec, data, items,
              suffixes         = c(Y1 = y1ind, Y2 = y2ind),
              standardize      = standardize,
              return_estimates = return_estimates)
}
