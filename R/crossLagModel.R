#' Build a Two-Wave Cross-Lagged Model Specification
#' @description
#' Constructs the \code{\link{fancyModel}} used by \code{\link{crossLagPaths}}:
#' a saturated two-wave cross-lagged panel model for an item \code{Y} and an
#' experience \code{X}, each measured at both waves. It estimates
#'
#' \itemize{
#'   \item \strong{selection} (\code{X2 ~ Y1}): the Time 1 item predicting the
#'     experience at Time 2, controlling for the experience at Time 1;
#'   \item \strong{change} (\code{Y2 ~ X1}): the Time 1 experience predicting
#'     the item at Time 2, controlling for the item at Time 1;
#'   \item the two \strong{stability} paths (\code{Y2 ~ Y1}, \code{X2 ~ X1});
#'   \item the Time 1 association (\code{Y1 ~~ X1}) and the Time 2
#'     \strong{co-change} (\code{Y2 ~~ X2}), the residual covariance left in
#'     the Time 2 scores once everything at Time 1 is accounted for;
#' }
#'
#' and it decomposes the stability of each variable, as
#' \code{\link{stabilityModel}} does, into a residual path, a cross-lagged
#' path, and one confounded path per control.
#'
#' @details
#' Writing \eqn{v_Y} for \eqn{Var(Y1)}, \eqn{v_X} for \eqn{Var(X1)}, and
#' \eqn{c} for their covariances, the two decompositions are
#'
#' \deqn{b_{Y2 \cdot Y1} = s_{YY} + (c_{YX}/v_Y)\, b_{change} +
#'   \sum_k (c_{YC_k}/v_Y)\, b_{Y2C_k}}
#' \deqn{b_{X2 \cdot X1} = s_{XX} + (c_{YX}/v_X)\, b_{selection} +
#'   \sum_k (c_{XC_k}/v_X)\, b_{X2C_k}}
#'
#' where each left side is the simple slope of the Time 2 score on the
#' Time 1 score. The cross-lagged pathway of the item's stability is the Time 1
#' association times the change effect; that of the experience's stability is
#' the Time 1 association times the selection effect. Controls predict both
#' Time 2 variables and covary with both Time 1 variables, and the Time 2
#' residuals covary, which leaves the model just-identified (df = 0), so each
#' decomposition sums exactly to its total.
#'
#' The co-change covariance is not decomposed further. It collects whatever
#' the Time 2 scores share that the stability and cross-lagged paths do not
#' carry: for example, change in both over the interval with a common cause,
#' or an effect of one on the other within the interval.
#'
#' @param X Character vector of length 2: the experience's Time 1 and Time 2
#'   column names, e.g. \code{c("power[T1]", "power[T2]")}.
#' @param controls Character vector of control variable names, or \code{NULL}
#'   (default).
#'
#' @return A \code{\link{fancyModel}} with sliding roles \code{c("Y1", "Y2")},
#'   fixed roles \code{X1}, \code{X2} and \code{C1..Ck}, and an \code{extract}
#'   table annotating each pathway with its \code{outcome} (\code{"Y"} or
#'   \code{"X"}, whose stability it decomposes) and \code{type}
#'   (\code{"residual"}, \code{"cross-lagged"}, \code{"confounded"} or
#'   \code{"total"}).
#'
#' @seealso \code{\link{crossLagPaths}} to fit it across a set of items, with
#'   optional reliability correction.
#'
#' @examples
#' crossLagModel(X = c("power[T1]", "power[T2]"), controls = "tenure")
#'
#' @export
crossLagModel <- function(X, controls = NULL) {

  if (missing(X) || length(X) != 2L)
    stop("`X` must give the experience's Time 1 and Time 2 column names.")
  X <- as.character(X)
  controls <- if (is.null(controls)) character(0) else as.character(controls)
  k <- length(controls)
  Cn <- if (k) paste0("C", seq_len(k)) else character(0)

  ctrl_on <- function(prefix)
    if (k) paste(sprintf(" + %sC%d*C%d", prefix, seq_len(k), seq_len(k)),
                 collapse = "") else ""

  L <- c(sprintf("Y2 ~ sYY*Y1 + chg*X1%s", ctrl_on("bY")),
         sprintf("X2 ~ sXX*X1 + sel*Y1%s", ctrl_on("bX")),
         "Y1 ~~ vY*Y1", "X1 ~~ vX*X1", "Y1 ~~ cYX*X1")
  for (i in seq_len(k))
    L <- c(L, sprintf("Y1 ~~ cYC%d*C%d", i, i), sprintf("X1 ~~ cXC%d*C%d", i, i))
  if (k > 1) for (p in utils::combn(k, 2, simplify = FALSE))
    L <- c(L, sprintf("C%d ~~ C%d", p[1], p[2]))
  L <- c(L, "Y2 ~~ cochg*X2")

  L <- c(L, "Y_viaX := (cYX/vY) * chg", "X_viaY := (cYX/vX) * sel")
  for (i in seq_len(k))
    L <- c(L, sprintf("Y_viaC%d := (cYC%d/vY) * bYC%d", i, i, i),
              sprintf("X_viaC%d := (cXC%d/vX) * bXC%d", i, i, i))
  sumOf <- function(first, via) paste(c(first, via), collapse = " + ")
  L <- c(L,
         sprintf("Y_total := %s", sumOf("sYY", c("Y_viaX", if (k) paste0("Y_viaC", seq_len(k))))),
         sprintf("X_total := %s", sumOf("sXX", c("X_viaY", if (k) paste0("X_viaC", seq_len(k))))))
  syntax <- paste(L, collapse = "\n")

  block <- function(o, first, cross, cross_via) {
    data.frame(
      path    = c("residual", paste0("via_", cross_via),
                  if (k) paste0("via_", controls), "total"),
      via     = c(NA_character_, cross_via, controls, NA_character_),
      type    = c("residual", "cross-lagged", rep("confounded", k), "total"),
      outcome = o,
      label   = c(first, cross, if (k) paste0(o, "_viaC", seq_len(k)),
                  paste0(o, "_total")),
      stringsAsFactors = FALSE)
  }
  # the item's column changes from fit to fit, so its pathway is named by role
  extract <- rbind(block("Y", "sYY", "Y_viaX", X[1]),
                   block("X", "sXX", "X_viaY", "Y1"))

  fancyModel(
    syntax   = syntax,
    slide    = c("Y1", "Y2"),
    vars     = stats::setNames(c(X, controls), c("X1", "X2", Cn)),
    extract  = extract,
    roles    = stats::setNames(c("Y1", "Y2", "X1", "X2", rep("control", k)),
                               c("Y1", "Y2", "X1", "X2", Cn)),
    # saturated (df = 0), so the baseline and h1 fits behind fit indices are
    # skipped; estimates and standard errors are unaffected
    sem_args = list(missing = "fiml", fixed.x = FALSE, h1 = FALSE, baseline = FALSE),
    label    = sprintf("two-wave cross-lagged model (%d control%s)",
                       k, if (k == 1) "" else "s")
  )
}
