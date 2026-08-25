#' Build a Stability-Decomposition Model Specification
#' @description
#' Constructs the \code{\link{fancyModel}} used by \code{\link{stabilityPaths}}
#' and \code{\link{allYstabilities}}: a saturated path model that partitions the
#' total \code{Y1}-to-\code{Y2} stability coefficient into mediated paths
#' (\code{Y1 -> X -> Y2}), confounded paths (\code{Y1 <-> C -> Y2}), and a
#' residual stability path.
#'
#' Like \code{\link{randomIntModel}} and \code{\link{nullModellavaan}}, this
#' builds model syntax without touching data. Use it directly when you want to
#' inspect or hand-edit the syntax, or pass it to \code{\link{fitModel}} /
#' \code{\link{modelOnAllY}} to fit.
#'
#' @details
#' Writing \eqn{v_1} for \eqn{Var(Y1)} and \eqn{c_{1k}} for \eqn{Cov(Y1, C_k)},
#' the decomposition encoded here is
#'
#' \deqn{b_{total} = b_{21} + \sum_j b_{Xj1} b_{2Xj} +
#'   \sum_k (c_{1k}/v_1)(b_{2Ck} + \sum_j b_{XjCk} b_{2Xj})}
#'
#' The \eqn{c_{1k}/v_1} scaling puts each confounded path on the same metric as
#' the stability coefficient it decomposes; without it the term is inflated by a
#' factor of \eqn{Var(Y1)}. That ratio is numerically identical to the
#' coefficient from regressing \eqn{C_k} on \eqn{Y1}; the covariance form is
#' used because it is an equivalent just-identified parameterization that does
#' not assert a direction between \code{Y1} and each control.
#'
#' Controls predict every mediator and \code{Y2}, all control pairs covary, and
#' mediator residuals covary with one another. That leaves the model
#' just-identified (df = 0), which is what makes the decomposition exact.
#'
#' @param X Character vector of mediator ("experience") variable names, or
#'   \code{NULL} (default) for a model with no mediators.
#' @param controls Character vector of control variable names, or \code{NULL}
#'   (default).
#'
#' @return A \code{\link{fancyModel}} with sliding roles \code{c("Y1", "Y2")},
#'   fixed roles \code{X1..Xm} and \code{C1..Ck} bound to the supplied columns,
#'   and an \code{extract} table annotating each pathway as \code{"residual"},
#'   \code{"mediated"}, \code{"confounded"}, or \code{"total"}.
#'
#' @seealso \code{\link{stabilityPaths}} to fit one Y1/Y2 pair,
#'   \code{\link{allYstabilities}} to slide across an item set.
#'
#' @examples
#' # inspect the generated syntax
#' stabilityModel(X = c("course1", "course2"), controls = "SAT Math")
#'
#' @export
stabilityModel <- function(X = NULL, controls = NULL) {

  X        <- if (is.null(X))        character(0) else as.character(X)
  controls <- if (is.null(controls)) character(0) else as.character(controls)
  m <- length(X); k <- length(controls)

  Xn <- if (m) paste0("X", seq_len(m)) else character(0)
  Cn <- if (k) paste0("C", seq_len(k)) else character(0)

  ## ---- model syntax -------------------------------------------------------
  L <- character(0)
  ctrl_on <- function(prefix) {
    if (k) paste(sprintf(" + %sC%d*C%d", prefix, seq_len(k), seq_len(k)),
                 collapse = "") else ""
  }

  for (j in seq_len(m))
    L <- c(L, sprintf("X%d ~ bX%d1*Y1%s", j, j, ctrl_on(sprintf("bX%d", j))))

  L <- c(L, sprintf(
    "Y2 ~ b21*Y1%s%s",
    if (m) paste(sprintf(" + b2X%d*X%d", seq_len(m), seq_len(m)), collapse = "") else "",
    ctrl_on("b2")))

  L <- c(L, "Y1 ~~ v1*Y1")
  for (i in seq_len(k)) L <- c(L, sprintf("Y1 ~~ cov_1C%d*C%d", i, i))
  if (k > 1) for (p in utils::combn(k, 2, simplify = FALSE))
    L <- c(L, sprintf("C%d ~~ C%d", p[1], p[2]))
  # keep the model just-identified so the decomposition is exact
  if (m > 1) for (p in utils::combn(m, 2, simplify = FALSE))
    L <- c(L, sprintf("X%d ~~ X%d", p[1], p[2]))

  for (j in seq_len(m)) L <- c(L, sprintf("viaX%d := bX%d1 * b2X%d", j, j, j))
  for (i in seq_len(k)) {
    tot_Ci <- paste0(
      "b2C", i,
      if (m) paste(sprintf(" + bX%dC%d*b2X%d", seq_len(m), i, seq_len(m)),
                   collapse = "") else "")
    L <- c(L, sprintf("viaC%d := (cov_1C%d/v1) * (%s)", i, i, tot_Ci))
  }
  path_lbls <- c("b21",
                 if (m) paste0("viaX", seq_len(m)),
                 if (k) paste0("viaC", seq_len(k)))
  L <- c(L, sprintf("total := %s", paste(path_lbls, collapse = " + ")))
  syntax <- paste(L, collapse = "\n")

  ## ---- what to extract, and how to annotate it ----------------------------
  extract <- data.frame(
    label = c(path_lbls, "total"),
    path  = c("residual", if (m) paste0("via_", X), if (k) paste0("via_", controls), "total"),
    via   = c(NA_character_, X, controls, NA_character_),
    type  = c("residual", rep("mediated", m), rep("confounded", k), "total"),
    stringsAsFactors = FALSE
  )
  # column order matches the historical $paths layout: path, via, type first
  extract <- extract[, c("path", "via", "type", "label")]

  fancyModel(
    syntax   = syntax,
    slide    = c("Y1", "Y2"),
    vars     = if (m + k) stats::setNames(c(X, controls), c(Xn, Cn)) else NULL,
    extract  = extract,
    roles    = stats::setNames(
                 c("Y1", "Y2", rep("mediator", m), rep("control", k)),
                 c("Y1", "Y2", Xn, Cn)),
    sem_args = list(missing = "fiml", fixed.x = FALSE),
    label    = sprintf("stability decomposition (%d mediator%s, %d control%s)",
                       m, if (m == 1) "" else "s", k, if (k == 1) "" else "s")
  )
}
