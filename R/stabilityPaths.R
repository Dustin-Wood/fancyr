#' Decompose Y1-Y2 Stability into Mediated and Confounded Pathways
#' @description
#' Fits a saturated path model that partitions the total stability coefficient
#' linking \code{Y1} to \code{Y2} into three kinds of pathway:
#'
#' \itemize{
#'   \item \strong{Mediated} paths \code{Y1 -> X -> Y2}, one per variable in
#'     \code{X}, each a specific indirect effect controlling for the other
#'     mediators and all controls.
#'   \item \strong{Confounded} paths \code{Y1 <-> C -> Y2}, one per variable in
#'     \code{controls}. The \code{Y1}-\code{C} link is estimated as an
#'     \emph{undirected covariance}, so the model makes no claim about whether
#'     \code{Y1} influences \code{C} or the reverse.
#'   \item The \strong{residual} stability path \code{Y1 -> Y2} remaining once
#'     all mediators and controls are in the model.
#' }
#'
#' These terms sum exactly to the total stability coefficient, i.e. the simple
#' regression of \code{Y2} on \code{Y1} with nothing else in the model.
#'
#' @details
#' Writing \eqn{v_1} for \eqn{Var(Y1)}, \eqn{c_{1k}} for \eqn{Cov(Y1, C_k)}, and
#' using the fitted coefficients, the decomposition is
#'
#' \deqn{b_{total} = b_{21} + \sum_j b_{Xj1} b_{2Xj} +
#'   \sum_k (c_{1k}/v_1)(b_{2Ck} + \sum_j b_{XjCk} b_{2Xj})}
#'
#' The \eqn{c_{1k}/v_1} scaling puts each confounded path on the same metric as
#' the stability coefficient it decomposes; without it the term is inflated by a
#' factor of \eqn{Var(Y1)} and the components do not sum to the total. Note that
#' \eqn{c_{1k}/v_1} is numerically identical to the coefficient from regressing
#' \eqn{C_k} on \eqn{Y1} -- the covariance parameterization is used because it is
#' an equivalent just-identified form that does not assert a direction.
#'
#' Each control's contribution uses its \emph{total} effect on \code{Y2}, which
#' includes routes running onward through the mediators. A consequence is that
#' the confounded shares are unchanged by adding or removing mediators; mediators
#' subdivide the residual path only.
#'
#' Controls are entered as predictors of every mediator and of \code{Y2}, all
#' control pairs are left free to covary, and mediator residuals are free to
#' covary with each other. This leaves the model just-identified (df = 0), which
#' is what makes the decomposition exact.
#'
#' @param data A data frame containing all named variables.
#' @param Y1 Name of the Time 1 outcome column.
#' @param Y2 Name of the Time 2 outcome column.
#' @param X Character vector of mediator ("experience") variable names, or
#'   \code{NULL} (default) to fit a model with no mediators, decomposing
#'   stability into confounded and residual paths only.
#' @param controls Character vector of control variable names linked to
#'   \code{Y1} by undirected covariance, or \code{NULL} (default).
#' @param standardize Logical. If \code{TRUE}, z-standardize \code{Y1},
#'   \code{Y2}, and all \code{X} and \code{controls} variables before fitting.
#'   Confounded-path terms are comparable across controls only when this is
#'   \code{TRUE}, since otherwise each is expressed in its own control's units.
#'   Defaults to \code{FALSE}.
#' @param missing Missing-data handling passed to \code{\link[lavaan]{sem}}.
#'   Defaults to \code{"fiml"}.
#' @param return_fit Logical. If \code{TRUE}, include the fitted lavaan object
#'   in \code{$fit}. Defaults to \code{FALSE}.
#'
#' @return A named list with components:
#' \item{paths}{Data frame of the decomposition: one row per pathway plus a
#'   \code{total} row, with \code{est}, \code{se}, \code{pvalue}, confidence
#'   limits, and \code{propTotal} (each path's share of total stability).}
#' \item{coefficients}{Data frame of the underlying structural coefficients,
#'   labelled with the original variable names.}
#' \item{totalStability}{The total stability coefficient (scalar).}
#' \item{n}{Number of observations used.}
#' \item{converged}{Logical; \code{FALSE} if the model failed to fit, in which
#'   case \code{paths} is returned with \code{NA} estimates.}
#' \item{syntax}{The generated lavaan model syntax, as a character string.}
#' \item{varmap}{Data frame mapping internal canonical names to originals.}
#' \item{fit}{The lavaan fit object, if \code{return_fit = TRUE}.}
#'
#' @seealso \code{\link{stabilityModel}} for the underlying model specification,
#'   \code{\link{allYstabilities}} to apply the same model across many items,
#'   and \code{\link{plotMedX}} to draw the result.
#'
#' @examples
#' set.seed(1)
#' n  <- 400
#' C1 <- rnorm(n)
#' Y1 <- 0.5 * C1 + rnorm(n)
#' X1 <- 0.35 * Y1 + rnorm(n)
#' Y2 <- 0.45 * Y1 + 0.30 * X1 + 0.25 * C1 + rnorm(n)
#' d  <- data.frame(Y1, Y2, X1, C1)
#'
#' # mediated + confounded decomposition
#' stabilityPaths(d, "Y1", "Y2", X = "X1", controls = "C1")$paths
#'
#' # no mediator: confounding only
#' stabilityPaths(d, "Y1", "Y2", controls = "C1")$paths
#'
#' @export
stabilityPaths <- function(data, Y1, Y2, X = NULL, controls = NULL,
                           standardize = FALSE, missing = "fiml",
                           return_fit = FALSE) {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (length(Y1) != 1L || length(Y2) != 1L)
    stop("`Y1` and `Y2` must each name a single column.")

  X        <- if (is.null(X))        character(0) else as.character(X)
  controls <- if (is.null(controls)) character(0) else as.character(controls)

  allvars <- c(Y1, Y2, X, controls)
  missing_cols <- setdiff(allvars, names(data))
  if (length(missing_cols))
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "))
  if (anyDuplicated(allvars))
    stop("A variable is named more than once across Y1/Y2/X/controls: ",
         paste(unique(allvars[duplicated(allvars)]), collapse = ", "))

  spec <- stabilityModel(X = X, controls = controls)
  spec$sem_args$missing <- missing

  out <- fitModel(spec, data,
                  bind = c(Y1 = Y1, Y2 = Y2),
                  standardize = standardize,
                  return_fit  = return_fit)

  if (!out$converged)
    warning("stabilityPaths: model did not fit (", out$status, ")")

  out
}
