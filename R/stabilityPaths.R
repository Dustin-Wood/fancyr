#' Decompose Stability into Mediated, Confounded, and Residual Pathways
#' @description
#' For each item measured at two waves, splits its stability -- how strongly
#' Time 1 scores predict Time 2 scores -- into the parts carried by
#' different routes:
#'
#' \itemize{
#'   \item \strong{Mediated} (\code{Y1 -> X -> Y2}): stability that runs through
#'     an experience. People's Time 1 standing predicts whether they have the
#'     experience (\emph{selection}), and the experience predicts their Time 2
#'     standing (\emph{socialization}); the product of the two is this path.
#'   \item \strong{Confounded} (\code{Y1 <-> C -> Y2}): stability attributable to
#'     a stable third variable, such as a demographic, that is associated with
#'     Time 1 scores and predicts Time 2 scores.
#'   \item \strong{Residual} (\code{Y1 -> Y2}): stability left once the mediated
#'     and confounded routes are accounted for.
#' }
#'
#' The parts sum exactly to the \strong{total} stability. In the default
#' standardized metric the total is the retest correlation, and each part's
#' \code{share} is the proportion of it carried by that route.
#'
#' Optionally, items (and continuous experiences or controls) can be corrected
#' for measurement error by supplying their \code{reliability}, which models
#' each as a latent variable. Unreliable Time 1 scores otherwise leave part of
#' the true trait unadjusted, which inflates the apparent effect of the
#' experience and the share of stability it seems to carry.
#'
#' @details
#' Writing \eqn{v_1} for \eqn{Var(Y1)} and \eqn{c_{1k}} for \eqn{Cov(Y1, C_k)},
#' the decomposition is
#'
#' \deqn{b_{total} = b_{21} + \sum_j b_{X_j1} b_{2X_j} +
#'   \sum_k (c_{1k}/v_1)(b_{2C_k} + \sum_j b_{X_jC_k} b_{2X_j})}
#'
#' Each confounded term uses the control's \emph{total} effect on \code{Y2},
#' including routes onward through the mediators, so confounded shares do not
#' change when mediators are added or removed; mediators subdivide the residual
#' path only. The \code{Y1}-\code{C} link is an undirected covariance, so the
#' model makes no claim about which causes which. Controls predict every
#' mediator and \code{Y2}, and mediator residuals covary, which leaves the model
#' just-identified (df = 0): this is what makes the parts sum exactly. See
#' \code{\link{stabilityModel}} for the model syntax.
#'
#' In the standardized metric every term is rescaled by the same factor,
#' \eqn{SD(Y1)/SD(Y2)}, so additivity holds in both metrics. With latent items
#' the standardization is of the latent variables: the total is then the
#' retest correlation corrected for unreliability.
#'
#' @section Reliability:
#' \code{reliability} accepts, from simplest to most specific:
#' \itemize{
#'   \item one number, used for every item at both waves, e.g. \code{.7};
#'   \item a named vector, e.g. \code{c(dominant = .75, warm = .6, ses = .9)}.
#'     Item names set that item's reliability at both waves; names of \code{X}
#'     or \code{controls} variables set that variable's reliability in every
#'     item's model. Items not named are left observed;
#'   \item a data frame with columns \code{item}, \code{T1} and \code{T2}, for
#'     reliabilities that differ between waves. Rows naming an \code{X} or
#'     \code{controls} variable use their \code{T1} value.
#' }
#' A suitable value for a single item is its short-interval retest correlation
#' (e.g. over a few days or weeks, when the trait itself should not have
#' changed). Standard errors treat the reliability as known, and results can
#' depend heavily on it: use \code{\link{reliabilitySensitivity}} to see how
#' much. Do not supply a reliability for a binary experience; a 0/1 indicator
#' of whether something happened is not a fallible measure of a continuous
#' latent variable.
#'
#' If a supplied reliability is lower than the retest correlation implies is
#' possible, the corrected stability exceeds 1. That item is still reported,
#' but flagged \code{admissible = FALSE}.
#'
#' @inheritSection stabilityData Missing data
#'
#' @param data A data frame with one row per person, typically from
#'   \code{\link{stabilityData}}, holding item columns named with the
#'   \code{suffixes} (e.g. \code{"dominant[T1]"}, \code{"dominant[T2]"}) plus
#'   any \code{X} and \code{controls} columns.
#' @param items Character vector of item base names. Defaults to the
#'   \code{"items"} attribute set by \code{\link{stabilityData}}. A single item
#'   is fine.
#' @param X Character vector of experience (mediator) variable names, or
#'   \code{NULL} (default) to decompose stability into confounded and residual
#'   parts only. Several are fitted as parallel mediators.
#' @param controls Character vector of control variable names, or \code{NULL}
#'   (default).
#' @param reliability Optional reliabilities for latent-variable correction;
#'   see the Reliability section. \code{NULL} (default) treats every variable as
#'   observed without error.
#' @param metric \code{"std"} (default) for fully standardized estimates;
#'   \code{"raw"} for estimates in the variables' own units.
#' @param suffixes Length-2 character vector: the suffixes that mark each
#'   item's Time 1 and Time 2 columns. Defaults to \code{c("[T1]", "[T2]")}.
#' @param missing Missing-data method passed to \code{\link[lavaan]{sem}}.
#'   Defaults to \code{"fiml"}; \code{"listwise"} drops incomplete cases.
#'
#' @return An object of class \code{fancyStability}: a list with
#' \item{paths}{Long data frame, one row per item per pathway: \code{item},
#'   \code{path}, \code{via}, \code{type} (\code{"residual"}, \code{"mediated"},
#'   \code{"confounded"} or \code{"total"}), \code{est}, \code{se},
#'   \code{pvalue}, \code{ci.lower}, \code{ci.upper}, and \code{share}.}
#' \item{coefficients}{Long data frame of the structural coefficients behind
#'   the decomposition for every item, e.g. selection (\code{X ~ Y1}) and
#'   socialization (\code{Y2 ~ X}).}
#' \item{summary}{Wide data frame, one row per item: sample sizes
#'   (\code{n} used by the model; \code{n_T1}, \code{n_T2}, \code{n_both}
#'   observed), the observed retest correlation \code{r_obs}, the reliabilities
#'   used, \code{admissible}, \code{status}, then an estimate and \code{_p}
#'   column for every pathway and structural coefficient.}
#' \item{status}{Data frame of each item's fitting status and admissibility.}
#' \item{fits}{Named list of the per-item \code{\link{fitModel}} results.}
#' \item{settings}{The \code{items}, \code{X}, \code{controls}, resolved
#'   reliabilities, \code{metric}, \code{suffixes} and \code{missing} used.}
#' \item{data}{The columns of \code{data} the models used, kept so the analysis
#'   can be refitted by \code{\link{reliabilitySensitivity}}.}
#'
#' Print the object for a compact table of each item's decomposition,
#' \code{summary()} it for standard errors and the underlying selection and
#' socialization paths, and \code{plot()} it to compare items (or draw one
#' item's path diagram with \code{plot(x, item = )}).
#'
#' @seealso \code{\link{stabilityData}} to build \code{data},
#'   \code{\link{plot.fancyStability}}, \code{\link{reliabilitySensitivity}},
#'   and \code{\link{stabilityModel}} / \code{\link{modelOnAllY}} for the
#'   underlying machinery.
#'
#' @examples
#' d <- stabilityData(stabilitySim$T1, stabilitySim$T2, stabilitySim$experience,
#'                    fill = list(leader = 0))
#'
#' # observed items
#' sp <- stabilityPaths(d, X = "leader", controls = "ses")
#' sp
#'
#' # corrected for each item's unreliability
#' spL <- stabilityPaths(d, X = "leader", controls = "ses",
#'                       reliability = stabilitySim$reliability)
#' spL
#' summary(spL)
#' plot(spL)
#' plot(spL, item = "dominant")
#'
#' @export
stabilityPaths <- function(data, items = attr(data, "items"), X = NULL,
                           controls = NULL, reliability = NULL,
                           metric = c("std", "raw"),
                           suffixes = c("[T1]", "[T2]"), missing = "fiml") {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  metric <- match.arg(metric)
  if (is.null(items) || !length(items))
    stop("No `items` given, and `data` has no \"items\" attribute ",
         "(set by stabilityData()).")
  items    <- as.character(items)
  X        <- if (is.null(X))        character(0) else as.character(X)
  controls <- if (is.null(controls)) character(0) else as.character(controls)
  if (length(suffixes) != 2L)
    stop("`suffixes` must give two suffixes: Time 1 then Time 2.")
  suffixes <- stats::setNames(as.character(suffixes), c("Y1", "Y2"))

  absent <- setdiff(c(X, controls), names(data))
  if (length(absent))
    stop("Column(s) not found in `data`: ", paste(absent, collapse = ", "))
  dup <- c(X, controls)[duplicated(c(X, controls))]
  if (length(dup))
    stop("A variable is named more than once across X/controls: ",
         paste(unique(dup), collapse = ", "))
  cols_T1 <- paste0(items, suffixes[["Y1"]])
  cols_T2 <- paste0(items, suffixes[["Y2"]])
  has_cols <- cols_T1 %in% names(data) & cols_T2 %in% names(data)
  if (!any(has_cols))
    stop("None of the items has both a ", suffixes[["Y1"]], " and a ",
         suffixes[["Y2"]], " column in `data`.")

  ## ---- reliabilities, resolved to one role-keyed vector per item ----------
  rel <- resolveReliability(reliability, items, X, controls)

  spec <- stabilityModel(X = X, controls = controls)
  spec$sem_args$missing <- missing

  res <- modelOnAllY(spec, data, items,
                     suffixes         = suffixes,
                     reliability      = rel$by_item,
                     metric           = metric,
                     return_estimates = TRUE)

  ## ---- per-item descriptives for the summary ------------------------------
  desc <- do.call(rbind, lapply(seq_along(items), function(i) {
    if (!has_cols[i])
      return(data.frame(n_T1 = NA_integer_, n_T2 = NA_integer_,
                        n_both = NA_integer_, r_obs = NA_real_))
    y1 <- as.numeric(data[[cols_T1[i]]]); y2 <- as.numeric(data[[cols_T2[i]]])
    both <- !is.na(y1) & !is.na(y2)
    data.frame(n_T1 = sum(!is.na(y1)), n_T2 = sum(!is.na(y2)), n_both = sum(both),
               r_obs = if (sum(both) > 2) stats::cor(y1[both], y2[both])
                       else NA_real_)
  }))

  s <- res$summary
  lead <- data.frame(item = s$item, n = s$n, desc,
                     rel_T1 = rel$table$T1, rel_T2 = rel$table$T2,
                     admissible = res$status$admissible, status = s$status,
                     stringsAsFactors = FALSE)
  summary_df <- cbind(lead, s[, setdiff(names(s), c("item", "n", "status")),
                              drop = FALSE])
  rownames(summary_df) <- NULL

  keep_cols <- intersect(c(attr(data, "id"), cols_T1, cols_T2, X, controls),
                         names(data))

  structure(
    list(
      paths        = res$paths,
      coefficients = res$coefficients,
      summary      = summary_df,
      status       = res$status,
      fits         = res$modelEstimates,
      settings     = list(items = items, X = X, controls = controls,
                          reliability = rel, metric = metric,
                          suffixes = suffixes, missing = missing),
      data         = data[, keep_cols, drop = FALSE]
    ),
    class = "fancyStability"
  )
}

# Turn the user-facing `reliability` argument into (a) a list, named by item,
# of role-keyed vectors for modelOnAllY(), and (b) an item-by-wave table for
# display. Roles: Y1/Y2 for the item, X1..Xm for X, C1..Ck for controls.
resolveReliability <- function(reliability, items, X, controls) {
  n <- length(items)
  tab <- data.frame(item = items, T1 = rep(NA_real_, n), T2 = rep(NA_real_, n),
                    stringsAsFactors = FALSE)
  other <- stats::setNames(numeric(0), character(0))  # X / control reliabilities
  role_of <- stats::setNames(
    c(character(0),
      if (length(X)) paste0("X", seq_along(X)),
      if (length(controls)) paste0("C", seq_along(controls))),
    c(X, controls))

  if (is.null(reliability)) {
    # nothing to do
  } else if (is.data.frame(reliability)) {
    need <- setdiff(c("item", "T1", "T2"), names(reliability))
    if (length(need))
      stop("A data frame `reliability` needs columns item, T1 and T2; missing: ",
           paste(need, collapse = ", "))
    unknown <- setdiff(reliability$item, c(items, X, controls))
    if (length(unknown))
      stop("`reliability` names variable(s) that are not items, X or controls: ",
           paste(unknown, collapse = ", "))
    hit <- match(items, reliability$item)
    tab$T1 <- reliability$T1[hit]
    tab$T2 <- reliability$T2[hit]
    ov <- reliability[reliability$item %in% names(role_of), , drop = FALSE]
    other <- stats::setNames(ifelse(is.na(ov$T1), ov$T2, ov$T1), ov$item)
  } else if (is.numeric(reliability)) {
    nm <- names(reliability)
    if (is.null(nm)) {
      if (length(reliability) == 1L) {
        tab$T1 <- tab$T2 <- reliability
      } else if (length(reliability) == n) {
        tab$T1 <- tab$T2 <- unname(reliability)
      } else {
        stop("An unnamed `reliability` must be one number or one per item (",
             n, "); got ", length(reliability), ".")
      }
    } else {
      unknown <- setdiff(nm, c(items, X, controls))
      if (length(unknown))
        stop("`reliability` names variable(s) that are not items, X or ",
             "controls: ", paste(unknown, collapse = ", "))
      hit <- match(items, nm)
      tab$T1 <- tab$T2 <- unname(reliability[hit])
      other <- reliability[nm %in% names(role_of)]
    }
  } else {
    stop("`reliability` must be a number, a named numeric vector, or a data ",
         "frame with columns item, T1, T2.")
  }

  vals <- c(tab$T1, tab$T2, other)
  bad  <- !is.na(vals) & (vals <= 0 | vals > 1)
  if (any(bad)) stop("Reliabilities must be in (0, 1].")

  other_roles <- if (length(other))
    stats::setNames(unname(other), role_of[names(other)]) else NULL
  by_item <- lapply(seq_len(n), function(i)
    c(Y1 = tab$T1[i], Y2 = tab$T2[i], other_roles))
  names(by_item) <- items

  list(by_item = by_item, table = tab, other = other)
}
