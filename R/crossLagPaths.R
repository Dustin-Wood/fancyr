#' Two-Wave Cross-Lagged Effects and Stability Decompositions
#' @description
#' For each item measured at two waves, fits a two-wave cross-lagged panel
#' model with an experience \code{X} that was also measured at both waves, and
#' reports:
#'
#' \itemize{
#'   \item \strong{Selection} (\code{X2 ~ Y1}): does the Time 1 item predict
#'     the experience at Time 2, controlling for the experience at Time 1?
#'     For example, do people high on a trait gain power over the interval?
#'   \item \strong{Change} (\code{Y2 ~ X1}): does the Time 1 experience predict
#'     the item at Time 2, controlling for the item at Time 1? For example,
#'     does having power move a trait?
#'   \item the \strong{stability} of each variable, the Time 1
#'     \strong{association} between them, and the Time 2 \strong{co-change}:
#'     the residual association of the Time 2 scores once everything at Time 1
#'     is accounted for.
#'   \item a decomposition of each variable's stability, as in
#'     \code{\link{stabilityPaths}}: a residual path, a \emph{cross-lagged}
#'     path (the Time 1 association times the change effect for the item, or
#'     times the selection effect for the experience), and a \emph{confounded}
#'     path for each control. The parts sum exactly to the total.
#' }
#'
#' Unlike the selection effect in \code{\link{stabilityPaths}}, which is the
#' concurrent association of the item with an experience, the selection effect
#' here is prospective: it is estimated net of where people already stood on
#' the experience at Time 1.
#'
#' @details
#' Controls predict both Time 2 variables and covary with both Time 1
#' variables, and the Time 2 residuals covary, so the model is just-identified
#' (df = 0); see \code{\link{crossLagModel}} for the syntax and the
#' decomposition formulas. The co-change covariance is not decomposed: it
#' collects what the Time 2 scores share beyond what the stability and
#' cross-lagged paths carry, such as change in both with a common cause, or an
#' effect of one on the other within the interval.
#'
#' In the default standardized metric, each variable's total stability is its
#' retest correlation (adjusted, if a reliability is given), the association
#' and co-change are correlations, and \code{share} is each pathway's
#' proportion of the total.
#'
#' @section Reliability:
#' As in \code{\link{stabilityPaths}}, any variable can be modelled as a latent
#' variable with a known reliability; by default every variable is analysed as
#' observed (reliability 1). \code{reliability} accepts:
#' \itemize{
#'   \item one number, used for every item at both waves (not for \code{X} or
#'     controls);
#'   \item a named vector, e.g. \code{c(communion = .8, power = .93)}. An item
#'     name sets that item at both waves; the \code{X} base name sets
#'     \code{X} at both waves; a column name (e.g. \code{"power[T2]"}) sets
#'     that one wave of \code{X}, or that control;
#'   \item a data frame with columns \code{item}, \code{T1} and \code{T2}, for
#'     values that differ between waves. A row for the \code{X} base name sets
#'     each wave of \code{X}.
#' }
#' See "Choosing a reliability" in \code{\link{stabilityPaths}}, and
#' \code{\link{reliabilitySensitivity}} to see how much the results depend on
#' the value chosen.
#'
#' @section Plots:
#' \code{plot()} on the result draws one of two graphics (details and all
#' options in \code{\link{crossLagPaths-methods}}):
#' \describe{
#'   \item{\code{plot(x)}}{The default: a scatterplot of every item's
#'     selection effect (\code{X2 ~ Y1}) against its change effect
#'     (\code{Y2 ~ X1}), with shaded bands where estimates can't be
#'     significant. \code{labels} shortens item names; \code{bands},
#'     \code{same_range}, \code{xlim}, \code{ylim} and \code{title} adjust
#'     it.}
#'   \item{\code{plot(x, type = "bars")}}{Both stability decompositions (the
#'     item's and \code{X}'s) as bars, side by side; \code{what},
#'     \code{sort} and \code{pool} as for \code{\link{plot.fancyStability}}.}
#' }
#' Both are \pkg{ggplot2} objects: add layers with \code{+}, save with
#' \code{ggplot2::ggsave()}, and find the plotted numbers in \code{$data}.
#' \code{\link{reliabilitySensitivity}} results have their own \code{plot()}.
#'
#' @inheritSection stabilityData Missing data
#'
#' @param data A data frame with one row per person, typically from
#'   \code{\link{stabilityData}}, holding each item's and \code{X}'s columns
#'   at both waves (named with the \code{suffixes}) plus any \code{controls}.
#' @param items Character vector of item base names. Defaults to the
#'   \code{"commonItems"} attribute set by \code{\link{stabilityData}}; the
#'   \code{X} variable is dropped from it.
#' @param X Base name of the experience, e.g. \code{"power"}, whose Time 1 and
#'   Time 2 columns are found with the \code{suffixes}. One variable.
#' @param controls Character vector of control variable names (Time 1 or
#'   stable variables), or \code{NULL} (default).
#' @param reliability Optional reliabilities; see the Reliability section.
#'   \code{NULL} (default) analyses every variable as observed.
#' @param metric \code{"std"} (default) for fully standardized estimates;
#'   \code{"raw"} for estimates in the variables' own units.
#' @param suffixes Length-2 character vector: the suffixes marking Time 1 and
#'   Time 2 columns. Defaults to \code{c("[T1]", "[T2]")}.
#' @param missing Missing-data method passed to \code{\link[lavaan]{sem}}.
#'   Defaults to \code{"fiml"}; \code{"listwise"} drops incomplete cases.
#' @param cores Number of CPU cores to spread the items over (default 1), or
#'   a cluster from \code{\link[parallel]{makeCluster}}; see
#'   \code{\link{modelOnAllY}}. Worth it for many items.
#' @param binary For a binary \code{X} or controls (exactly two distinct
#'   values, e.g. 0/1): \code{"sd"} (default) standardizes them like other
#'   variables; \code{"unit"} reports their standardized coefficients per 1
#'   vs 0; see \code{\link{stabilityPaths}}. The decompositions are the same
#'   either way.
#'
#' @return An object of class \code{fancyCrossLag}: a list with
#' \item{effects}{Long data frame, one row per item per effect: \code{item},
#'   \code{effect} (\code{"selection"}, \code{"change"}, \code{"stability"},
#'   \code{"association"}, \code{"co-change"} or \code{"control"}),
#'   \code{outcome} (the variable predicted, for regressions), \code{via},
#'   \code{path}, \code{est}, \code{se}, \code{pvalue}, \code{ci.lower},
#'   \code{ci.upper}.}
#' \item{paths}{Long data frame of both stability decompositions:
#'   \code{item}, \code{outcome} (\code{"item"} or the \code{X} name: whose
#'   stability is decomposed), \code{path}, \code{via}, \code{type}
#'   (\code{"residual"}, \code{"cross-lagged"}, \code{"confounded"} or
#'   \code{"total"}), \code{est}, \code{se}, \code{pvalue}, \code{ci.lower},
#'   \code{ci.upper}, and \code{share}.}
#' \item{coefficients}{Long data frame of all structural coefficients.}
#' \item{summary}{Wide data frame, one row per item: sample sizes, observed
#'   retest correlations of the item (\code{r_obs}) and of \code{X}
#'   (\code{r_obs_X}), reliabilities used, \code{admissible}, \code{status},
#'   then an estimate and \code{_p} column for every pathway and coefficient.}
#' \item{status}{Data frame of each item's fitting status and admissibility.}
#' \item{fits}{Named list of the per-item \code{\link{fitModel}} results.}
#' \item{settings}{The \code{items}, \code{X}, \code{Xcols}, \code{controls},
#'   resolved reliabilities, \code{metric}, \code{suffixes} and
#'   \code{missing} used.}
#' \item{data}{The columns of \code{data} the models used, kept so the
#'   analysis can be refitted by \code{\link{reliabilitySensitivity}}.}
#'
#' Print the object for a table of each item's effects and decompositions,
#' \code{summary()} it for standard errors and confidence intervals, and
#' \code{plot()} it for the selection-versus-change scatterplot or the
#' decomposition bars; see \code{\link{crossLagPaths-methods}}.
#'
#' @seealso \code{\link{crossLagModel}}, \code{\link{stabilityPaths}},
#'   \code{\link{reliabilitySensitivity}}
#'
#' @examples
#' d <- stabilityData(powerTraits$T1, powerTraits$T2, powerTraits$people,
#'                    commonItems = c("power", "communion", "agency"))
#'
#' # do communal or agentic people gain power, and does power change them?
#' cl <- crossLagPaths(d, items = c("communion", "agency"), X = "power",
#'                     controls = "tenure")
#' cl
#' summary(cl)
#'
#' # adjusted for retest reliability (assumed .8 for the profiles; .93 for
#' # power, an average over many raters)
#' clL <- crossLagPaths(d, items = c("communion", "agency"), X = "power",
#'                      controls = "tenure",
#'                      reliability = c(communion = .8, agency = .8, power = .93))
#' clL
#'
#' @export
crossLagPaths <- function(data, items = attr(data, "commonItems"), X,
                          controls = NULL, reliability = NULL,
                          metric = c("std", "raw"),
                          suffixes = c("[T1]", "[T2]"), missing = "fiml",
                          cores = 1, binary = c("sd", "unit")) {
  binary <- match.arg(binary)

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  metric <- match.arg(metric)
  if (base::missing(X) || length(X) != 1L || !nzchar(X))
    stop("`X` must be the base name of one variable measured at both waves, ",
         "e.g. X = \"power\".")
  X <- as.character(X)
  if (length(suffixes) != 2L)
    stop("`suffixes` must give two suffixes: Time 1 then Time 2.")
  suffixes <- stats::setNames(as.character(suffixes), c("Y1", "Y2"))
  Xcols <- paste0(X, suffixes)
  if (is.null(items) || !length(items))
    stop("No `items` given, and `data` has no \"commonItems\" attribute ",
         "(set by stabilityData()).")
  items <- setdiff(as.character(items), X)
  if (!length(items)) stop("No items left once `X` is removed from `items`.")
  controls <- if (is.null(controls)) character(0) else as.character(controls)

  absent <- setdiff(c(Xcols, controls), names(data))
  if (length(absent)) {
    hint <- if (any(absent %in% attr(data, "dropped")))
      paste0("\n  stabilityData() dropped them as one-wave columns; rebuild ",
             "`data` with keep = \"T1\" (or \"all\").") else ""
    stop("Column(s) not found in `data`: ", paste(absent, collapse = ", "), hint)
  }
  if (any(Xcols %in% controls))
    stop("`controls` includes a column of `X` (", X, "); the model already ",
         "includes X at both waves.")
  dup <- controls[duplicated(controls)]
  if (length(dup))
    stop("A control is named more than once: ", paste(unique(dup), collapse = ", "))
  cols_T1 <- paste0(items, suffixes[["Y1"]])
  cols_T2 <- paste0(items, suffixes[["Y2"]])
  has_cols <- cols_T1 %in% names(data) & cols_T2 %in% names(data)
  if (!any(has_cols))
    stop("None of the items has both a ", suffixes[["Y1"]], " and a ",
         suffixes[["Y2"]], " column in `data`.")

  rel <- resolveReliability(reliability, items, character(0), controls,
                            xWaves = list(base = X, cols = Xcols))

  spec <- crossLagModel(X = Xcols, controls = controls)
  spec$sem_args$missing <- missing

  res <- modelOnAllY(spec, data, items,
                     suffixes         = suffixes,
                     reliability      = rel$by_item,
                     metric           = metric,
                     return_estimates = TRUE,
                     cores            = cores,
                     binary           = binary)

  ## ---- descriptives --------------------------------------------------------
  retest <- function(a, b) {
    both <- !is.na(a) & !is.na(b)
    c(n_both = sum(both),
      r = if (sum(both) > 2) stats::cor(a[both], b[both]) else NA_real_)
  }
  x1 <- as.numeric(data[[Xcols[1]]]); x2 <- as.numeric(data[[Xcols[2]]])
  rX <- retest(x1, x2)[["r"]]
  desc <- do.call(rbind, lapply(seq_along(items), function(i) {
    if (!has_cols[i])
      return(data.frame(n_T1 = NA_integer_, n_T2 = NA_integer_,
                        n_both = NA_integer_, r_obs = NA_real_))
    y1 <- as.numeric(data[[cols_T1[i]]]); y2 <- as.numeric(data[[cols_T2[i]]])
    rt <- retest(y1, y2)
    data.frame(n_T1 = sum(!is.na(y1)), n_T2 = sum(!is.na(y2)),
               n_both = as.integer(rt[["n_both"]]), r_obs = rt[["r"]])
  }))

  s <- res$summary
  lead <- data.frame(item = s$item, n = s$n, desc, r_obs_X = rX,
                     rel_T1 = rel$table$T1, rel_T2 = rel$table$T2,
                     admissible = res$status$admissible, status = s$status,
                     stringsAsFactors = FALSE)
  summary_df <- cbind(lead, s[, setdiff(names(s), c("item", "n", "status")),
                              drop = FALSE])
  rownames(summary_df) <- NULL

  ## ---- decompositions, labelled by whose stability they decompose ----------
  paths <- res$paths
  paths$outcome <- ifelse(paths$outcome == "Y", "item", X)
  paths <- paths[, c("item", "outcome", setdiff(names(paths), c("item", "outcome")))]

  settings <- list(items = items, X = X, Xcols = Xcols, controls = controls,
                   reliability = rel, metric = metric, suffixes = suffixes,
                   missing = missing, cores = if (is.numeric(cores)) cores else 1L,
                   binary = binary)
  keep_cols <- intersect(c(attr(data, "id"), cols_T1, cols_T2, Xcols, controls),
                         names(data))

  structure(
    list(
      effects      = crossLagEffects(res$coefficients, settings),
      paths        = paths,
      coefficients = res$coefficients,
      summary      = summary_df,
      status       = res$status,
      fits         = res$modelEstimates,
      settings     = settings,
      data         = data[, keep_cols, drop = FALSE]
    ),
    class = "fancyCrossLag"
  )
}

# Label the structural coefficients of a cross-lag fit by role: selection
# (X2 ~ Y1), change (Y2 ~ X1), the two stabilities, the Time 1 association,
# Time 2 co-change, and each control's effect on either Time 2 variable.
crossLagEffects <- function(cf, s) {
  cols <- c("item", "effect", "outcome", "via", "path", "est", "se", "pvalue",
            "ci.lower", "ci.upper")
  if (is.null(cf) || !nrow(cf))
    return(stats::setNames(data.frame(matrix(nrow = 0, ncol = length(cols))), cols))
  y1 <- paste0(cf$item, s$suffixes[1]); y2 <- paste0(cf$item, s$suffixes[2])
  x1 <- s$Xcols[1]; x2 <- s$Xcols[2]
  reg <- cf$op == "~"
  effect <- ifelse(reg & cf$lhs == x2 & cf$rhs == y1, "selection",
            ifelse(reg & cf$lhs == y2 & cf$rhs == x1, "change",
            ifelse(reg & ((cf$lhs == y2 & cf$rhs == y1) | (cf$lhs == x2 & cf$rhs == x1)),
                   "stability",
            ifelse(reg & cf$lhs %in% c(y2, x2) & cf$rhs %in% s$controls, "control",
            ifelse(!reg & ((cf$lhs == y1 & cf$rhs == x1) | (cf$lhs == x1 & cf$rhs == y1)),
                   "association",
            ifelse(!reg & ((cf$lhs == y2 & cf$rhs == x2) | (cf$lhs == x2 & cf$rhs == y2)),
                   "co-change", NA))))))
  keep <- !is.na(effect)
  cf <- cf[keep, ]; effect <- effect[keep]; y1 <- y1[keep]; y2 <- y2[keep]
  # write the item's own columns by role, so a path reads the same for every item
  role <- function(v) ifelse(v == y1, "Y1", ifelse(v == y2, "Y2", v))
  lhs <- role(cf$lhs); rhs <- role(cf$rhs)
  outcome <- ifelse(!reg[keep], NA_character_,
                    ifelse(lhs == "Y2", "item", s$X))
  via <- ifelse(effect %in% c("control"), cf$rhs,
         ifelse(effect == "selection", "Y1",
         ifelse(effect == "change", x1, NA_character_)))
  # lavaan may list a covariance either way round (it differs between latent
  # and observed fits); write it item first so a path always reads the same
  path <- ifelse(effect == "association", paste("Y1 ~~", x1),
          ifelse(effect == "co-change", paste("Y2 ~~", x2),
                 paste(lhs, cf$op, rhs)))
  out <- data.frame(item = cf$item, effect = effect, outcome = outcome, via = via,
                    path = path,
                    est = cf$est, se = cf$se, pvalue = cf$pvalue,
                    ci.lower = cf$ci.lower, ci.upper = cf$ci.upper,
                    stringsAsFactors = FALSE)
  ord <- c("selection", "change", "stability", "association", "co-change", "control")
  out <- out[order(match(out$item, unique(out$item)), match(out$effect, ord)), ]
  rownames(out) <- NULL
  out
}
