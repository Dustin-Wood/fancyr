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
#' Optionally, items (and continuous experiences or controls) can be adjusted
#' for their retest reliability by supplying \code{reliability}, which models
#' each as a latent variable: the person's expected score over a chosen
#' interval (see "Choosing a reliability"). A single Time 1 answer otherwise
#' controls for that expected score only partly, and the remainder inflates the
#' apparent effect of the experience and the share of stability it seems to
#' carry.
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
#' stability of the expected scores, i.e. the retest correlation adjusted for
#' the items' reliabilities.
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
#' Standard errors treat the reliability as known, and results can depend
#' heavily on it: use \code{\link{reliabilitySensitivity}} to see how much. Do
#' not supply a reliability for a binary experience; a 0/1 record of whether
#' something happened is the quantity of interest itself, not one answer
#' sampled from a range of answers a person might give.
#'
#' If a supplied reliability is lower than the retest correlation implies is
#' possible, the adjusted stability exceeds 1. That item is still reported,
#' but flagged \code{admissible = FALSE}.
#'
#' @section Choosing a reliability:
#' An item modelled with a \code{reliability} stands for the person's
#' \emph{expected score} on that item over some interval \eqn{t}: roughly, the
#' average of the answers they would give if asked repeatedly across that span.
#' Nothing here assumes this expected score is fixed. It can change between
#' \code{Y1} and \code{Y2}, which is the whole subject of the model. The
#' appropriate reliability is the share of variance in a single answer that
#' is shared with the expected score over \eqn{t}.
#'
#' A \strong{retest correlation over an interval of about \eqn{t}} estimates
#' exactly this (Wood et al., 2023). Whatever contributes to both answers --
#' whatever persists over \eqn{t} -- is part of the expected score. Influences
#' that come and go within \eqn{t}, such as mood, the day's circumstances, or
#' memory of having just answered, still contribute systematically to each
#' answer. They are simply not part of the expected score over \eqn{t}. As
#' \eqn{t} lengthens from an hour to a day, a week, a month or a year, more of
#' the influences behind an answer have the chance to change, and the retest
#' correlation falls (Watson, 2004).
#'
#' The choice of \eqn{t} therefore decides what the adjusted estimates
#' describe:
#' \itemize{
#'   \item \strong{Short} (the same session, or a few days to a week): nearly
#'     everything behind an answer persists, so the reliability is high and the
#'     adjustment small. The adjusted stability then describes scores that
#'     still carry the circumstances of the particular occasion.
#'   \item \strong{Long} (approaching the interval between \code{Y1} and
#'     \code{Y2}): the change the model is meant to study is folded into the
#'     reliability, and the adjustment does too much. In the limit, a
#'     reliability equal to the \code{Y1}-\code{Y2} retest correlation makes
#'     the adjusted stability 1 by construction.
#'   \item \strong{In between}: long enough that what is specific to an
#'     occasion has turned over, and short relative to the interval being
#'     studied. For waves a year apart, two- to four-week retest correlations
#'     are a reasonable target.
#' }
#' Internal consistency (e.g. alpha) fits the same logic, since its items are
#' answered at different moments too, typically about a minute apart. Alpha is
#' often described as underestimating reliability. What it underestimates is
#' the retest reliability of those items \emph{over that short interval}. If two
#' items answered about a minute apart give an alpha of .80, each item's own
#' retest correlation over a minute (averaged, e.g. as a geometric mean) should
#' be higher than .80. Over longer intervals such as two weeks, observed retest
#' correlations are regularly \emph{lower} than alpha (McCrae et al., 2011;
#' Chmielewski & Watson, 2009). So alpha is not a suitable value for waves
#' months or years apart, and a single item has no alpha at all.
#'
#' Suitable retest values are often unavailable. Published retest correlations
#' for similar items or measures can give a starting range. Whatever the
#' source, check the conclusions across the plausible range with
#' \code{\link{reliabilitySensitivity}}. The observed \code{Y1}-\code{Y2}
#' retest correlation is a floor: a reliability below it is inadmissible.
#'
#' @references
#' Chmielewski, M., & Watson, D. (2009). What is being assessed and why it
#' matters: The impact of transient error on trait research. \emph{Journal of
#' Personality and Social Psychology, 97}(1), 186--202.
#'
#' McCrae, R. R., Kurtz, J. E., Yamagata, S., & Terracciano, A. (2011).
#' Internal consistency, retest reliability, and their implications for
#' personality scale validity. \emph{Personality and Social Psychology
#' Review, 15}(1), 28--50.
#'
#' Watson, D. (2004). Stability versus change, dependability versus error:
#' Issues in the assessment of personality over time. \emph{Journal of
#' Research in Personality, 38}(4), 319--350.
#'
#' Wood, D., Lowman, G. H., Armstrong, B. F., III, & Harms, P. D. (2023).
#' Using retest-adjusted correlations as indicators of the semantic similarity
#' of items. \emph{Journal of Personality and Social Psychology, 125}(2),
#' 437--454. \doi{10.1037/pspp0000441}
#'
#' @inheritSection stabilityData Missing data
#'
#' @param data A data frame with one row per person, typically from
#'   \code{\link{stabilityData}}, holding item columns named with the
#'   \code{suffixes} (e.g. \code{"dominant[T1]"}, \code{"dominant[T2]"}) plus
#'   any \code{X} and \code{controls} columns.
#' @param items Character vector of item base names. Defaults to the
#'   \code{"commonItems"} attribute set by \code{\link{stabilityData}}. A
#'   single item is fine.
#' @param X Character vector of experience (mediator) variable names, or
#'   \code{NULL} (default) to decompose stability into confounded and residual
#'   parts only. Several are fitted as parallel mediators.
#' @param controls Character vector of control variable names, or \code{NULL}
#'   (default).
#' @param reliability Optional reliabilities (ideally retest correlations) for
#'   latent-variable adjustment; see the Reliability and Choosing a reliability
#'   sections. \code{NULL} (default) analyses every variable as observed.
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
#' # How much of the stability of two role identities runs through power?
#' d <- stabilityData(powerTraits$T1, powerTraits$T2, powerTraits$people,
#'                    commonItems = c("power", "Powerful_role", "Shy_role"))
#'
#' # observed items
#' sp <- stabilityPaths(d, items = c("Powerful_role", "Shy_role"),
#'                      X = "power[T1]", controls = "tenure")
#' sp
#'
#' # adjusted for retest reliability (assumed .75 for the items; .93 for
#' # power, an average over many raters)
#' spL <- stabilityPaths(d, items = c("Powerful_role", "Shy_role"),
#'                       X = "power[T1]", controls = "tenure",
#'                       reliability = c(Powerful_role = .75, Shy_role = .75,
#'                                       "power[T1]" = .93))
#' spL
#' summary(spL)
#' plot(spL)
#' plot(spL, item = "Powerful_role")
#'
#' @export
stabilityPaths <- function(data, items = attr(data, "commonItems"), X = NULL,
                           controls = NULL, reliability = NULL,
                           metric = c("std", "raw"),
                           suffixes = c("[T1]", "[T2]"), missing = "fiml") {

  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  metric <- match.arg(metric)
  if (is.null(items) || !length(items))
    stop("No `items` given, and `data` has no \"commonItems\" attribute ",
         "(set by stabilityData()).")
  items    <- as.character(items)
  X        <- if (is.null(X))        character(0) else as.character(X)
  controls <- if (is.null(controls)) character(0) else as.character(controls)
  if (length(suffixes) != 2L)
    stop("`suffixes` must give two suffixes: Time 1 then Time 2.")
  suffixes <- stats::setNames(as.character(suffixes), c("Y1", "Y2"))

  absent <- setdiff(c(X, controls), names(data))
  if (length(absent)) {
    hint <- if (any(absent %in% attr(data, "dropped")))
      paste0("\n  stabilityData() dropped them as one-wave columns; rebuild ",
             "`data` with keep = \"T1\" (or \"all\").") else ""
    stop("Column(s) not found in `data`: ", paste(absent, collapse = ", "), hint)
  }
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
