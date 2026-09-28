# print / summary / plot methods for fancyStability objects (stabilityPaths()).

# Pathway columns in display order: residual, mediated, confounded, total.
pathOrder <- function(paths) {
  one <- paths[paths$item == paths$item[1], ]
  one$path[order(match(one$type, c("residual", "mediated", "cross-lagged",
                                   "confounded", "total")))]
}

# One row per item, one "estimate (share%)" cell per pathway, from a $paths
# table holding a single decomposition per item.
decompCells <- function(p, digits) {
  cols <- pathOrder(p)
  items <- unique(p$item)
  cell <- function(row) {
    if (!nrow(row) || is.na(row$est)) return("--")
    if (row$type == "total") return(fmtNum(row$est, digits))
    sprintf("%s (%s%%)", fmtNum(row$est, digits), fmtNum(100 * row$share, 0))
  }
  tab <- do.call(rbind, lapply(items, function(it) {
    vapply(cols, function(cn) cell(p[p$item == it & p$path == cn, ][1, ]),
           character(1))
  }))
  matrix(tab, nrow = length(items), dimnames = list(items, pathLabel(cols)))
}

# round first, so a tiny negative prints as 0.00 rather than -0.00
fmtNum <- function(v, dg) formatC(round(v, dg) + 0, digits = dg, format = "f")

# Admissibility flag appended to item names in printed tables.
admFlag <- function(adm) ifelse(is.na(adm), " ?", ifelse(adm, "", " !"))

pathLabel <- function(path) sub("^via_", "via ", path)

# The structural coefficients behind each item's decomposition, labelled by
# role: selection (X ~ Y1), change (Y2 ~ X), stability (Y2 ~ Y1), and control
# (Y2 ~ C). One row per item per coefficient, with inference.
effectTable <- function(x) {
  s  <- x$settings
  cf <- x$coefficients[x$coefficients$op == "~", ]
  y1 <- paste0(cf$item, s$suffixes[1])
  y2 <- paste0(cf$item, s$suffixes[2])
  effect <- ifelse(cf$lhs %in% s$X & cf$rhs == y1, "selection",
            ifelse(cf$lhs == y2 & cf$rhs %in% s$X, "change",
            ifelse(cf$lhs == y2 & cf$rhs == y1, "stability",
            ifelse(cf$lhs == y2 & cf$rhs %in% s$controls, "control", NA))))
  keep <- !is.na(effect)
  cf <- cf[keep, ]; effect <- effect[keep]
  via <- ifelse(effect == "selection", cf$lhs, ifelse(effect == "stability", NA, cf$rhs))
  path <- ifelse(effect == "selection", paste(cf$lhs, "~ Y1"),
          ifelse(effect == "stability", "Y2 ~ Y1", paste("Y2 ~", cf$rhs)))
  out <- data.frame(item = cf$item, effect = effect, via = via, path = path,
                    est = cf$est, se = cf$se, pvalue = cf$pvalue,
                    ci.lower = cf$ci.lower, ci.upper = cf$ci.upper,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

# Combine sets of pathways (e.g. one per dummy code of a factor) into a single
# pathway per item, for display. Pathways are additive, so the pooled estimate
# and share are sums and the parts still add to the total. Inference columns
# (se, CI, p) are left NA: a sum of pathways has no single test here. Works on
# a fancyStability $paths table or a reliabilitySensitivity() result, where
# pooling is done separately at each reliability.
poolPaths <- function(p, pool) {
  if (is.null(pool) || !length(pool)) return(p)
  if (!is.list(pool) || is.null(names(pool)) || any(!nzchar(names(pool))))
    stop("`pool` must be a named list of variable sets, ",
         "e.g. list(organization = c(\"orgB\", \"orgC\")).", call. = FALSE)
  for (nm in names(pool)) {
    members <- paste0("via_", pool[[nm]])
    unknown <- setdiff(members, p$path)
    if (length(unknown))
      stop("`pool$", nm, "` names variables with no pathway in `x`: ",
           paste(sub("^via_", "", unknown), collapse = ", "), call. = FALSE)
    hit <- p$path %in% members
    keep <- intersect(c("item", "outcome", "type", "reliability", "admissible"),
                      names(p))
    key <- do.call(paste, p[intersect(c("item", "outcome", "reliability"), names(p))])
    pooled <- do.call(rbind, lapply(unique(key[hit]), function(k) {
      r <- p[hit & key == k, ]
      o <- r[1, ]
      o[setdiff(names(o), keep)] <- NA
      o$path <- paste0("via_", nm)
      if ("via" %in% names(o)) o$via <- nm
      o$est <- sum(r$est); o$share <- sum(r$share)
      o
    }))
    p <- rbind(p[!hit, ], pooled)
  }
  p
}

settingsLine <- function(x) {
  s <- x$settings
  c(sprintf("  mediators (X): %s", if (length(s$X)) paste(s$X, collapse = ", ") else "none"),
    sprintf("  controls:      %s", if (length(s$controls)) paste(s$controls, collapse = ", ") else "none"),
    sprintf("  latent:        %s", relText(s)),
    sprintf("  metric:        %s   missing: %s",
            if (s$metric == "std") "standardized" else "raw", s$missing))
}

# Which variables were modelled as latent, and at what reliability.
relText <- function(s) {
  rel <- s$reliability$table
  latent <- !is.na(rel$T1) | !is.na(rel$T2)
  rel_txt <- if (!any(latent)) "none (observed items)" else {
    r <- range(c(rel$T1, rel$T2), na.rm = TRUE)
    sprintf("%d of %d items latent (reliability %s)", sum(latent), nrow(rel),
            if (r[1] == r[2]) sprintf("%.2f", r[1])
            else sprintf("%.2f-%.2f", r[1], r[2]))
  }
  if (length(s$reliability$other))
    rel_txt <- paste0(rel_txt, "; also ",
                      paste(sprintf("%s = %.2f", names(s$reliability$other),
                                    s$reliability$other), collapse = ", "))
  rel_txt
}

#' @param x,object A \code{fancyStability} object from
#'   \code{\link{stabilityPaths}}.
#' @param digits Number of decimal places to print.
#' @param pool Optional named list of variable sets whose pathways should be
#'   shown combined, e.g. \code{list(organization = c("orgB", "orgC"))} to
#'   show one \code{via organization} column instead of one per dummy code.
#'   Display only: the model, its degrees of freedom, and \code{summary()} are
#'   unaffected. Pathways are additive, so the combined estimate and share are
#'   sums, and the parts still add to the total.
#' @param ... Further arguments; for \code{plot()} with \code{item}, passed to
#'   the path-diagram options below.
#' @rdname stabilityPaths-methods
#' @name stabilityPaths-methods
#' @title Print, Summarize, and Plot a Stability Decomposition
#' @description
#' \code{print()} shows each item's decomposition in one line: every pathway's
#' estimate with its share of the total in parentheses. \code{summary()} adds
#' standard errors, confidence intervals and p-values for every pathway, and
#' the selection (\code{X ~ Y1}) and socialization (\code{Y2 ~ X}) paths
#' whose product forms each mediated pathway. \code{plot()} compares items, or
#' draws one item's path diagram; see \code{\link{plot.fancyStability}}.
#' @return \code{print()} returns \code{x} invisibly; \code{summary()} returns
#'   an object of class \code{summary.fancyStability} with components
#'   \code{paths} (long table with inference) and \code{structural} (one row
#'   per item).
#' @export
print.fancyStability <- function(x, digits = 2, pool = NULL, ...) {
  p <- poolPaths(x$paths, pool)
  items <- unique(p$item)
  tab <- decompCells(p, digits)
  rownames(tab) <- NULL

  s <- x$summary
  flag <- admFlag(s$admissible)
  out <- data.frame(item = paste0(s$item, flag), n = s$n, tab,
                    check.names = FALSE, stringsAsFactors = FALSE)

  cat("<fancyStability> stability decomposition of", length(items),
      if (length(items) == 1) "item\n" else "items\n")
  cat(settingsLine(x), sep = "\n")
  cat("\n  estimate (share of total stability)\n")
  print(out, row.names = FALSE, right = FALSE)
  if (any(flag == " !"))
    cat("\n  ! inadmissible solution (e.g. adjusted stability > 1); see $status.",
        "\n    The reliability supplied is probably too low for that item.\n")
  if (any(flag == " ?"))
    cat("\n  ? model not estimated; see $status.\n")
  invisible(x)
}

#' @rdname stabilityPaths-methods
#' @export
summary.fancyStability <- function(object, ...) {
  p <- object$paths
  paths <- data.frame(item = p$item, path = pathLabel(p$path), type = p$type,
                      est = p$est, se = p$se, ci.lower = p$ci.lower,
                      ci.upper = p$ci.upper, pvalue = p$pvalue, share = p$share,
                      stringsAsFactors = FALSE)

  s <- object$summary
  X <- object$settings$X
  pick <- function(col) if (col %in% names(s)) s[[col]] else rep(NA_real_, nrow(s))
  structural <- data.frame(item = s$item, n = s$n, r_obs = s$r_obs,
                           stringsAsFactors = FALSE)
  for (v in X) {
    structural[[paste0(v, " ~ Y1")]] <- pick(paste0(v, "_on_Y1"))   # selection
    structural[[paste0("Y2 ~ ", v)]] <- pick(paste0("Y2_on_", v))   # socialization
  }
  structural[["Y2 ~ Y1"]] <- pick("Y2_on_Y1")                       # residual

  structure(list(paths = paths, structural = structural,
                 settings = object$settings, status = object$status,
                 header = settingsLine(object)),
            class = "summary.fancyStability")
}

#' @rdname stabilityPaths-methods
#' @export
print.summary.fancyStability <- function(x, digits = 3, ...) {
  cat("<fancyStability summary>\n")
  cat(x$header, sep = "\n")
  cat("\nPathways (est, 95% CI, p, share of total):\n")
  p <- x$paths
  num <- c("est", "se", "ci.lower", "ci.upper", "share")
  p[num] <- lapply(p[num], round, digits)
  p$pvalue <- format.pval(p$pvalue, digits = 2, eps = .001)
  print(p, row.names = FALSE)
  cat("\nStructural paths behind the decomposition:",
      "\n  selection (X ~ Y1) x socialization (Y2 ~ X) = mediated pathway;",
      "residual stability is Y2 ~ Y1\n")
  st <- x$structural
  nm <- setdiff(names(st), c("item", "n"))
  st[nm] <- lapply(st[nm], round, digits)
  print(st, row.names = FALSE)
  bad <- x$status[!x$status$status %in% "Success", , drop = FALSE]
  if (nrow(bad)) {
    cat("\nItems with problems:\n")
    print(bad, row.names = FALSE)
  }
  invisible(x)
}

#' Plot a Stability Decomposition
#' @description
#' With no \code{item}, draws one horizontal bar per item, divided into its
#' pathways: residual stability in grey, mediated pathways in blues, confounded
#' pathways in oranges. Positive parts stack rightward from zero and negative
#' parts leftward, and a black tick marks each item's total. With \code{item},
#' draws that item's path diagram instead. With \code{type = "effects"}, draws
#' a scatterplot of every item's selection effect (\code{X ~ Y1}, horizontal)
#' against its change effect (\code{Y2 ~ X}, vertical), labelled by item.
#'
#' @section Selection and change effects:
#' In the effects scatterplot, items in the upper-right and lower-left
#' quadrants have selection and change effects of the same sign: the
#' experience is more common among people high (or low) on the item, and
#' pushes the item further in that direction, i.e. the relationship is
#' \emph{corresponsive} (Roberts, Caspi, & Moffitt, 2003). Items in the other
#' two quadrants have \emph{anti-corresponsive} effects, and items near an axis
#' have one effect without the other. A point's fill shows which of its
#' effects reached p < .05: black for both, orange for selection only, blue for
#' change only, and white for neither. Labels are placed with \pkg{ggrepel}
#' when it is installed. By default both axes share one range centred on zero,
#' with equal scaling (\code{same_range = TRUE}). To compare two analyses
#' (e.g. with and without a reliability adjustment), pass both plots the same
#' \code{xlim} and \code{ylim}, and the same list of fits as \code{bands}.
#'
#' @section Significance bands:
#' An estimate is significant at .05 when it is more than 1.96 standard errors
#' from zero, so each item has its own critical value, \eqn{1.96 \times SE}.
#' The shaded bands summarize these across items, separately for each axis:
#' \itemize{
#'   \item the \strong{darker band} runs to the smallest critical value: an
#'     estimate inside it would not be significant for any item;
#'   \item the \strong{lighter band} runs to the largest: an estimate inside it
#'     would be significant for some items but not others;
#'   \item beyond both, an estimate would be significant for every item.
#' }
#' Thin grey lines mark the band edges, and solid black lines mark zero on
#' each axis.
#' The bands differ between the axes when one kind of effect is estimated more
#' precisely than the other, e.g. because more people contribute to it.
#'
#' @references
#' Roberts, B. W., Caspi, A., & Moffitt, T. E. (2003). Work experiences and
#' personality development in young adulthood. \emph{Journal of Personality
#' and Social Psychology, 84}(3), 582--593.
#'
#' @param x A \code{fancyStability} object from \code{\link{stabilityPaths}}.
#' @param item Optional item name. If given, draw its path diagram.
#' @param what For the bar chart: \code{"est"} (default) plots the estimates,
#'   which add up to total stability; \code{"share"} plots each pathway's
#'   proportion of the total.
#' @param sort Logical. Sort the bar chart by total stability? Default
#'   \code{FALSE} keeps item order.
#' @param pool For the bar chart: optional named list of variable sets whose
#'   pathways are drawn as one segment, as in \code{print()}; see
#'   \code{\link{stabilityPaths-methods}}.
#' @param type \code{"bars"} (default) for the bar chart, or \code{"effects"}
#'   for the selection-versus-change scatterplot (ignored when \code{item} is
#'   given).
#' @param X For \code{type = "effects"}: which mediator's effects to plot.
#'   Defaults to the first \code{X}.
#' @param labels For \code{type = "effects"}: point labels, either a character
#'   vector (one per item, in item order) or a function applied to the item
#'   names, e.g. \code{function(i) sub("_role$", "", i)}. Defaults to the item
#'   names.
#' @param ... For the path diagram, any of:
#'   \describe{
#'     \item{\code{item_label}}{Name written in the Y1 and Y2 nodes; defaults
#'       to \code{item}.}
#'     \item{\code{x_label}, \code{control_labels}}{Display names for the
#'       mediators and controls, in model order.}
#'     \item{\code{show_controls}}{\code{FALSE} omits the controls from the
#'       drawing (the estimates are still control-adjusted).}
#'     \item{\code{suppress_control_cov}}{\code{TRUE} drops covariance arcs not
#'       involving Y1, to unclutter.}
#'     \item{\code{show_estimates}, \code{show_pvalues}, \code{digits}}{What
#'       numbers to write on the paths, and how.}
#'     \item{\code{label_cex}, \code{title}}{Node text size; plot title.}
#'   }
#'   Latent variables are drawn as ellipses labelled with their reliability.
#'   For the effects scatterplot: \code{xlim} and \code{ylim} (axis ranges),
#'   \code{title}, \code{bands} (\code{TRUE}, \code{FALSE}, or a list of fits
#'   to compute the significance bands from) and \code{same_range}; see the
#'   sections above. The bar chart ignores \code{...}.
#'
#' @return The bar chart and effects scatterplot return \pkg{ggplot2} objects,
#'   which print as the plot and can be modified with \code{+} (e.g. a theme
#'   or title). The scatterplot's data, each item's selection and change
#'   effects with p-values, are in its \code{$data}. The path diagram returns
#'   the \code{qgraph} object invisibly.
#' @seealso \code{\link{stabilityPaths}}
#' @examples
#' roles <- c("Powerful_role", "Persuasive_role", "Shy_role")
#' d <- stabilityData(powerTraits$T1, powerTraits$T2, powerTraits$people,
#'                    commonItems = c("power", roles))
#' orgs <- paste0("org", LETTERS[2:7])
#' d[orgs] <- lapply(LETTERS[2:7], function(o) as.numeric(d$org == o))
#' sp <- stabilityPaths(d, items = roles, X = "power[T1]",
#'                      controls = c("tenure", orgs))
#' plot(sp, pool = list(organization = orgs))
#' plot(sp, what = "share", pool = list(organization = orgs))
#' plot(sp, item = "Powerful_role", show_controls = FALSE)
#' plot(sp, type = "effects", labels = function(i) sub("_role$", "", i))
#' @export
#' @importFrom graphics plot rect segments abline axis legend par text
#' @importFrom grDevices hcl.colors
#' @importFrom ggplot2 .data
plot.fancyStability <- function(x, item = NULL, what = c("est", "share"),
                                sort = FALSE, pool = NULL,
                                type = c("bars", "effects"), X = NULL,
                                labels = NULL, ...) {
  type <- match.arg(type)
  if (is.null(item) && type == "effects")
    return(plotEffects(x, X = X, labels = labels, ...))
  if (!is.null(item)) {
    if (!item %in% names(x$fits))
      stop("No item named \"", item, "\". Items: ",
           paste(names(x$fits), collapse = ", "))
    args <- list(...)
    if (is.null(args$item_label)) args$item_label <- item
    return(do.call(plotMedX, c(list(sp = x$fits[[item]]), args)))
  }

  decompBars(poolPaths(x$paths, pool), adm = x$summary[c("item", "admissible")],
             what = match.arg(what), sort = sort, metric = x$settings$metric)
}

# Stacked decomposition bars, one per item. With an `outcome` column in `p`
# (a cross-lag fit), one panel per outcome. `adm` has columns item, admissible.
decompBars <- function(p, adm, what, sort, metric) {
  facet <- "outcome" %in% names(p)
  if (!facet) p$outcome <- ""
  one <- p[p$item == p$item[1], ]
  one <- one[order(match(one$type, c("residual", "mediated", "cross-lagged",
                                     "confounded", "total"))), ]
  parts <- unique(one$path[one$type != "total"])
  types <- one$type[match(parts, one$path)]
  items <- unique(p$item)

  tots <- p[p$type == "total", c("item", "outcome", "est")]
  if (what == "share") tots$est <- 1
  # first item at the top unless sorted by total stability (of the first outcome)
  first <- tots[tots$outcome == tots$outcome[1], ]
  ord <- if (sort) first$item[order(first$est)] else rev(items)

  n_med <- sum(types %in% c("mediated", "cross-lagged"))
  n_conf <- sum(types == "confounded")
  ramp <- function(from, to, k) if (k) grDevices::colorRampPalette(c(from, to))(k)
  pal <- c("grey75",
           ramp("#2F6DB5", "#9DC3EA", n_med),    # mediated / cross-lagged: blues
           ramp("#D9731E", "#F5C28F", n_conf))   # confounded: oranges
  names(pal) <- pathLabel(parts)

  a <- adm$admissible[match(items, adm$item)]
  lbl <- stats::setNames(paste0(items, ifelse(!is.na(a) & !a, " !", "")), items)

  bars <- p[p$path %in% parts, ]
  bars$value <- if (what == "share") bars$share else bars$est
  bars$path  <- factor(pathLabel(bars$path), levels = pathLabel(parts))
  bars$item  <- factor(bars$item, levels = ord)
  totals <- data.frame(item = factor(tots$item, levels = ord), value = tots$est,
                       outcome = tots$outcome, mark = "total")
  if (facet) {
    lv <- unique(p$outcome)
    strip <- function(o) ifelse(o == "item", "stability of item",
                                paste("stability of", o))
    bars$outcome   <- factor(strip(bars$outcome), levels = strip(lv))
    totals$outcome <- factor(strip(totals$outcome), levels = strip(lv))
  }

  g <- ggplot2::ggplot(bars, ggplot2::aes(x = .data$value, y = .data$item)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
    ggplot2::geom_col(ggplot2::aes(fill = .data$path), width = .7,
                      colour = "white", linewidth = .3, na.rm = TRUE,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_point(data = totals, ggplot2::aes(shape = .data$mark),
                        size = 7, colour = "black", na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::scale_shape_manual(values = c(total = "|"), name = NULL) +
    ggplot2::scale_y_discrete(labels = lbl) +
    ggplot2::labs(x = if (what == "share") "share of total stability"
                      else sprintf("stability (%s)",
                                   if (metric == "std") "standardized" else "raw"),
                  y = NULL) +
    fancyTheme() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.y = ggplot2::element_blank())
  if (facet) g <- g + ggplot2::facet_wrap(~ outcome, nrow = 1)
  g
}

# Shared look for the package's ggplot graphics.
fancyTheme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "grey80", fill = NA),
                   strip.text = ggplot2::element_text(face = "bold"),
                   plot.title = ggplot2::element_text(face = "bold"))
}

# One row per item: its selection and change effects with SEs and p-values,
# and admissibility. For a stabilityPaths() fit selection is X ~ Y1 and change
# Y2 ~ X; for a crossLagPaths() fit, X2 ~ Y1 and Y2 ~ X1.
selChgTable <- function(x, X = NULL) {
  s <- x$settings
  if (inherits(x, "fancyCrossLag")) {
    e <- x$effects
    sel <- e[e$effect == "selection", ]
    chg <- e[e$effect == "change", ]
  } else {
    if (!length(s$X))
      stop("`x` has no mediator (X); there are no selection or change effects to plot.")
    if (is.null(X)) X <- s$X[1]
    if (!X %in% s$X) stop("`X` must be one of: ", paste(s$X, collapse = ", "))
    e <- effectTable(x)
    sel <- e[e$effect == "selection" & e$via == X, ]
    chg <- e[e$effect == "change" & e$via == X, ]
  }
  items <- s$items
  m <- function(tab, col) tab[[col]][match(items, tab$item)]
  data.frame(item = items,
             selection = m(sel, "est"), selection_se = m(sel, "se"),
             selection_p = m(sel, "pvalue"),
             change = m(chg, "est"), change_se = m(chg, "se"),
             change_p = m(chg, "pvalue"),
             admissible = x$status$admissible[match(items, x$status$item)],
             stringsAsFactors = FALSE)
}

# Critical values for the significance bands: an estimate is significant at
# .05 when |b| > 1.96 * SE, so each item has its own threshold. `inner` is
# the smallest threshold (below it, no item's estimate would be significant)
# and `outer` the largest (beyond it, every item's would be).
sigBands <- function(tabs) {
  tab <- do.call(rbind, tabs)
  tab <- tab[is.na(tab$admissible) | tab$admissible, ]
  z <- stats::qnorm(.975)
  crit <- function(se) { se <- se[!is.na(se)]
    if (length(se)) z * range(se) else c(NA_real_, NA_real_) }
  list(selection = stats::setNames(crit(tab$selection_se), c("inner", "outer")),
       change    = stats::setNames(crit(tab$change_se), c("inner", "outer")))
}

# Selection (X ~ Y1) vs change (Y2 ~ X) scatterplot, one labelled point per
# item. Behind plot(x, type = "effects").
plotEffects <- function(x, X = NULL, labels = NULL, xlim = NULL, ylim = NULL,
                        title = NULL, bands = TRUE, same_range = TRUE, ...) {
  X <- if (is.null(X)) x$settings$X[1] else X
  effectsScatter(selChgTable(x, X), labels = labels, xlim = xlim, ylim = ylim,
                 title = title, bands = bands, same_range = same_range,
                 xlab = paste0("Selection effects (", X, " ~ Y1)"),
                 ylab = paste0("Change effects (Y2 ~ ", X, ")"))
}

# The scatterplot itself, from a selChgTable(). Shared by the stabilityPaths
# and crossLagPaths plot methods, which differ only in which effects they pass.
# `bands`: TRUE (from the items plotted), FALSE, or a list of fits whose items
# all contribute, so several plots can share the same bands.
effectsScatter <- function(out, labels, xlim, ylim, title, xlab, ylab,
                           bands = TRUE, same_range = TRUE) {
  items <- out$item
  adm <- out$admissible
  lab <- if (is.null(labels)) items
         else if (is.function(labels)) labels(items)
         else as.character(labels)
  if (length(lab) != length(items)) stop("`labels` must give one label per item.")

  show <- !is.na(out$selection) & !is.na(out$change) & (is.na(adm) | adm)
  if (any(!show))
    message("Not plotted (not estimated or inadmissible): ",
            paste(items[!show], collapse = ", "))

  out$label <- lab
  d <- out[show, ]

  bd <- if (isTRUE(bands)) sigBands(list(d))
        else if (is.list(bands)) sigBands(lapply(bands, selChgTable))
        else NULL

  # one key for significance: which of the two effects reached p < .05
  selSig <- !is.na(d$selection_p) & d$selection_p < .05
  chgSig <- !is.na(d$change_p) & d$change_p < .05
  sig_lv <- c("both", "selection only", "change only", "neither")
  d$sig <- factor(ifelse(selSig & chgSig, "both", ifelse(selSig, "selection only",
                  ifelse(chgSig, "change only", "neither"))), levels = sig_lv)
  fills <- c(both = "black", `selection only` = "#D9731E",
             `change only` = "#2F6DB5", neither = "white")
  lines <- c(both = "black", `selection only` = "black",
             `change only` = "black", neither = "grey45")

  if (same_range && is.null(xlim) && is.null(ylim)) {
    lim <- max(abs(c(d$selection, d$change, unlist(bd))), na.rm = TRUE) * 1.08
    xlim <- ylim <- c(-lim, lim)
  }

  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$selection, y = .data$change))
  if (!is.null(bd)) {
    band <- function(which, level, fill) {
      w <- bd[[which]][[level]]
      if (is.na(w)) return(NULL)
      if (which == "selection")
        ggplot2::annotate("rect", xmin = -w, xmax = w, ymin = -Inf, ymax = Inf, fill = fill)
      else
        ggplot2::annotate("rect", ymin = -w, ymax = w, xmin = -Inf, xmax = Inf, fill = fill)
    }
    light <- "grey92"; medium <- "grey80"
    g <- g + band("selection", "outer", light) + band("change", "outer", light) +
      band("selection", "inner", medium) + band("change", "inner", medium)
    # thin edges at each critical value, drawn over both fills so every edge
    # shows, including where a band crosses the other axis's band
    edge <- function(which) {
      w <- stats::na.omit(unname(unlist(bd[[which]])))
      if (!length(w)) return(NULL)
      at <- c(-w, w)
      if (which == "selection")
        ggplot2::geom_vline(xintercept = at, colour = "grey55", linewidth = .25)
      else
        ggplot2::geom_hline(yintercept = at, colour = "grey55", linewidth = .25)
    }
    g <- g + edge("selection") + edge("change")
  }
  g <- g +
    ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = .5) +
    ggplot2::geom_vline(xintercept = 0, colour = "black", linewidth = .5) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$sig, colour = .data$sig),
                        shape = 21, size = 2.6, stroke = .8, show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = fills, limits = sig_lv, drop = FALSE,
                               name = "p < .05:") +
    ggplot2::scale_colour_manual(values = lines, limits = sig_lv, drop = FALSE,
                                 name = "p < .05:")
  g <- g + if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$label), size = 3,
                             max.overlaps = Inf, seed = 1, min.segment.length = .3,
                             segment.colour = "grey70", segment.size = .3,
                             box.padding = .25, point.padding = .15)
  } else {
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3, vjust = -.8)
  }
  caption <- if (!is.null(bd))
    "Shading: dark, not significant for any item; light, significant for some items (depends on the item's SE)."
  coord <- if (same_range) ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE)
           else ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  g + coord +
    ggplot2::labs(x = xlab, y = ylab, title = title, caption = caption) +
    fancyTheme() +
    ggplot2::theme(legend.position = "bottom", panel.grid = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(colour = "grey35", hjust = 0))
}
