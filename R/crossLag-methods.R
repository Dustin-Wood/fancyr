# print / summary / plot methods for fancyCrossLag objects (crossLagPaths()).

crossLagHeader <- function(x) {
  s <- x$settings
  c(sprintf("  X:        %s (%s)", s$X, paste(s$Xcols, collapse = ", ")),
    sprintf("  controls: %s", if (length(s$controls)) paste(s$controls, collapse = ", ") else "none"),
    sprintf("  latent:   %s", relText(s)),
    sprintf("  metric:   %s   missing: %s",
            if (s$metric == "std") "standardized" else "raw", s$missing),
    if (!is.null(b <- binaryText(x))) sprintf("  binary:   %s", b))
}

# The effects shown in print(), in order, with their column headings.
crossLagColumns <- function(s) {
  data.frame(
    effect  = c("selection", "change", "stability", "stability", "association",
                "co-change"),
    path    = c(paste(s$Xcols[2], "~ Y1"), paste("Y2 ~", s$Xcols[1]), "Y2 ~ Y1",
                paste(s$Xcols[2], "~", s$Xcols[1]), paste("Y1 ~~", s$Xcols[1]),
                paste("Y2 ~~", s$Xcols[2])),
    heading = c("selection", "change", "stability", paste(s$X, "stability"),
                "T1 assoc.", "co-change"),
    stringsAsFactors = FALSE)
}

#' @param x,object A \code{fancyCrossLag} object from
#'   \code{\link{crossLagPaths}}.
#' @param digits Number of decimal places to print.
#' @param pool Optional named list of control sets whose confounded pathways
#'   are shown combined, e.g. \code{list(organization = c("orgB", "orgC"))};
#'   see \code{\link{stabilityPaths-methods}}. Display only.
#' @param ... Further arguments; for \code{plot()}, \code{xlim}, \code{ylim}
#'   \code{title}, and \code{label_size} (text size of the point labels, in
#'   mm; default 3) for the effects scatterplot.
#' @rdname crossLagPaths-methods
#' @name crossLagPaths-methods
#' @title Print, Summarize, and Plot a Cross-Lagged Analysis
#' @description
#' \code{print()} shows each item's cross-lagged effects (selection, change),
#' the two stabilities, the Time 1 association and the Time 2 co-change, with
#' \code{*} marking p < .05; then each variable's stability decomposition,
#' with every pathway's share of the total in parentheses. \code{summary()}
#' adds standard errors, confidence intervals and p-values. \code{plot()}
#' draws the selection-versus-change scatterplot (\code{type = "effects"},
#' the default) or the decomposition bars (\code{type = "bars"}).
#' @return \code{print()} returns \code{x} invisibly; \code{summary()} returns
#'   an object of class \code{summary.fancyCrossLag} with components
#'   \code{effects} and \code{paths}; \code{plot()} returns a \pkg{ggplot2}
#'   object.
#' @examples
#' roles <- c("Powerful_role", "Persuasive_role", "Shy_role", "Warm_role")
#' d <- stabilityData(powerTraits$T1, powerTraits$T2, powerTraits$people,
#'                    commonItems = c("power", roles))
#' cl <- crossLagPaths(d, items = roles, X = "power", controls = "tenure")
#' cl
#' plot(cl, labels = function(i) sub("_role$", "", i))
#' plot(cl, type = "bars")
#' @export
print.fancyCrossLag <- function(x, digits = 2, pool = NULL, ...) {
  s <- x$settings
  e <- x$effects
  cc <- crossLagColumns(s)
  items <- s$items
  cell <- function(it, k) {
    r <- e[e$item == it & e$effect == cc$effect[k] & e$path == cc$path[k], ]
    if (!nrow(r) || is.na(r$est[1])) return("--")
    paste0(fmtNum(r$est[1], digits),
           if (!is.na(r$pvalue[1]) && r$pvalue[1] < .05) "*" else " ")
  }
  tab <- vapply(seq_len(nrow(cc)), function(k)
    vapply(items, cell, character(1), k = k), character(length(items)))
  tab <- matrix(tab, nrow = length(items), dimnames = list(NULL, cc$heading))
  flag <- admFlag(x$summary$admissible, x$summary$status)
  out <- data.frame(item = paste0(x$summary$item, flag), n = x$summary$n, tab,
                    check.names = FALSE, stringsAsFactors = FALSE)

  cat("<fancyCrossLag> two-wave cross-lagged model of", length(items),
      if (length(items) == 1) "item" else "items", "with", s$X, "\n")
  cat(crossLagHeader(x), sep = "\n")
  cat("\n  effects (* p < .05)\n",
      sprintf("   selection = %s ~ Y1;  change = Y2 ~ %s\n", s$Xcols[2], s$Xcols[1]),
      sep = "")
  print(out, row.names = FALSE, right = FALSE)

  p <- poolPaths(x$paths, pool)
  for (o in c("item", s$X)) {
    tab <- decompCells(p[p$outcome == o, ], digits)
    rownames(tab) <- NULL
    blk <- data.frame(item = paste0(items, flag), tab, check.names = FALSE,
                      stringsAsFactors = FALSE)
    cat(sprintf("\n  stability of %s: estimate (share of total)\n",
                if (o == "item") "each item" else o))
    print(blk, row.names = FALSE, right = FALSE)
  }
  flagNotes(flag)
  invisible(x)
}

#' @rdname crossLagPaths-methods
#' @export
summary.fancyCrossLag <- function(object, ...) {
  p <- object$paths
  paths <- data.frame(item = p$item, outcome = p$outcome, path = pathLabel(p$path),
                      type = p$type, est = p$est, se = p$se,
                      ci.lower = p$ci.lower, ci.upper = p$ci.upper,
                      pvalue = p$pvalue, share = p$share, stringsAsFactors = FALSE)
  structure(list(effects = object$effects, paths = paths,
                 status = object$status, header = crossLagHeader(object)),
            class = "summary.fancyCrossLag")
}

#' @rdname crossLagPaths-methods
#' @export
print.summary.fancyCrossLag <- function(x, digits = 3, ...) {
  rnd <- function(d) {
    num <- intersect(c("est", "se", "ci.lower", "ci.upper", "share"), names(d))
    d[num] <- lapply(d[num], round, digits)
    d$pvalue <- format.pval(d$pvalue, digits = 2, eps = .001)
    d
  }
  cat("<fancyCrossLag summary>\n")
  cat(x$header, sep = "\n")
  cat("\nEffects (est, 95% CI, p):\n")
  e <- x$effects
  print(rnd(e[, c("item", "effect", "path", "est", "se", "ci.lower", "ci.upper",
                  "pvalue")]), row.names = FALSE)
  cat("\nStability decompositions (est, 95% CI, p, share of total):\n")
  print(rnd(x$paths), row.names = FALSE)
  bad <- x$status[!x$status$status %in% "Success", , drop = FALSE]
  if (nrow(bad)) {
    cat("\nItems with problems:\n")
    print(bad, row.names = FALSE)
  }
  invisible(x)
}

#' @param type \code{"effects"} (default) for a scatterplot of every item's
#'   selection effect (\code{X2 ~ Y1}, horizontal) against its change effect
#'   (\code{Y2 ~ X1}, vertical); \code{"bars"} for each item's two stability
#'   decompositions, side by side. The scatterplot is read as in
#'   \code{\link{plot.fancyStability}}: points in the upper-right and
#'   lower-left quadrants are corresponsive.
#' @param labels For \code{type = "effects"}: point labels, either a character
#'   vector (one per item) or a function applied to the item names.
#' @param bands For \code{type = "effects"}: shade the regions where
#'   estimates are not significant? See "Significance bands" in
#'   \code{\link{plot.fancyStability}}. \code{TRUE} (default) computes them
#'   from the items plotted; a list of fits (e.g. \code{list(fitObserved,
#'   fitAdjusted)}) computes them across all of those fits' items, so several
#'   plots can share the same bands; \code{FALSE} omits them.
#' @param same_range For \code{type = "effects"}: give both axes the same
#'   range, centred on zero, with equal scaling (default \code{TRUE}), so
#'   selection and change effects of the same size sit the same distance from
#'   zero. \code{xlim} and \code{ylim} in \code{...} override the range.
#' @param what,sort For \code{type = "bars"}: plot the estimates
#'   (\code{"est"}, default) or shares (\code{"share"}), and whether to sort
#'   items by the item's total stability.
#' @rdname crossLagPaths-methods
#' @export
plot.fancyCrossLag <- function(x, type = c("effects", "bars"), labels = NULL,
                               bands = TRUE, same_range = TRUE,
                               what = c("est", "share"), sort = FALSE,
                               pool = NULL, ...) {
  type <- match.arg(type)
  s <- x$settings
  if (type == "bars")
    return(decompBars(poolPaths(x$paths, pool),
                      adm = x$summary[c("item", "admissible")],
                      what = match.arg(what), sort = sort, metric = s$metric))

  a <- list(...)
  effectsScatter(selChgTable(x), labels = labels, xlim = a$xlim, ylim = a$ylim,
                 title = a$title, bands = bands, same_range = same_range,
                 label_size = if (is.null(a$label_size)) 3 else a$label_size,
                 xlab = sprintf("Selection effects (%s ~ Y1)", s$Xcols[2]),
                 ylab = sprintf("Change effects (Y2 ~ %s)", s$Xcols[1]))
}
