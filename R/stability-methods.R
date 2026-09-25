# print / summary / plot methods for fancyStability objects (stabilityPaths()).

# Pathway columns in display order: residual, mediated, confounded, total.
pathOrder <- function(paths) {
  one <- paths[paths$item == paths$item[1], ]
  one$path[order(match(one$type, c("residual", "mediated", "confounded", "total")))]
}

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
    keep <- intersect(c("item", "type", "reliability", "admissible"), names(p))
    key <- if ("reliability" %in% names(p)) paste(p$item, p$reliability) else p$item
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
  c(sprintf("  mediators (X): %s", if (length(s$X)) paste(s$X, collapse = ", ") else "none"),
    sprintf("  controls:      %s", if (length(s$controls)) paste(s$controls, collapse = ", ") else "none"),
    sprintf("  latent:        %s", rel_txt),
    sprintf("  metric:        %s   missing: %s",
            if (s$metric == "std") "standardized" else "raw", s$missing))
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
  cols <- pathOrder(p)
  items <- unique(p$item)

  # round first, so a tiny negative prints as 0.00 rather than -0.00
  num <- function(v, dg) formatC(round(v, dg) + 0, digits = dg, format = "f")
  cell <- function(row) {
    if (!nrow(row) || is.na(row$est)) return("--")
    if (row$type == "total") return(num(row$est, digits))
    sprintf("%s (%s%%)", num(row$est, digits), num(100 * row$share, 0))
  }
  tab <- do.call(rbind, lapply(items, function(it) {
    vapply(cols, function(cn) cell(p[p$item == it & p$path == cn, ][1, ]),
           character(1))
  }))
  tab <- matrix(tab, nrow = length(items), dimnames = list(NULL, pathLabel(cols)))

  s <- x$summary
  flag <- ifelse(is.na(s$admissible), " ?", ifelse(s$admissible, "", " !"))
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
#' have one effect without the other. A point's fill shows its change effect
#' (filled if p < .05, white otherwise) and its outline shows its selection
#' effect (black if p < .05, light grey otherwise); a filled point takes its
#' outline colour. Labels are placed with \pkg{ggrepel} when it is installed. To
#' compare two analyses (e.g. with and without a reliability adjustment), pass
#' both plots the same \code{xlim} and \code{ylim}.
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
#'   For the effects scatterplot: \code{xlim} and \code{ylim} (axis ranges)
#'   and \code{title}. The bar chart ignores \code{...}.
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

  what <- match.arg(what)
  p <- poolPaths(x$paths, pool)
  cols  <- pathOrder(p)
  parts <- setdiff(cols, cols[p$type[match(cols, p$path)] == "total"])
  types <- p$type[match(parts, p$path)]
  items <- unique(p$item)

  tot <- vapply(items, function(it) {
    if (what == "share") 1 else p$est[p$item == it & p$type == "total"][1]
  }, numeric(1))
  # first item at the top unless sorted by total stability
  ord <- if (sort) items[order(tot)] else rev(items)

  n_med  <- sum(types == "mediated"); n_conf <- sum(types == "confounded")
  ramp <- function(from, to, k) if (k) grDevices::colorRampPalette(c(from, to))(k)
  pal <- c("grey75",
           ramp("#2F6DB5", "#9DC3EA", n_med),    # mediated: blues
           ramp("#D9731E", "#F5C28F", n_conf))   # confounded: oranges
  names(pal) <- pathLabel(parts)

  adm <- x$summary$admissible[match(items, x$summary$item)]
  lbl <- stats::setNames(paste0(items, ifelse(!is.na(adm) & !adm, " !", "")), items)

  bars <- p[p$path %in% parts, ]
  bars$value <- if (what == "share") bars$share else bars$est
  bars$path  <- factor(pathLabel(bars$path), levels = pathLabel(parts))
  bars$item  <- factor(bars$item, levels = ord)
  totals <- data.frame(item = factor(items, levels = ord), value = tot,
                       mark = "total")

  ggplot2::ggplot(bars, ggplot2::aes(x = .data$value, y = .data$item)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey40") +
    ggplot2::geom_col(ggplot2::aes(fill = .data$path), width = .7,
                      colour = "white", linewidth = .3, na.rm = TRUE,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_point(data = totals, ggplot2::aes(shape = .data$mark),
                        size = 7, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::scale_shape_manual(values = c(total = "|"), name = NULL) +
    ggplot2::scale_y_discrete(labels = lbl) +
    ggplot2::labs(x = if (what == "share") "share of total stability"
                      else sprintf("stability (%s)",
                                   if (x$settings$metric == "std") "standardized" else "raw"),
                  y = NULL) +
    fancyTheme() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.y = ggplot2::element_blank())
}

# Shared look for the package's ggplot graphics.
fancyTheme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(colour = "grey80", fill = NA),
                   strip.text = ggplot2::element_text(face = "bold"),
                   plot.title = ggplot2::element_text(face = "bold"))
}

# Selection (X ~ Y1) vs change (Y2 ~ X) scatterplot, one labelled point per
# item. Behind plot(x, type = "effects").
plotEffects <- function(x, X = NULL, labels = NULL, xlim = NULL, ylim = NULL,
                        title = NULL, ...) {
  s <- x$settings
  if (!length(s$X)) stop("`x` has no mediator (X); there are no selection or change effects to plot.")
  if (is.null(X)) X <- s$X[1]
  if (!X %in% s$X) stop("`X` must be one of: ", paste(s$X, collapse = ", "))

  e <- effectTable(x)
  sel <- e[e$effect == "selection" & e$via == X, ]
  chg <- e[e$effect == "change" & e$via == X, ]
  items <- s$items
  adm <- x$status$admissible[match(items, x$status$item)]
  out <- data.frame(item = items,
                    selection = sel$est[match(items, sel$item)],
                    selection_p = sel$pvalue[match(items, sel$item)],
                    change = chg$est[match(items, chg$item)],
                    change_p = chg$pvalue[match(items, chg$item)],
                    admissible = adm, stringsAsFactors = FALSE)
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
  sig_lv <- c("p < .05", "p ≥ .05")
  selSig <- !is.na(d$selection_p) & d$selection_p < .05
  chgSig <- !is.na(d$change_p) & d$change_p < .05
  # outline shows the selection effect (black vs light grey); fill shows the
  # change effect (filled in the outline colour vs white)
  d$selKey  <- factor(ifelse(selSig, sig_lv[1], sig_lv[2]), levels = sig_lv)
  d$fillKey <- factor(ifelse(!chgSig, "ns", ifelse(selSig, "sigDark", "sigLight")),
                      levels = c("sigDark", "sigLight", "ns"))
  dark <- "black"; light <- "grey65"

  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$selection, y = .data$change)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$selKey, fill = .data$fillKey),
                        shape = 21, size = 2.6, stroke = 1) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(c(dark, light), sig_lv), limits = sig_lv,
      name = "selection effect",
      guide = ggplot2::guide_legend(order = 1, override.aes = list(fill = "white"))) +
    ggplot2::scale_fill_manual(
      values = c(sigDark = dark, sigLight = light, ns = "white"),
      limits = c("sigDark", "sigLight", "ns"), breaks = c("sigDark", "ns"),
      labels = sig_lv, name = "change effect",
      guide = ggplot2::guide_legend(order = 2, override.aes = list(colour = dark)))
  g <- g + if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$label), size = 3,
                             max.overlaps = Inf, seed = 1, min.segment.length = .3,
                             segment.colour = "grey70", segment.size = .3,
                             box.padding = .25, point.padding = .15)
  } else {
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3, vjust = -.8)
  }
  g + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = paste0("Selection effects (", X, " ~ Y1)"),
                  y = paste0("Change effects (Y2 ~ ", X, ")"), title = title) +
    fancyTheme() +
    ggplot2::theme(legend.position = "bottom")
}
