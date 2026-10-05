#' How Much Do Stability Results Depend on the Modeled Reliability?
#' @description
#' Refits a \code{\link{stabilityPaths}} analysis over a grid of item
#' reliabilities and collects, at each value, the structural effects (e.g. the
#' selection and change effects of an experience, with their standard errors
#' and p-values) and the stability decomposition. A reliability of 1 is the
#' unadjusted (observed-variable) analysis, so the grid shows how the results
#' move as single answers are treated as less closely tied to the expected
#' score they are adjusted toward.
#'
#' @details
#' At each grid value, every item gets that reliability at both waves. Any
#' reliabilities supplied for \code{X} or \code{controls} variables in the
#' original analysis are kept fixed. Everything else -- items, mediators,
#' controls, metric, missing-data handling -- is taken from \code{x}.
#'
#' The effects reported for each item are:
#' \describe{
#'   \item{selection}{\code{X ~ Y1}: how the Time 1 item predicts each
#'     experience or mediator \code{X}.}
#'   \item{change}{\code{Y2 ~ X}: how \code{X} predicts the Time 2 item,
#'     controlling for the Time 1 item, i.e. its prediction of change.}
#'   \item{stability}{\code{Y2 ~ Y1}: the residual stability path.}
#'   \item{control}{\code{Y2 ~ C}: how each control predicts the Time 2 item.}
#' }
#' Modeling the items as less reliable controls more completely for where
#' people usually stand at Time 1, so a change effect that survives low
#' modeled reliabilities is harder to attribute to incomplete control.
#'
#' For a \code{\link{crossLagPaths}} result the effects are those in its
#' \code{$effects}: selection (\code{X2 ~ Y1}), change (\code{Y2 ~ X1}), both
#' stabilities, the Time 1 association, the Time 2 co-change, and the controls'
#' effects; \code{$paths} holds both stability decompositions, with an
#' \code{outcome} column saying whose stability each row decomposes. The grid
#' varies the items' reliability only; the reliability of \code{X} stays as
#' supplied in the original analysis.
#'
#' Grid values below an item's retest correlation typically give inadmissible
#' solutions (adjusted stability above 1); these rows are kept but flagged
#' \code{admissible = FALSE}, and the plot method leaves them out.
#'
#' A useful grid spans the reliabilities you consider plausible for the items:
#' ideally the range of their retest correlations over an interval of a few
#' weeks. See the "Choosing a reliability" section of
#' \code{\link{stabilityPaths}} for why that interval, rather than a
#' same-session or very short one.
#'
#' @param x A \code{fancyStability} object from \code{\link{stabilityPaths}},
#'   or a \code{fancyCrossLag} object from \code{\link{crossLagPaths}}.
#' @param rel Numeric vector of reliabilities to try, each in (0, 1]. Defaults
#'   to \code{seq(.5, 1, .1)}.
#' @param items Items to include. Defaults to all items in \code{x}.
#' @param cores Number of CPU cores to spread each refit's items over, or a
#'   cluster from \code{\link[parallel]{makeCluster}}. Defaults to the value
#'   used for \code{x}. Worker processes are started once and reused across
#'   the grid.
#'
#' @return An object of class \code{fancyStabilitySensitivity}: a list with
#' \item{effects}{Data frame, one row per reliability per item per effect:
#'   \code{reliability}, \code{item}, \code{effect} (\code{"selection"},
#'   \code{"change"}, \code{"stability"} or \code{"control"}), \code{via}
#'   (the \code{X} or control involved), \code{path}, \code{est}, \code{se},
#'   \code{pvalue}, \code{ci.lower}, \code{ci.upper}, and \code{admissible}.}
#' \item{paths}{Data frame of the decomposition at each reliability:
#'   \code{reliability}, \code{item}, \code{path}, \code{type}, \code{est},
#'   \code{se}, \code{share}, and \code{admissible}.}
#' \item{metric}{The metric of the estimates (\code{"std"} or \code{"raw"}).}
#'
#' \code{print()} tabulates the selection and change effects across the grid.
#' \code{plot()} draws them by default (see the arguments below), or the
#' decomposition with \code{what = "est"} or \code{"share"}. It returns a
#' \pkg{ggplot2} object, which prints as the plot and can be modified with
#' \code{+}.
#'
#' @seealso \code{\link{stabilityPaths}}, \code{\link{crossLagPaths}}
#'
#' @examples
#' d <- stabilityData(powerTraits$T1, powerTraits$T2, powerTraits$people,
#'                    commonItems = c("power", "communion", "agency"))
#' sp <- stabilityPaths(d, items = c("communion", "agency"),
#'                      X = "power[T1]", controls = "tenure")
#' sens <- reliabilitySensitivity(sp, rel = c(.6, .8, 1))
#' sens
#' plot(sens)
#' plot(sens, what = "est")
#'
#' @export
reliabilitySensitivity <- function(x, rel = seq(.5, 1, .1),
                                   items = x$settings$items,
                                   cores = x$settings$cores) {
  crossLag <- inherits(x, "fancyCrossLag")
  if (!inherits(x, "fancyStability") && !crossLag)
    stop("`x` must be a stabilityPaths() or crossLagPaths() result.")
  if (!is.numeric(rel) || any(is.na(rel)) || any(rel <= 0 | rel > 1))
    stop("`rel` must be reliabilities in (0, 1].")
  s <- x$settings
  unknown <- setdiff(items, s$items)
  if (length(unknown))
    stop("Item(s) not in `x`: ", paste(unknown, collapse = ", "))

  # start any worker processes once, and reuse them at every grid value
  oc <- openCores(if (is.null(cores)) 1 else cores, length(items))
  on.exit(oc$close(), add = TRUE)

  fits <- lapply(sort(unique(rel)), function(r) {
    relv <- c(stats::setNames(rep(r, length(items)), items), s$reliability$other)
    refit <- if (crossLag) crossLagPaths else stabilityPaths
    fit <- refit(x$data, items = items, X = s$X, controls = s$controls,
                 reliability = relv, metric = s$metric,
                 suffixes = unname(s$suffixes), missing = s$missing,
                 cores = oc$cores,
                 binary = if (is.null(s$binary)) "sd" else s$binary)
    adm <- function(it) fit$status$admissible[match(it, fit$status$item)]
    p <- fit$paths
    e <- if (crossLag) fit$effects else effectTable(fit)
    paths <- data.frame(reliability = r, item = p$item, path = p$path,
                        type = p$type, est = p$est, se = p$se,
                        share = p$share, admissible = adm(p$item),
                        stringsAsFactors = FALSE)
    if (crossLag) paths <- cbind(paths[1:2], outcome = p$outcome, paths[-(1:2)],
                                 stringsAsFactors = FALSE)
    list(
      paths = paths,
      effects = data.frame(reliability = r, e, admissible = adm(e$item),
                           stringsAsFactors = FALSE))
  })
  bind <- function(part) {
    out <- do.call(rbind, lapply(fits, `[[`, part))
    rownames(out) <- NULL
    out
  }
  structure(list(effects = bind("effects"), paths = bind("paths"),
                 metric = s$metric),
            class = "fancyStabilitySensitivity")
}

#' @param digits Number of decimal places to print.
#' @rdname reliabilitySensitivity
#' @export
print.fancyStabilitySensitivity <- function(x, digits = 2, ...) {
  e <- x$effects[x$effects$effect %in% c("selection", "change"), ]
  cell <- ifelse(is.na(e$est), "--",
                 paste0(formatC(round(e$est, digits) + 0, digits = digits, format = "f"),
                        ifelse(!is.na(e$pvalue) & e$pvalue < .05, "*", " "),
                        ifelse(!is.na(e$admissible) & !e$admissible, "!", "")))
  e$row <- paste(e$item, e$path, sep = ": ")
  rels <- sort(unique(e$reliability))
  rows <- unique(e$row)
  tab <- t(vapply(rows, function(r)
    cell[e$row == r][match(rels, e$reliability[e$row == r])], character(length(rels))))
  tab <- matrix(tab, nrow = length(rows), dimnames = list(rows, format(rels)))
  cat("<fancyStabilitySensitivity> selection and change effects by modeled reliability\n")
  cat("  metric:", if (x$metric == "std") "standardized" else "raw",
      "   * p < .05   ! inadmissible\n\n")
  print(noquote(tab), right = TRUE)
  invisible(x)
}

#' @param what What to plot: \code{"effects"} (default) draws the structural
#'   effects with 95\% confidence intervals, filled where p < .05;
#'   \code{"est"} or \code{"share"} draw the stability decomposition's
#'   pathway estimates or their shares of the total.
#' @param effects For \code{what = "effects"}: which effects to draw, any of
#'   \code{"selection"} (default), \code{"change"} (default),
#'   \code{"stability"} and \code{"control"}, plus, for a cross-lag fit,
#'   \code{"association"} and \code{"co-change"}. One panel per effect and
#'   path.
#' @param show_controls For \code{what = "est"} or \code{"share"}: draw a panel
#'   for each control's (confounded) pathway? Default \code{FALSE}.
#' @param pool With \code{show_controls = TRUE}, an optional named list of
#'   control sets to draw as one panel, e.g.
#'   \code{list(organization = c("orgB", "orgC"))}; see
#'   \code{\link{stabilityPaths-methods}}.
#' @param highlight Items to pick out: each is drawn in its own colour (with
#'   confidence intervals, for \code{what = "effects"}) on top of the
#'   all-items summary described under \code{max_items}. Setting it always
#'   gives that summary view.
#' @param max_items With up to this many items (default 8), each item gets its
#'   own coloured line. With more, the plot shows what happens across items
#'   instead: each item is a thin grey line, the black line is the median
#'   across items, and the band holds the middle 50\% of them. For
#'   \code{what = "effects"}, the numbers at the top and bottom of each panel
#'   count the items whose effect is positive or negative with p < .05.
#' @param ... Not used.
#' @rdname reliabilitySensitivity
#' @export
plot.fancyStabilitySensitivity <- function(x, what = c("effects", "est", "share"),
                                           effects = c("selection", "change"),
                                           show_controls = FALSE, pool = NULL,
                                           highlight = NULL, max_items = 8,
                                           ...) {
  what <- match.arg(what)
  effects <- match.arg(effects, c("selection", "change", "stability", "control",
                                  "association", "co-change"),
                       several.ok = TRUE)
  ylab_est <- if (identical(x$metric, "raw")) "estimate" else "standardized estimate"

  # one row per reliability, item and panel, with the plotted value in `y`
  if (what == "effects") {
    d <- x$effects[x$effects$effect %in% effects, ]
    ok <- !is.na(d$admissible) & d$admissible
    d[!ok, c("est", "ci.lower", "ci.upper")] <- NA
    titles <- c(selection = "Selection effect", change = "Change effect",
                stability = "Stability", control = "Control effect",
                association = "T1 association", `co-change` = "Co-change")
    d$panel <- paste0(titles[d$effect], ": ", pathLabel(d$path))
    d$panel <- factor(d$panel, levels = unique(d$panel[order(match(d$effect, effects))]))
    d$sig <- factor(ifelse(!is.na(d$pvalue) & d$pvalue < .05, "p < .05", "p \u2265 .05"),
                    levels = c("p < .05", "p \u2265 .05"))
    d$y <- d$est
    ylab <- ylab_est
  } else {
    d <- x$paths
    if (!show_controls) d <- d[d$type != "confounded", ]
    else d <- poolPaths(d, pool)
    d <- d[d$type != "total" | what == "est", ]
    d$y <- ifelse(!is.na(d$admissible) & d$admissible, d[[what]], NA)
    d$panel <- pathLabel(d$path)
    # a cross-lag fit decomposes two stabilities; say whose each panel shows
    if ("outcome" %in% names(d))
      d$panel <- paste0(ifelse(d$outcome == "item", "item", d$outcome), ": ", d$panel)
    d$panel <- factor(d$panel, levels = unique(d$panel))
    ylab <- if (what == "share") "share of total" else ylab_est
  }
  d$item <- factor(d$item, levels = unique(d$item))
  unknown <- setdiff(highlight, levels(d$item))
  if (length(unknown))
    stop("`highlight` item(s) not in `x`: ", paste(unknown, collapse = ", "))
  rels <- sort(unique(d$reliability))

  g <- if (is.null(highlight) && nlevels(d$item) <= max_items)
         sensItemLines(d, what, rels)
       else sensSummary(d, what, rels, unique(highlight))
  g +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    ggplot2::scale_x_continuous(breaks = rels, labels = noLeadingZero) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    ggplot2::labs(x = "modeled reliability", y = ylab) +
    fancyTheme() +
    ggplot2::theme(legend.position = "bottom",
                   plot.caption = ggplot2::element_text(hjust = 0, colour = "grey30"))
}

# Colours for items drawn one by one: Dark2 for up to 8, evenly spaced hues
# beyond that.
itemColours <- function(items) {
  pal <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
           "#A6761D", "#666666")
  n <- length(items)
  stats::setNames(if (n <= 8) pal[seq_len(n)] else grDevices::hcl.colors(n, "Dark 3"),
                  items)
}

# Layers for items drawn one by one: a coloured line per item and, for
# effects, 95% CIs and points filled where p < .05, dodged so the intervals
# don't overlap.
sensItemLayers <- function(d, what, rels, dodge = .45) {
  aes_item <- ggplot2::aes(x = .data$reliability, y = .data$y,
                           colour = .data$item, group = .data$item)
  cols <- ggplot2::scale_colour_manual(values = itemColours(levels(d$item)),
                                       name = NULL)
  if (what != "effects")
    return(list(
      ggplot2::geom_line(data = d, mapping = aes_item, na.rm = TRUE),
      ggplot2::geom_point(data = d, mapping = aes_item, size = 2, na.rm = TRUE),
      cols))
  step <- if (length(rels) > 1) min(diff(rels)) else .1
  pd <- if (nlevels(d$item) > 1) ggplot2::position_dodge(width = step * dodge)
        else "identity"
  list(
    ggplot2::geom_linerange(data = d, ggplot2::aes(x = .data$reliability,
                              ymin = .data$ci.lower, ymax = .data$ci.upper,
                              colour = .data$item, group = .data$item),
                            position = pd, na.rm = TRUE, alpha = .8),
    ggplot2::geom_line(data = d, mapping = aes_item, position = pd, na.rm = TRUE),
    ggplot2::geom_point(data = d, ggplot2::aes(x = .data$reliability, y = .data$y,
                          colour = .data$item, group = .data$item,
                          shape = .data$sig),
                        position = pd, fill = "white", size = 2.4, stroke = .9,
                        na.rm = TRUE),
    ggplot2::scale_shape_manual(values = c("p < .05" = 19, "p \u2265 .05" = 21),
                                name = NULL, drop = FALSE),
    cols)
}

sensItemLines <- function(d, what, rels) {
  g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$reliability, y = .data$y))
  for (l in sensItemLayers(d, what, rels)) g <- g + l
  g + ggplot2::scale_y_continuous(labels = noLeadingZero)
}

# The sensitivity plot for many items, showing what happens across them
# rather than a line per item that no palette or legend could tell apart.
# Each item is a thin grey line; the black line is the median across items
# and the band their middle 50%, among admissible fits. For effects, the
# counts at the top and bottom of each panel are the items with p < .05 in
# each direction. `highlight`ed items are drawn in colour on top.
sensSummary <- function(d, what, rels, highlight = NULL) {
  keys <- unique(d[c("panel", "reliability")])
  st <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
    r <- d[d$panel == keys$panel[i] & d$reliability == keys$reliability[i], ]
    y <- r$y[!is.na(r$y)]
    q <- if (length(y)) stats::quantile(y, c(.25, .5, .75), names = FALSE)
         else rep(NA_real_, 3)
    out <- data.frame(keys[i, ], n = length(y), lo = q[1], med = q[2], hi = q[3])
    if (what == "effects") {
      sig <- !is.na(r$y) & r$sig == "p < .05"
      out$pos <- sum(sig & r$y > 0)
      out$neg <- sum(sig & r$y < 0)
    }
    out
  }))
  n_items <- nlevels(d$item)
  alpha <- max(.08, min(.5, 10 / n_items))

  bg <- d[!d$item %in% highlight, ]
  g <- ggplot2::ggplot(bg, ggplot2::aes(x = .data$reliability, y = .data$y)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$item), colour = "grey45",
                       alpha = alpha, linewidth = .35, na.rm = TRUE) +
    ggplot2::geom_ribbon(data = st, inherit.aes = FALSE,
                         ggplot2::aes(x = .data$reliability, ymin = .data$lo,
                                      ymax = .data$hi),
                         fill = "grey20", alpha = .18, na.rm = TRUE) +
    ggplot2::geom_line(data = st, inherit.aes = FALSE,
                       ggplot2::aes(x = .data$reliability, y = .data$med),
                       colour = "black", linewidth = 1.1, na.rm = TRUE)
  caption <- sprintf(paste("Grey lines: each of the %d items. Black line: median",
                           "across items; band: middle 50%% of items."), n_items)
  if (what == "effects") {
    g <- g +
      ggplot2::geom_text(data = st, inherit.aes = FALSE, size = 3,
                         ggplot2::aes(x = .data$reliability, y = Inf,
                                      label = paste0("+", .data$pos)), vjust = 1.5) +
      ggplot2::geom_text(data = st, inherit.aes = FALSE, size = 3,
                         ggplot2::aes(x = .data$reliability, y = -Inf,
                                      label = paste0("\u2212", .data$neg)), vjust = -.6)
    caption <- paste0(caption, "\nNumbers: items with p < .05, positive (top) ",
                      "and negative (bottom).")
  }
  if (any(st$n < n_items)) caption <- paste(caption, "Inadmissible fits are left out.")

  if (length(highlight)) {
    hd <- d[d$item %in% highlight, ]
    hd$item <- factor(hd$item, levels = highlight)
    for (l in sensItemLayers(hd, what, rels, dodge = .3)) g <- g + l
  }
  g +
    ggplot2::scale_y_continuous(labels = noLeadingZero,
                                expand = ggplot2::expansion(mult = .14)) +
    ggplot2::labs(caption = caption)
}
