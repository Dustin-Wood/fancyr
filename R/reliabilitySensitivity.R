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
#' @param x A \code{fancyStability} object from \code{\link{stabilityPaths}}.
#' @param rel Numeric vector of reliabilities to try, each in (0, 1]. Defaults
#'   to \code{seq(.5, 1, .1)}.
#' @param items Items to include. Defaults to all items in \code{x}.
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
#' @seealso \code{\link{stabilityPaths}}
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
                                   items = x$settings$items) {
  if (!inherits(x, "fancyStability"))
    stop("`x` must be a stabilityPaths() result.")
  if (!is.numeric(rel) || any(is.na(rel)) || any(rel <= 0 | rel > 1))
    stop("`rel` must be reliabilities in (0, 1].")
  s <- x$settings
  unknown <- setdiff(items, s$items)
  if (length(unknown))
    stop("Item(s) not in `x`: ", paste(unknown, collapse = ", "))

  fits <- lapply(sort(unique(rel)), function(r) {
    relv <- c(stats::setNames(rep(r, length(items)), items), s$reliability$other)
    fit <- stabilityPaths(x$data, items = items, X = s$X, controls = s$controls,
                          reliability = relv, metric = s$metric,
                          suffixes = unname(s$suffixes), missing = s$missing)
    adm <- function(it) fit$status$admissible[match(it, fit$status$item)]
    p <- fit$paths
    e <- effectTable(fit)
    list(
      paths = data.frame(reliability = r, item = p$item, path = p$path,
                         type = p$type, est = p$est, se = p$se,
                         share = p$share, admissible = adm(p$item),
                         stringsAsFactors = FALSE),
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
#'   \code{"stability"} and \code{"control"}. One panel per effect and
#'   mediator (or control).
#' @param show_controls For \code{what = "est"} or \code{"share"}: draw a panel
#'   for each control's (confounded) pathway? Default \code{FALSE}.
#' @param pool With \code{show_controls = TRUE}, an optional named list of
#'   control sets to draw as one panel, e.g.
#'   \code{list(organization = c("orgB", "orgC"))}; see
#'   \code{\link{stabilityPaths-methods}}.
#' @param ... Not used.
#' @rdname reliabilitySensitivity
#' @export
plot.fancyStabilitySensitivity <- function(x, what = c("effects", "est", "share"),
                                           effects = c("selection", "change"),
                                           show_controls = FALSE, pool = NULL,
                                           ...) {
  what <- match.arg(what)
  effects <- match.arg(effects, c("selection", "change", "stability", "control"),
                       several.ok = TRUE)
  ylab_est <- if (identical(x$metric, "raw")) "estimate" else "standardized estimate"

  if (what == "effects") {
    d <- x$effects[x$effects$effect %in% effects, ]
    ok <- !is.na(d$admissible) & d$admissible
    d[!ok, c("est", "ci.lower", "ci.upper")] <- NA
    titles <- c(selection = "Selection effect", change = "Change effect",
                stability = "Stability", control = "Control effect")
    d$panel <- paste0(titles[d$effect], ": ", pathLabel(d$path))
    d$panel <- factor(d$panel, levels = unique(d$panel[order(match(d$effect, effects))]))
    d$sig <- factor(ifelse(!is.na(d$pvalue) & d$pvalue < .05, "p < .05", "p \u2265 .05"),
                    levels = c("p < .05", "p \u2265 .05"))
    d$item <- factor(d$item, levels = unique(d$item))
    rels <- sort(unique(d$reliability))
    step <- if (length(rels) > 1) min(diff(rels)) else .1
    pd <- ggplot2::position_dodge(width = step * .45)

    return(
      ggplot2::ggplot(d, ggplot2::aes(x = .data$reliability, y = .data$est,
                                      colour = .data$item, group = .data$item)) +
        ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
        ggplot2::geom_linerange(ggplot2::aes(ymin = .data$ci.lower, ymax = .data$ci.upper),
                                position = pd, na.rm = TRUE, alpha = .8) +
        ggplot2::geom_line(position = pd, na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(shape = .data$sig), position = pd,
                            fill = "white", size = 2.4, stroke = .9, na.rm = TRUE) +
        ggplot2::scale_shape_manual(values = c("p < .05" = 19, "p \u2265 .05" = 21),
                                    name = NULL, drop = FALSE) +
        ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL) +
        ggplot2::scale_x_continuous(breaks = rels) +
        ggplot2::facet_wrap(~ panel, scales = "free_y", nrow = 1) +
        ggplot2::labs(x = "modeled reliability", y = ylab_est) +
        fancyTheme() +
        ggplot2::theme(legend.position = "bottom")
    )
  }

  d <- x$paths
  if (!show_controls) d <- d[d$type != "confounded", ]
  else d <- poolPaths(d, pool)
  d <- d[d$type != "total" | what == "est", ]
  d$y <- ifelse(!is.na(d$admissible) & d$admissible, d[[what]], NA)
  d$path <- factor(pathLabel(d$path), levels = unique(pathLabel(d$path)))
  d$item <- factor(d$item, levels = unique(d$item))
  rels <- sort(unique(d$reliability))

  ggplot2::ggplot(d, ggplot2::aes(x = .data$reliability, y = .data$y,
                                  colour = .data$item, group = .data$item)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL) +
    ggplot2::scale_x_continuous(breaks = rels) +
    ggplot2::facet_wrap(~ path, scales = "free_y", nrow = 1) +
    ggplot2::labs(x = "modeled reliability",
                  y = if (what == "share") "share of total" else ylab_est) +
    fancyTheme() +
    ggplot2::theme(legend.position = "bottom")
}
