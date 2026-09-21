#' How Much Does a Stability Decomposition Depend on the Assumed Reliability?
#' @description
#' Refits a \code{\link{stabilityPaths}} analysis over a grid of item
#' reliabilities and collects the decomposition at each value. A reliability of
#' 1 is the uncorrected (observed-variable) analysis, so the grid shows how the
#' conclusions move as more of each item's variance is treated as measurement
#' error.
#'
#' @details
#' At each grid value, every item gets that reliability at both waves. Any
#' reliabilities supplied for \code{X} or \code{controls} variables in the
#' original analysis are kept fixed. Everything else -- items, mediators,
#' controls, metric, missing-data handling -- is taken from \code{x}.
#'
#' Grid values below an item's retest correlation typically give inadmissible
#' solutions (corrected stability above 1); these rows are kept but flagged
#' \code{admissible = FALSE}, and the plot method leaves them out.
#'
#' @param x A \code{fancyStability} object from \code{\link{stabilityPaths}}.
#' @param rel Numeric vector of reliabilities to try, each in (0, 1]. Defaults
#'   to \code{seq(.5, 1, .1)}.
#' @param items Items to include. Defaults to all items in \code{x}.
#'
#' @return A data frame of class \code{fancyStabilitySensitivity}, one row per
#'   reliability per item per pathway, with columns \code{reliability},
#'   \code{item}, \code{path}, \code{type}, \code{est}, \code{se},
#'   \code{share}, and \code{admissible}. Its \code{plot()} method draws one
#'   panel per pathway type, with a line per item.
#'
#' @seealso \code{\link{stabilityPaths}}
#'
#' @examples
#' d <- stabilityData(stabilitySim$T1, stabilitySim$T2, stabilitySim$experience,
#'                    fill = list(leader = 0))
#' sp <- stabilityPaths(d, X = "leader", controls = "ses")
#' sens <- reliabilitySensitivity(sp, rel = c(.6, .7, .8, .9, 1))
#' head(sens)
#' plot(sens)
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

  out <- do.call(rbind, lapply(sort(unique(rel)), function(r) {
    relv <- c(stats::setNames(rep(r, length(items)), items), s$reliability$other)
    fit <- stabilityPaths(x$data, items = items, X = s$X, controls = s$controls,
                          reliability = relv, metric = s$metric,
                          suffixes = unname(s$suffixes), missing = s$missing)
    p <- fit$paths
    adm <- fit$status$admissible[match(p$item, fit$status$item)]
    data.frame(reliability = r, item = p$item, path = p$path, type = p$type,
               est = p$est, se = p$se, share = p$share, admissible = adm,
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  class(out) <- c("fancyStabilitySensitivity", "data.frame")
  out
}

#' @param what \code{"share"} (default) or \code{"est"}: which quantity to plot.
#' @param ... Passed to \code{\link[graphics]{matplot}}.
#' @rdname reliabilitySensitivity
#' @export
#' @importFrom graphics matplot mtext
plot.fancyStabilitySensitivity <- function(x, what = c("share", "est"), ...) {
  what <- match.arg(what)
  d <- as.data.frame(x)
  d <- d[d$type != "total" | what == "est", ]
  d$y <- ifelse(!is.na(d$admissible) & d$admissible, d[[what]], NA)

  paths <- unique(d$path)
  items <- unique(d$item)
  rels  <- sort(unique(d$reliability))
  cols  <- grDevices::hcl.colors(max(length(items), 2), "Dark 3")[seq_along(items)]

  nP <- length(paths)
  op <- graphics::par(mfrow = c(1, nP), mar = c(4, 4, 2.5, 1), oma = c(2.5, 0, 0, 0))
  on.exit(graphics::par(op), add = TRUE)
  for (pt in paths) {
    Y <- vapply(items, function(it)
      d$y[d$path == pt & d$item == it][match(rels, d$reliability[d$path == pt & d$item == it])],
      numeric(length(rels)))
    Y <- matrix(Y, nrow = length(rels))
    graphics::matplot(rels, Y, type = "b", pch = 16, lty = 1, col = cols,
                      xlab = "assumed reliability",
                      ylab = if (what == "share") "share of total" else "estimate",
                      main = pathLabel(pt), ...)
  }
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  graphics::legend("bottom", legend = items, col = cols, lty = 1, pch = 16,
                   horiz = TRUE, bty = "n", cex = 1, xpd = TRUE)
  invisible(x)
}
