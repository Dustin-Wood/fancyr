# print / summary / plot methods for fancyStability objects (stabilityPaths()).

# Pathway columns in display order: residual, mediated, confounded, total.
pathOrder <- function(paths) {
  one <- paths[paths$item == paths$item[1], ]
  one$path[order(match(one$type, c("residual", "mediated", "confounded", "total")))]
}

pathLabel <- function(path) sub("^via_", "via ", path)

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
print.fancyStability <- function(x, digits = 2, ...) {
  p <- x$paths
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
    cat("\n  ! inadmissible solution (e.g. corrected stability > 1); see $status.",
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
#' draws that item's path diagram instead.
#'
#' @param x A \code{fancyStability} object from \code{\link{stabilityPaths}}.
#' @param item Optional item name. If given, draw its path diagram.
#' @param what For the bar chart: \code{"est"} (default) plots the estimates,
#'   which add up to total stability; \code{"share"} plots each pathway's
#'   proportion of the total.
#' @param sort Logical. Sort the bar chart by total stability? Default
#'   \code{FALSE} keeps item order.
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
#'   For the bar chart, \code{...} is passed to \code{\link[graphics]{plot}}.
#'
#' @return Invisibly, the \code{qgraph} object (diagram) or the plotted matrix
#'   of pathway values (bar chart).
#' @seealso \code{\link{stabilityPaths}}
#' @examples
#' d <- stabilityData(stabilitySim$T1, stabilitySim$T2, stabilitySim$experience,
#'                    fill = list(leader = 0))
#' sp <- stabilityPaths(d, X = "leader", controls = "ses",
#'                      reliability = stabilitySim$reliability)
#' plot(sp)
#' plot(sp, what = "share")
#' plot(sp, item = "dominant")
#' @export
#' @importFrom graphics plot rect segments abline axis legend par text
#' @importFrom grDevices hcl.colors
plot.fancyStability <- function(x, item = NULL, what = c("est", "share"),
                                sort = FALSE, ...) {
  if (!is.null(item)) {
    if (!item %in% names(x$fits))
      stop("No item named \"", item, "\". Items: ",
           paste(names(x$fits), collapse = ", "))
    args <- list(...)
    if (is.null(args$item_label)) args$item_label <- item
    return(do.call(plotMedX, c(list(sp = x$fits[[item]]), args)))
  }

  what <- match.arg(what)
  p <- x$paths
  cols  <- pathOrder(p)
  parts <- setdiff(cols, cols[p$type[match(cols, p$path)] == "total"])
  types <- p$type[match(parts, p$path)]
  items <- unique(p$item)

  val <- function(it, pth) {
    r <- p[p$item == it & p$path == pth, ][1, ]
    if (what == "share") r$share else r$est
  }
  M <- matrix(vapply(items, function(it) vapply(parts, function(pt) val(it, pt),
                                                numeric(1)),
                     numeric(length(parts))),
              nrow = length(parts), dimnames = list(parts, items))
  tot <- vapply(items, function(it) {
    if (what == "share") 1 else p$est[p$item == it & p$type == "total"][1]
  }, numeric(1))
  if (sort) { o <- order(tot); M <- M[, o, drop = FALSE]; tot <- tot[o] }

  n_med  <- sum(types == "mediated"); n_conf <- sum(types == "confounded")
  ramp <- function(from, to, k) if (k) grDevices::colorRampPalette(c(from, to))(k)
  pal <- c("grey80",
           ramp("#2F6DB5", "#9DC3EA", n_med),    # mediated: blues
           ramp("#D9731E", "#F5C28F", n_conf))   # confounded: oranges

  adm <- x$summary$admissible[match(colnames(M), x$summary$item)]
  lbl <- paste0(colnames(M), ifelse(!is.na(adm) & !adm, " !", ""))

  neg <- colSums(pmin(M, 0), na.rm = TRUE); pos <- colSums(pmax(M, 0), na.rm = TRUE)
  xlim <- range(c(0, neg, pos, tot), na.rm = TRUE)
  nI <- ncol(M)

  op <- graphics::par(mar = c(6.5, max(4, max(nchar(lbl)) * 0.55 + 1), 2, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(NA, xlim = xlim, ylim = c(0.5, nI + 0.5), yaxt = "n",
                 xlab = if (what == "share") "share of total stability"
                        else sprintf("stability (%s)", if (x$settings$metric == "std")
                                     "standardized" else "raw"),
                 ylab = "", ...)
  graphics::axis(2, at = seq_len(nI), labels = lbl, las = 1, tick = FALSE)
  graphics::abline(v = 0, col = "grey40")
  for (i in seq_len(nI)) {
    right <- 0; left <- 0
    for (k in seq_along(parts)) {
      v <- M[k, i]
      if (is.na(v)) next
      if (v >= 0) { graphics::rect(right, i - .35, right + v, i + .35, col = pal[k], border = "white"); right <- right + v }
      else        { graphics::rect(left + v, i - .35, left, i + .35, col = pal[k], border = "white"); left <- left + v }
    }
    if (!is.na(tot[i]))
      graphics::segments(tot[i], i - .45, tot[i], i + .45, lwd = 2.5)
  }
  # legend in the bottom margin, below the axis title
  usr <- graphics::par("usr")
  graphics::legend(x = mean(usr[1:2]), y = usr[3] - 0.12 * (usr[4] - usr[3]),
                   xjust = 0.5, yjust = 1, xpd = NA, horiz = TRUE, bty = "n",
                   fill = c(pal, NA), border = c(rep("white", length(pal)), NA),
                   lty = c(rep(NA, length(pal)), 1), lwd = c(rep(NA, length(pal)), 2.5),
                   legend = c(pathLabel(parts), "total"), cex = 0.9)
  invisible(M)
}
