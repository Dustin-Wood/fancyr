#' Compute a Single Sign-Scored Scale Mean and Reliability
#'
#' @description
#' Reverse-scores any items flagged in \code{sign}, then returns the
#' row-wise scale mean, Cronbach's alpha, and a vector of potentially
#' miskeyed items for the selected items.
#'
#' @param data Data frame or matrix of item scores. If \code{smin} and
#'   \code{smax} are supplied, raw scores on \eqn{[smin, smax]} are accepted
#'   and will be cx-transformed internally. Otherwise data must already be on
#'   \eqn{[-1, 1]} (use \code{\link{cx}} first); a warning is issued if
#'   values outside that range are detected.
#' @param sign Numeric vector of item signs (\code{+1} forward,
#'   \code{-1} reverse). Length must equal \code{ncol(data)}, ordered to
#'   match its columns. Defaults to all \code{+1} if omitted.
#' @param items Column names or integer indices specifying which items in
#'   \code{data} belong to this scale.
#' @param na.rm Logical; whether to ignore \code{NA}s when computing the
#'   scale mean. Default \code{TRUE}.
#' @param smin,smax Original scale endpoints (e.g., \code{smin = 1},
#'   \code{smax = 5} for a 1–5 Likert item). When both are given, items are
#'   cx-transformed internally before scoring. Must be supplied together.
#' @param return_original Logical; if \code{TRUE} and \code{smin}/\code{smax}
#'   are given, the scale mean is returned on the original
#'   \eqn{[smin, smax]} range via \code{\link{uncx}}. Alpha is always
#'   reported on the cx scale. Default \code{FALSE}.
#' @param .check_range Internal use only. When \code{FALSE}, skips the
#'   \eqn{[-1,1]} range check (used by \code{\link{makeAllScales}} to
#'   avoid redundant per-scale warnings after a single up-front check).
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{alpha}}{Full \code{psych::alpha} result on the
#'       sign-corrected, cx-scaled items. Access the numeric value via
#'       \code{$alpha$total$raw_alpha}.}
#'     \item{\code{scaleMean}}{Numeric vector of row-wise means. On the
#'       cx \eqn{[-1, 1]} scale by default; on \eqn{[smin, smax]} when
#'       \code{return_original = TRUE}.}
#'     \item{\code{miskeyed}}{Character vector of item names whose
#'       item-rest correlation (\code{r.drop}) is negative after sign
#'       correction, suggesting they may be incorrectly keyed.
#'       \code{character(0)} when none are flagged.}
#'   }
#'
#' @details
#' Sign correction is applied as \code{items_matrix \%*\% diag(sign[items])},
#' which scales each item column by its sign. \code{psych::alpha} is called
#' with \code{check.keys = FALSE} because items are already sign-corrected.
#' Miskeyed items are identified as those with a negative \code{r.drop}
#' (item-rest correlation) in \code{alpha$item.stats}.
#'
#' @seealso \code{\link{makeAllScales}} for computing all scales at once;
#'   \code{\link{cx}}, \code{\link{uncx}}.
#'
#' @examples
#' d <- data.frame(
#'   item1 = c(2, 3, 4, 5, 5, 4, 3, 2, 1, 1),
#'   item2 = c(4, 3, 2, 1, 1, 2, 3, 4, 5, 5),  # reverse of item1
#'   item3 = c(3, 4, 5, 4, 3, 4, 5, 3, 2, 2)
#' )
#'
#' # Correct keying: item2 scored -1
#' res <- cScale(d, sign = c(1, -1, 1), items = colnames(d), smin = 1, smax = 5)
#' res$alpha$total$raw_alpha
#' res$miskeyed   # character(0): no miskeyed items
#'
#' # All items scored +1 — item2 will appear miskeyed
#' res_all1 <- cScale(d, sign = c(1, 1, 1), items = colnames(d), smin = 1, smax = 5)
#' res_all1$miskeyed
#'
#' @importFrom psych alpha
#' @export
cScale <- function(data, sign, items, na.rm = TRUE,
                   smin = NULL, smax = NULL, return_original = FALSE,
                   .check_range = TRUE) {
  if (xor(is.null(smin), is.null(smax)))
    stop("'smin' and 'smax' must be supplied together, or not at all.")
  if (return_original && (is.null(smin) || is.null(smax)))
    stop("'return_original = TRUE' requires 'smin' and 'smax'.")

  if (missing(sign)) {
    sign <- setNames(rep(1L, length(items)), items)
  } else if (is.null(names(sign))) {
    if (length(sign) == ncol(data)) {
      names(sign) <- colnames(data)
    } else if (length(sign) == length(items)) {
      names(sign) <- items
    } else {
      stop(sprintf(
        "'sign' must have length ncol(data) (%d) or length(items) (%d).",
        ncol(data), length(items)
      ))
    }
  }

  item_matrix <- as.matrix(data[, items])

  if (!is.null(smin)) {
    item_matrix <- cx(item_matrix, smin = smin, smax = smax)
  } else if (.check_range) {
    item_range <- range(item_matrix, na.rm = TRUE)
    if (item_range[2] > 1 || item_range[1] < -1)
      warning(sprintf(
        "Item values outside [-1, 1] detected (observed range [%s, %s]). ",
        round(item_range[1], 2), round(item_range[2], 2)
      ), "Scale means may not be interpretable. ",
        "Supply 'smin' and 'smax' to cx-transform data internally.",
        call. = FALSE
      )
  }

  cx2 <- as.data.frame(item_matrix %*% diag(sign[items]))
  colnames(cx2) <- items

  scale_mean <- rowMeans(cx2, na.rm = na.rm)
  if (return_original)
    scale_mean <- uncx(scale_mean, newmin = smin, newmax = smax)

  alpha_result <- if (ncol(cx2) >= 2L) {
    utils::capture.output(
      result <- suppressMessages(suppressWarnings(
        psych::alpha(cx2, check.keys = FALSE)
      ))
    )
    result
  } else NULL

  r_drop <- tryCatch(alpha_result$item.stats$r.drop, error = function(e) NULL)
  miskeyed <- if (!is.null(r_drop))
    rownames(alpha_result$item.stats)[!is.na(r_drop) & r_drop < 0]
  else
    character(0)

  list(
    alpha     = alpha_result,
    scaleMean = scale_mean,
    miskeyed  = miskeyed
  )
}
