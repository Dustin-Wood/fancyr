#' Compute Scale Scores and Reliabilities for a Multi-Scale Inventory
#'
#' @description
#' Given a data frame of item responses and a scoring key, computes
#' sign-scored scale means, Cronbach's alpha, and a table of potentially
#' miskeyed items for every scale defined in the key.
#' Calls \code{\link{cScale}} for each scale.
#'
#' @param data Data frame of item responses. If \code{smin} and \code{smax}
#'   are supplied, raw scores on \eqn{[smin, smax]} are accepted and will be
#'   cx-transformed internally. Otherwise item columns must already be on
#'   \eqn{[-1, 1]}; a warning is issued if values outside that range are
#'   detected.
#' @param key Data frame with one row per item, containing at minimum the
#'   columns named by \code{scaleCol}, \code{itemCol}, and \code{signCol}
#'   (unless \code{signCol = "all1"}).
#' @param scaleCol Name of the column in \code{key} that identifies which
#'   scale each item belongs to. Rows where this column is \code{NA} are
#'   dropped before processing.
#' @param itemCol Name of the column in \code{key} containing item
#'   identifiers matching \code{colnames(data)}.
#' @param signCol Name of the column in \code{key} with item signs
#'   (\code{+1} forward, \code{-1} reverse). Default \code{"sign"}.
#'   Use the special value \code{"all1"} to score all items \code{+1}
#'   regardless of any key column — useful for screening potentially
#'   miskeyed items before finalising a scoring key.
#' @param na.rm Logical; whether to ignore \code{NA}s when computing scale
#'   means. Default \code{TRUE}.
#' @param smin,smax Original scale endpoints (e.g., \code{smin = 1},
#'   \code{smax = 5} for a 1–5 Likert item). When both are given, all item
#'   columns are cx-transformed once before scoring. Must be supplied together.
#' @param return_original Logical; if \code{TRUE} and \code{smin}/\code{smax}
#'   are given, scale means are returned on the original \eqn{[smin, smax]}
#'   range via \code{\link{uncx}}. Alpha values are always on the cx scale.
#'   Default \code{FALSE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{scaleMeans}}{Data frame with one row per participant and
#'       one column per scale. Values are on \eqn{[-1, 1]} (cx scale) unless
#'       \code{return_original = TRUE}.}
#'     \item{\code{alphas}}{Named list of full \code{psych::alpha} results,
#'       one per scale. Access a numeric value via
#'       \code{alphas$ScaleName$total$raw_alpha}.}
#'     \item{\code{miskeyed}}{Data frame with columns \code{scale} and
#'       \code{item} listing every item whose item-rest correlation
#'       (\code{r.drop}) is negative after sign correction, suggesting
#'       incorrect keying. \code{NULL} when no items are flagged. A message
#'       is also printed summarising the count.}
#'   }
#'
#' @details
#' When \code{smin}/\code{smax} are supplied, the cx-transformation is applied
#' once to the full item subset before iterating over scales. The out-of-range
#' warning and the miskeyed message each fire at most once per call.
#' Items not found in \code{colnames(data)} trigger a warning and are skipped.
#'
#' @seealso \code{\link{cScale}} for single-scale computation;
#'   \code{\link{cx}}, \code{\link{uncx}}.
#'
#' @examples
#' d <- data.frame(
#'   item1 = c(2, 3, 4, 5, 5, 4, 3, 2, 1, 1),
#'   item2 = c(4, 3, 2, 1, 1, 2, 3, 4, 5, 5),  # reverse of item1
#'   item3 = c(3, 4, 5, 4, 3, 4, 5, 3, 2, 2),
#'   item4 = c(1, 2, 3, 2, 1, 2, 3, 2, 4, 3),
#'   item5 = c(2, 1, 3, 2, 2, 3, 1, 2, 3, 4)
#' )
#' key <- data.frame(
#'   item  = c("item1", "item2", "item3", "item4", "item5"),
#'   scale = c("A", "A", "A", "B", "B"),
#'   sign  = c(1, -1, 1, 1, 1),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Correct keying, raw 1-5 data in, cx means out
#' out <- makeAllScales(d, key, scaleCol = "scale", itemCol = "item",
#'                      smin = 1, smax = 5)
#' out$scaleMeans
#' sapply(out$alphas, function(a) round(a$total$raw_alpha, 3))
#' out$miskeyed   # NULL: no miskeyed items with correct key
#'
#' # Screen for miskeyed items by ignoring the sign column
#' screen <- makeAllScales(d, key, scaleCol = "scale", itemCol = "item",
#'                         signCol = "all1", smin = 1, smax = 5)
#' screen$miskeyed   # item2 flagged in scale A
#'
#' @export
makeAllScales <- function(data, key, scaleCol, itemCol, signCol = "sign",
                          na.rm = TRUE, smin = NULL, smax = NULL,
                          return_original = FALSE) {
  for (col in c(scaleCol, itemCol)) {
    if (!col %in% names(key))
      stop(sprintf("Column '%s' not found in key.", col))
  }
  if (!identical(signCol, "all1") && !signCol %in% names(key))
    stop(sprintf("Column '%s' not found in key.", signCol))
  if (xor(is.null(smin), is.null(smax)))
    stop("'smin' and 'smax' must be supplied together, or not at all.")
  if (return_original && (is.null(smin) || is.null(smax)))
    stop("'return_original = TRUE' requires 'smin' and 'smax'.")

  key <- key[!is.na(key[[scaleCol]]), , drop = FALSE]

  # Subset data to item columns; handle missing items once, up front
  all_items <- key[[itemCol]]
  if (is.numeric(all_items)) {
    item_data <- data[, all_items, drop = FALSE]
  } else {
    missing_items <- setdiff(all_items, colnames(data))
    if (length(missing_items) > 0)
      warning(sprintf(
        "%d item(s) in key not found in data and will be skipped: %s",
        length(missing_items), paste(missing_items, collapse = ", ")
      ), call. = FALSE)
    key       <- key[key[[itemCol]] %in% colnames(data), , drop = FALSE]
    item_data <- data[, key[[itemCol]], drop = FALSE]
  }

  all_signs <- if (identical(signCol, "all1"))
    rep(1L, nrow(key))
  else
    key[[signCol]]

  # cx-transform or range-check — fires once, not once per scale
  if (!is.null(smin)) {
    item_data <- as.data.frame(cx(item_data, smin = smin, smax = smax))
  } else {
    item_range <- range(item_data, na.rm = TRUE)
    if (item_range[2] > 1 || item_range[1] < -1)
      warning(sprintf(
        "Item values outside [-1, 1] detected (observed range [%s, %s]). ",
        round(item_range[1], 2), round(item_range[2], 2)
      ), "Scale means may not be interpretable. ",
        "Supply 'smin' and 'smax' to cx-transform data internally.",
        call. = FALSE
      )
  }

  scale_groups <- split(key, key[[scaleCol]])

  results <- lapply(scale_groups, function(s) {
    cScale(item_data, all_signs, s[[itemCol]], na.rm = na.rm,
           .check_range = FALSE)
  })

  scaleMeans <- as.data.frame(
    lapply(results, `[[`, "scaleMean"),
    check.names = FALSE
  )

  if (return_original)
    scaleMeans <- as.data.frame(
      lapply(scaleMeans, uncx, newmin = smin, newmax = smax),
      check.names = FALSE
    )

  # Collect miskeyed items across all scales and report once
  mk_list <- Filter(Negate(is.null), lapply(names(results), function(sc) {
    mk <- results[[sc]]$miskeyed
    if (length(mk) > 0)
      data.frame(scale = sc, item = mk, stringsAsFactors = FALSE)
    else NULL
  }))
  miskeyed <- if (length(mk_list) > 0) do.call(rbind, mk_list) else NULL

  if (!is.null(miskeyed))
    message(sprintf(
      "%d item(s) across %d scale(s) have negative item-rest correlations and may be miskeyed; see $miskeyed.",
      nrow(miskeyed), length(unique(miskeyed$scale))
    ))

  list(
    scaleMeans = scaleMeans,
    alphas     = lapply(results, `[[`, "alpha"),
    miskeyed   = miskeyed
  )
}
