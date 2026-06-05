#' Screen a Qualtrics Item Block for Response Speed and Variability
#' @description
#' Estimate two data-quality indicators for a single block of survey items,
#' using the five-column metadata group that Qualtrics emits immediately after
#' the rated items: the \emph{Display Order} field plus the four page-timing
#' fields (\emph{First Click}, \emph{Last Click}, \emph{Page Submit},
#' \emph{Click Count}).
#'
#' From these it computes, per respondent:
#' \itemize{
#'   \item \strong{spi} -- average seconds per item (response speed), via
#'     \code{\link{spi}}; and
#'   \item \strong{prMaxSD} -- proportion of the maximum possible response
#'     variability (straight-lining indicator), via \code{\link{prMaxSD}}.
#' }
#'
#' The rated items themselves do not need to be named: Qualtrics writes the
#' item-response columns contiguously, immediately to the left of this metadata
#' block, so the block location and the item count (read from \emph{Display
#' Order}) are sufficient to locate them.
#'
#' @param data A data frame (or tibble) of Qualtrics export data.
#' @param cols The five-column metadata block, given either as column names or
#'   column indices. Length 5 (or 3) is expected. The columns are identified by
#'   matching their names against the Qualtrics labels \emph{Display Order}
#'   (or \emph{DO}), \emph{First Click}, and \emph{Last Click}; \emph{Page
#'   Submit} and \emph{Click Count} are accepted but not required. If the names
#'   carry no such labels, the columns are assumed to be in Qualtrics export
#'   order: Display Order, First Click, Last Click, Page Submit, Click Count.
#' @param smin Scale minimum of the rated items (passed to \code{\link{prMaxSD}}).
#' @param smax Scale maximum of the rated items (passed to \code{\link{prMaxSD}}).
#' @param items Optional. The rated-item columns (names or indices). If
#'   \code{NULL} (default), they are taken to be the \code{n} columns
#'   immediately preceding the metadata block, where \code{n} is the number of
#'   items listed in \emph{Display Order}.
#' @param check.names.in Optional column map used to verify (and, where the data
#'   column names are uninformative, to identify) the five block columns. It
#'   should be a data frame with a \code{qname} column matching
#'   \code{colnames(data)} and a \code{sub} column giving each column's
#'   human-readable label -- i.e. the \code{*_colmap} object produced alongside
#'   the data by \code{\link{colmap}}. When supplied, the labels at the
#'   specified \code{cols} must match the Qualtrics block labels
#'   (\emph{Display Order}, \emph{First Click}, \emph{Last Click},
#'   \emph{Page Submit}, \emph{Click Count}); a mismatch stops with an error,
#'   since it signals that \code{cols} is misaligned with the timing block.
#'   Default \code{NULL} (no check). See \emph{Details} for whether you need it.
#' @details
#' The number of items presented in the block (\code{nPresented}) is read as the
#' maximum number of pipe-separated entries found in the \emph{Display Order}
#' column across respondents. This is a single value for the block and is what
#' anchors the item-column slice -- a per-respondent count cannot be used to
#' select columns. Respondents who never reached the page (blank Display Order)
#' receive \code{NA} for \code{spi}.
#'
#' \code{nRated} counts how many of the block's items the respondent actually
#' answered (non-missing responses), which may be fewer than \code{nPresented};
#' \code{prRated = nRated / nPresented} expresses this as a completion
#' proportion.
#'
#' \code{qScreen()} only \emph{measures} data quality -- it never drops rows.
#' Filtering is left to the caller (see \emph{Examples}). As a rule of thumb,
#' Wood et al. (2017) recommend retaining only cases with \code{spi > 1} (at
#' least one second per item, on average) and \code{prMaxSD > .25} (more than
#' minimal response variability); careless responders tend to fail one or both.
#' A completeness screen such as \code{prRated > .90} is also often sensible,
#' though there is no established convention for its cut.
#'
#' \strong{Do you need \code{check.names.in}?} Usually not. The function already
#' refuses a Display Order column that does not parse into a pipe-separated list
#' of items, and the timing fields are numeric, so a grossly mis-specified block
#' fails on its own. The colmap check earns its place in two cases the
#' structural checks miss: an off-by-one \code{cols} (you miscounted columns,
#' but the wrong column still looks plausible) and exports whose data column
#' names carry no \emph{First Click}/\emph{_DO}-style cues, where name-based
#' identification would otherwise fall back to assuming canonical column order.
#' Supplying the colmap removes that guesswork by identifying each column from
#' its authoritative \code{sub} label.
#' @return A data frame with one row per respondent and the columns
#'   \code{nPresented} (items shown in the block), \code{nRated} (items the
#'   respondent answered), \code{prRated} (\code{nRated / nPresented}),
#'   \code{spi} (seconds per item), and \code{prMaxSD} (proportion of maximum
#'   possible response variability).
#' @references Wood, D., Harms, P., Lowman, G. H., & DeSimone, J. A. (2017).
#'   Response speed and response consistency as mutually validating indicators
#'   of data quality in online samples. \emph{Social Psychological and
#'   Personality Science, 8}(4), 454-464.
#' @seealso \code{\link{spi}}, \code{\link{prMaxSD}}
#' @export
#' @examples
#' \dontrun{
#' # Screen the block whose timing columns are named with the "SRQ" prefix,
#' # rated on a 1-6 scale:
#' qScreen(F24CB.3,
#'         cols = c("SRQ_DO", "SRQ_First Click", "SRQ_Last Click",
#'                  "SRQ_Page Submit", "SRQ_Click Count"),
#'         smin = 1, smax = 6)
#'
#' # The block can equally be given by column position:
#' qScreen(F24CB.3, cols = 87:91, smin = 1, smax = 6)
#'
#' # Validate the block against the column map before screening:
#' qScreen(F24CB.3, cols = 87:91, smin = 1, smax = 6,
#'         check.names.in = F24CB.3_colmap)
#'
#' # --- Filtering on the indicators -------------------------------------
#' # Base R: keep respondents who pass the Wood et al. (2017) rules of thumb.
#' q <- qScreen(F24CB.3, cols = 87:91, smin = 1, smax = 6)
#' clean <- F24CB.3[q$spi > 1 & q$prMaxSD > .25, ]
#'
#' # dplyr: bind the indicators on, then filter (keeps the indicators too).
#' library(dplyr)
#' clean <- F24CB.3 %>%
#'   bind_cols(qScreen(., cols = 87:91, smin = 1, smax = 6)) %>%
#'   filter(spi > 1, prMaxSD > .25)
#'
#' # Add a completeness screen, e.g. answered more than 90% of the block:
#' clean <- F24CB.3 %>%
#'   bind_cols(qScreen(., cols = 87:91, smin = 1, smax = 6)) %>%
#'   filter(spi > 1, prMaxSD > .25, prRated > .90)
#' }
qScreen <- function(data, cols, smin, smax, items = NULL,
                    check.names.in = NULL) {

  nm <- colnames(data)

  # --- resolve the metadata block to column indices -----------------------
  if (is.character(cols)) {
    idx <- match(cols, nm)
    if (anyNA(idx)) {
      stop("These `cols` were not found in `data`: ",
           paste(cols[is.na(idx)], collapse = ", "))
    }
  } else {
    idx <- as.integer(cols)
    if (anyNA(idx) || any(idx < 1) || any(idx > ncol(data))) {
      stop("`cols` indices are out of range for `data`.")
    }
  }
  blockNames <- nm[idx]

  # --- optional: validate the block against a column map ------------------
  # `labels` is the source used to identify the three key columns: the colmap's
  # authoritative `sub` labels when supplied, otherwise the data column names.
  labels <- blockNames
  if (!is.null(check.names.in)) {
    cmap <- check.names.in
    if (!all(c("qname", "sub") %in% names(cmap))) {
      stop("`check.names.in` must be a column map with `qname` and `sub` ",
           "columns (as produced by colmap()).")
    }
    labels <- as.character(cmap$sub[match(blockNames, cmap$qname)])
    if (anyNA(labels)) {
      stop("These `cols` have no entry in `check.names.in$qname`: ",
           paste(blockNames[is.na(labels)], collapse = ", "))
    }
    expected <- c("Display Order", "First Click", "Last Click",
                  "Page Submit", "Click Count")
    bad <- !(labels %in% expected)
    if (any(bad)) {
      stop("`cols` do not line up with a Qualtrics timing block. ",
           "Expected labels among {", paste(expected, collapse = ", "),
           "}, but column(s) ",
           paste0(blockNames[bad], " = \"", labels[bad], "\"",
                  collapse = ", "),
           " did not match. Check that `cols` points at the right columns.")
    }
  }

  # --- identify Display Order / First Click / Last Click by label ---------
  find <- function(pattern) {
    hit <- grep(pattern, labels, ignore.case = TRUE)
    if (length(hit)) idx[hit[1]] else NA_integer_
  }
  doCol    <- find("display.?order|(^|[^a-z])do([^a-z]|$)")
  firstCol <- find("first.?click")
  lastCol  <- find("last.?click")

  # fall back to canonical Qualtrics export order if labels are uninformative
  if (is.na(doCol) || is.na(firstCol) || is.na(lastCol)) {
    if (length(idx) < 3) {
      stop("Could not identify Display Order / First Click / Last Click from ",
           "`cols`, and fewer than 3 columns were supplied.")
    }
    if (is.na(doCol))    doCol    <- idx[1]
    if (is.na(firstCol)) firstCol <- idx[2]
    if (is.na(lastCol))  lastCol  <- idx[3]
  }

  # --- number of items presented (from Display Order) ---------------------
  do <- as.character(data[[doCol]])
  nByRow <- ifelse(is.na(do) | do == "",
                   0L,
                   lengths(strsplit(do, "|", fixed = TRUE)))
  nPresented <- max(nByRow)
  if (nPresented < 2) {
    stop("Display Order column (", nm[doCol],
         ") does not contain a parseable list of items.")
  }

  # --- locate the rated-item columns --------------------------------------
  if (is.null(items)) {
    blockStart <- min(idx)
    itemIdx <- (blockStart - nPresented):(blockStart - 1)
    if (itemIdx[1] < 1) {
      stop("Inferred item columns fall before the start of `data`; ",
           "pass `items` explicitly.")
    }
  } else if (is.character(items)) {
    itemIdx <- match(items, nm)
    if (anyNA(itemIdx)) {
      stop("These `items` were not found in `data`: ",
           paste(items[is.na(itemIdx)], collapse = ", "))
    }
  } else {
    itemIdx <- as.integer(items)
  }
  itemMat <- data.matrix(data[, itemIdx, drop = FALSE])

  # --- compute the two indicators -----------------------------------------
  nItemsForSpi <- ifelse(nByRow >= 2, nByRow, NA_integer_)
  speed <- spi(t_first = as.numeric(data[[firstCol]]),
               t_last  = as.numeric(data[[lastCol]]),
               n_items = nItemsForSpi)

  # Rows with no (or one) item rated legitimately yield NA here; prMaxSD's
  # internal sd() warns "NaNs produced" on those, which is expected noise for a
  # screening pass over data that includes non-respondents -- so muffle it.
  variability <- suppressWarnings(prMaxSD(itemMat, smin = smin, smax = smax))
  nRated <- rowSums(is.finite(itemMat))

  data.frame(
    nPresented = nPresented,
    nRated     = nRated,
    prRated    = nRated / nPresented,
    spi        = speed,
    prMaxSD    = variability
  )
}
