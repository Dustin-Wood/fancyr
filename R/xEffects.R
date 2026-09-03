#' Experience Effects on Longitudinal Change
#' @description
#' Merges two-timepoint data, computes retest correlations for common items,
#' merges in the experience variable(s), and calls \code{\link{allYstabilities}} to
#' decompose each item's T1-to-T2 stability into mediated, confounded, and
#' residual pathways.
#'
#' T1 and T2 data frames must already have item names as column names. An
#' \code{id_col} column present in both data frames is used for the merge.
#' Optionally, a \code{date_col} can be provided to compute the measurement
#' interval in days.
#'
#' @param T1_data Data frame of Time 1 data. Must include \code{id_col} and
#'   all columns named in \code{commonitems}.
#' @param T2_data Data frame of Time 2 data. Must include \code{id_col} and
#'   all columns named in \code{commonitems}.
#' @param commonitems Character vector of item base names present in both
#'   \code{T1_data} and \code{T2_data}.
#' @param xFile Data frame containing the experience variable(s). Must include
#'   \code{id_col}. Optional; ignored when \code{xVar} is \code{NULL}.
#' @param xVar Character vector naming the experience variable column(s) in
#'   \code{xFile}. More than one may be given, in which case they are fitted as
#'   parallel mediators. Set to \code{NULL} (default) to fit a mediator-free
#'   model, decomposing stability into confounded and residual paths only.
#' @param id_col Name of the participant ID column present in all three data
#'   frames. Defaults to \code{"id"}. Each supplied data frame must have one row
#'   per ID: because every join here is on \code{id_col}, and \code{merge()}
#'   multiplies rows on a duplicated key rather than erroring, duplicate IDs
#'   would silently inflate every sample size. Duplicates therefore raise an
#'   error naming the offending IDs instead of being merged.
#' @param date_col Name of a date/datetime column in \code{T1_data} and
#'   \code{T2_data} used to compute the measurement interval. Set to
#'   \code{NULL} (default) to skip interval calculation.
#' @param standardize Logical. If \code{TRUE}, z-standardize Y1, Y2, X, and
#'   all control variables before fitting models. Passed to
#'   \code{\link{allYstabilities}}. Defaults to \code{FALSE}.
#' @param controls Character vector of control variable column names in
#'   \code{T1_data}. When provided, these variables are carried through the
#'   merge and passed to \code{\link{allYstabilities}}, where each is linked to Y1 by
#'   an undirected covariance and enters as a predictor of Y2 and of every
#'   experience variable. Defaults to \code{NULL} (no controls).
#' @param NA_to_0 Logical. If \code{TRUE}, recode each experience variable so
#'   that values equal to 1 remain 1 and all other values (including \code{NA})
#'   become 0. Note this binarizes the variable, not merely fills its \code{NA}s.
#'   Useful when the variable encodes presence/absence of an experience.
#'   Defaults to \code{FALSE}.
#'
#' @return A named list with the following components:
#' \item{retest_rs}{Retest correlations for \code{commonitems} between T1 and
#'   T2, from \code{\link[psych]{corr.test}} with \code{use = "pairwise"}. Each
#'   correlation uses every case with both of its two variables observed, so
#'   \code{retest_rs$n} is a matrix of per-pair sample sizes rather than a
#'   single number.}
#' \item{xEffects}{Output of \code{\link{allYstabilities}}: the long-format stability
#'   path decomposition, per-item coefficients, sample sizes, the wide
#'   one-row-per-item \code{$summary}, and the per-item model results in
#'   \code{$modelEstimates}.}
#' \item{measurementInterval}{Data frame of per-person measurement intervals in
#'   days, aligned row-for-row to the merged analysis sample, or \code{NULL} if
#'   \code{date_col} is not provided. Anyone whose date is missing or unparseable
#'   gets \code{NA} rather than being dropped.}
#'
#' @export
#' @importFrom psych corr.test
#' @importFrom lubridate parse_date_time
xEffects <- function(T1_data, T2_data, commonitems, xFile = NULL, xVar = NULL,
                     id_col = "id", date_col = NULL, controls = NULL,
                     standardize = FALSE, NA_to_0 = FALSE) {

  xVar <- if (is.null(xVar)) character(0) else as.character(xVar)
  if (length(xVar) && is.null(xFile))
    stop("`xFile` is required when `xVar` names one or more variables.")

  # Every join below is on id_col, and merge() silently multiplies rows on a
  # duplicated key. Refuse to run rather than return inflated sample sizes.
  dup_why <- paste("Merging on a duplicated ID multiplies rows and silently",
                   "inflates every sample size.")
  checkUniqueIDs(T1_data, id_col, "T1_data", why = dup_why)
  checkUniqueIDs(T2_data, id_col, "T2_data", why = dup_why)
  if (length(xVar)) checkUniqueIDs(xFile, id_col, "xFile", why = dup_why)

  # Subset to id + common items (+ controls if provided) for T1
  T1_sub <- T1_data[, c(id_col, commonitems, controls), drop = FALSE]
  T2_sub <- T2_data[, c(id_col, commonitems), drop = FALSE]

  # Apply [T1] / [T2] suffixes to item columns only (not controls)
  item_cols_t1 <- paste0(commonitems, "[T1]")
  item_cols_t2 <- paste0(commonitems, "[T2]")
  names(T1_sub)[names(T1_sub) %in% commonitems] <- item_cols_t1
  names(T2_sub)[names(T2_sub) %in% commonitems] <- item_cols_t2

  # Optionally extract date columns before renaming
  if (!is.null(date_col)) {
    T1_dates <- T1_data[, c(id_col, date_col), drop = FALSE]
    T2_dates <- T2_data[, c(id_col, date_col), drop = FALSE]
    names(T1_dates)[2] <- "date_T1"
    names(T2_dates)[2] <- "date_T2"
  }

  # Merge T1 and T2 by id
  merged <- merge(T1_sub, T2_sub, by = id_col)

  # Compute retest correlations. Pairwise, not listwise: "complete.obs" would
  # drop any case missing ANY item in the set, so the retest rs would be based
  # on a smaller, differently-composed sample than the per-item models, which
  # use FIML. Pairwise keeps each correlation on the cases that actually have
  # its two variables.
  retest_rs <- psych::corr.test(
    merged[, item_cols_t1, drop = FALSE],
    merged[, item_cols_t2, drop = FALSE],
    use = "pairwise", method = "pearson", ci = FALSE
  )

  # Merge experience variable(s), if any
  if (length(xVar)) {
    xmiss <- setdiff(c(id_col, xVar), names(xFile))
    if (length(xmiss))
      stop("Column(s) not found in `xFile`: ", paste(xmiss, collapse = ", "))
    xFile_sub <- xFile[, c(id_col, xVar), drop = FALSE]
    merged <- merge(merged, xFile_sub, by = id_col, all.x = TRUE)

    # Optionally recode each experience variable: 1 stays 1, everything else -> 0
    if (NA_to_0) {
      for (v in xVar)
        merged[[v]] <- ifelse(!is.na(merged[[v]]) & merged[[v]] == 1, 1, 0)
    }
  }

  # Compute measurement interval if date_col provided.
  #
  # Aligned to `merged` via match(), not built by a separate merge() of the two
  # date frames: that would produce its own row set (anyone with dates but
  # without complete item data, in its own sort order), so the intervals would
  # not correspond row-for-row to the analysis sample. IDs are unique here --
  # guarded above -- so match() is unambiguous.
  measurementInterval <- NULL
  if (!is.null(date_col)) {
    ids   <- merged[[id_col]]
    raw1  <- T1_dates$date_T1[match(ids, T1_dates[[id_col]])]
    raw2  <- T2_dates$date_T2[match(ids, T2_dates[[id_col]])]
    date1 <- lubridate::parse_date_time(raw1, orders = c("mdy HM", "ymd HMS"))
    date2 <- lubridate::parse_date_time(raw2, orders = c("mdy HM", "ymd HMS"))

    measurementInterval <- data.frame(
      ids,
      interval_days = as.numeric(difftime(date2, date1, units = "days")),
      stringsAsFactors = FALSE
    )
    names(measurementInterval)[1] <- id_col
  }

  # Run mediation analysis
  xEff <- allYstabilities(
    data        = merged,
    items       = commonitems,
    X           = if (length(xVar)) xVar else NULL,
    controls    = controls,
    standardize = standardize
  )

  list(
    retest_rs           = retest_rs,
    xEffects            = xEff,
    measurementInterval = measurementInterval
  )
}
