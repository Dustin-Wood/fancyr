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
#'   frames. Defaults to \code{"id"}.
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
#' \item{retest_rs}{Retest correlation matrix for \code{commonitems} between T1 and T2.}
#' \item{xEffects}{Output of \code{\link{allYstabilities}}: the long-format stability
#'   path decomposition, per-item coefficients, sample sizes, and the per-item
#'   per-item model results in \code{$modelEstimates}.}
#' \item{measurementInterval}{Data frame of per-person measurement intervals in days, or \code{NULL} if \code{date_col} is not provided.}
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

  # Compute retest correlations
  retest_rs <- psych::corr.test(
    merged[, item_cols_t1, drop = FALSE],
    merged[, item_cols_t2, drop = FALSE],
    use = "complete.obs", method = "pearson", ci = FALSE
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

  # Compute measurement interval if date_col provided
  measurementInterval <- NULL
  if (!is.null(date_col)) {
    dates_merged <- merge(T1_dates, T2_dates, by = id_col)
    dates_merged$date_T1_parsed <- lubridate::parse_date_time(
      dates_merged$date_T1, orders = c("mdy HM", "ymd HMS")
    )
    dates_merged$date_T2_parsed <- lubridate::parse_date_time(
      dates_merged$date_T2, orders = c("mdy HM", "ymd HMS")
    )
    dates_merged$interval_days <- as.numeric(
      difftime(dates_merged$date_T2_parsed, dates_merged$date_T1_parsed,
               units = "days")
    )
    measurementInterval <- dates_merged[, c(id_col, "interval_days")]
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
