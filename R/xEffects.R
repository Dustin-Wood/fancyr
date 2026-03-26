#' Experience Effects on Longitudinal Change
#' @description
#' Merges two-timepoint data, computes retest correlations for common items,
#' merges in an experience variable, and calls \code{\link{medXonAllY}} to
#' estimate selection and change effects for each item.
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
#' @param xFile Data frame containing the experience variable. Must include
#'   \code{id_col}.
#' @param xVar Name of the experience variable column in \code{xFile}.
#' @param id_col Name of the participant ID column present in all three data
#'   frames. Defaults to \code{"id"}.
#' @param date_col Name of a date/datetime column in \code{T1_data} and
#'   \code{T2_data} used to compute the measurement interval. Set to
#'   \code{NULL} (default) to skip interval calculation.
#' @param zY Logical. Passed to \code{\link{medXonAllY}}. Defaults to
#'   \code{FALSE}.
#' @param zX Logical. Passed to \code{\link{medXonAllY}}. Defaults to
#'   \code{FALSE}.
#' @param controls Character vector of control variable column names in
#'   \code{T1_data}. When provided, these variables are carried through the
#'   merge and passed to \code{\link{medXonAllY}} as additional predictors of
#'   both X and Y2 in each item's model. Defaults to \code{NULL} (no controls).
#' @param NA_to_0 Logical. If \code{TRUE}, recode the experience variable so
#'   that values equal to 1 remain 1 and all other values (including \code{NA})
#'   become 0. Useful when the variable encodes presence/absence of an
#'   experience. Defaults to \code{FALSE}.
#'
#' @return A named list with the following components:
#' \item{retest_rs}{Retest correlation matrix for \code{commonitems} between T1 and T2.}
#' \item{xEffects}{Output of \code{\link{medXonAllY}}: selection, change, indirect, and residual stability effects.}
#' \item{measurementInterval}{Data frame of per-person measurement intervals in days, or \code{NULL} if \code{date_col} is not provided.}
#'
#' @export
#' @importFrom psych corr.test
#' @importFrom lubridate parse_date_time
xEffects <- function(T1_data, T2_data, commonitems, xFile, xVar,
                     id_col = "id", date_col = NULL, controls = NULL,
                     zY = FALSE, zX = FALSE, NA_to_0 = FALSE) {

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

  # Merge experience variable
  xFile_sub <- xFile[, c(id_col, xVar), drop = FALSE]
  merged <- merge(merged, xFile_sub, by = id_col, all.x = TRUE)

  # Optionally recode experience variable: 1 stays 1, everything else -> 0
  if (NA_to_0) {
    merged[[xVar]] <- ifelse(!is.na(merged[[xVar]]) & merged[[xVar]] == 1, 1, 0)
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
  xEff <- medXonAllY(
    data     = merged,
    items    = commonitems,
    X        = xVar,
    controls = controls,
    zY       = zY,
    zX       = zX
  )

  list(
    retest_rs           = retest_rs,
    xEffects            = xEff,
    measurementInterval = measurementInterval
  )
}
