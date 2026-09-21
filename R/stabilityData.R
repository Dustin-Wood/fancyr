#' Assemble a Two-Wave Data Frame for Stability Analysis
#' @description
#' Merges Time 1 and Time 2 data, plus any further files (e.g. records of an
#' experience), into the one-row-per-person frame that
#' \code{\link{stabilityPaths}} analyses. Items measured at both waves get
#' \code{[T1]} and \code{[T2]} suffixes; everything else keeps its name.
#'
#' @details
#' What ends up in the result:
#' \itemize{
#'   \item \strong{Items}: every column in \code{items}, from both waves,
#'     renamed \code{item[T1]} and \code{item[T2]}.
#'   \item \strong{Other T1 columns} (e.g. baseline controls such as
#'     demographics) are kept under their own names. Other T2 columns are
#'     dropped; to keep one, pass it (with the ID) through \code{...}.
#'   \item \strong{Extra files} in \code{...} are joined on \code{id} and keep
#'     their column names. They are left-joined: people who appear only in an
#'     extra file are not added. A column name that already exists is an
#'     error, since silently suffixing it would change what \code{X} and
#'     \code{controls} refer to.
#'   \item \strong{interval_days}, when \code{date} is given: days between each
#'     person's two measurements, \code{NA} where either date is missing or
#'     unparseable.
#' }
#'
#' Every input must have one row per ID; duplicates are an error that names
#' them (a duplicated key would otherwise multiply rows in the merge).
#'
#' @section Missing data:
#' By default (\code{join = "full"}) the result keeps everyone measured at
#' \emph{either} wave, including people who dropped out after Time 1. That
#' matters because \code{\link{stabilityPaths}} fits its models by full
#' information maximum likelihood (FIML), which uses every value a person
#' has rather than discarding anyone with a missing value. A person seen only
#' at Time 1 still informs the Time 1 variance and how Time 1 scores relate to
#' the experience and the controls.
#'
#' The payoff is in the assumption required. Analysing only people seen at both
#' waves (\code{join = "inner"}, i.e. listwise deletion) is unbiased only if
#' dropout is \emph{missing completely at random} (MCAR): unrelated to anything,
#' measured or not. FIML is unbiased under the weaker \emph{missing at random}
#' (MAR) assumption: dropout may depend on things that were observed -- such as
#' a person's Time 1 score or a control variable -- as long as, given those, it
#' does not further depend on the unobserved Time 2 score itself. Since people
#' who drop out of longitudinal studies commonly differ at baseline, MAR is
#' usually the far more plausible of the two. Including variables in the model
#' that predict dropout makes MAR more plausible still.
#'
#' Neither approach protects against dropout that depends on the unobserved
#' Time 2 score itself (\emph{missing not at random}); that calls for
#' sensitivity analysis rather than a different default.
#'
#' Key references: Rubin (1976) for the MCAR/MAR/MNAR distinctions; Schafer
#' and Graham (2002) and Graham (2009) for accessible reviews recommending ML
#' and multiple imputation over deletion; Enders and Bandalos (2001) for FIML in
#' structural equation models; Enders (2010) for a book-length treatment; and
#' Collins, Schafer and Kam (2001) on adding variables that predict
#' missingness.
#'
#' @references
#' Collins, L. M., Schafer, J. L., & Kam, C.-M. (2001). A comparison of
#' inclusive and restrictive strategies in modern missing data procedures.
#' \emph{Psychological Methods, 6}(4), 330--351.
#'
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Enders, C. K., & Bandalos, D. L. (2001). The relative performance of full
#' information maximum likelihood estimation for missing data in structural
#' equation models. \emph{Structural Equation Modeling, 8}(3), 430--457.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real
#' world. \emph{Annual Review of Psychology, 60}, 549--576.
#'
#' Rubin, D. B. (1976). Inference and missing data. \emph{Biometrika, 63}(3),
#' 581--592.
#'
#' Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the state
#' of the art. \emph{Psychological Methods, 7}(2), 147--177.
#'
#' @param T1,T2 Data frames of Time 1 and Time 2 data, each including \code{id}.
#' @param ... Further data frames to join on \code{id}, e.g. a file recording
#'   who had an experience.
#' @param id Name of the ID column present in every data frame. Defaults to
#'   \code{"id"}.
#' @param items Character vector of item names measured at both waves. Defaults
#'   to every numeric column (other than \code{id} and \code{date}) that
#'   \code{T1} and \code{T2} share.
#' @param join \code{"full"} (default) keeps everyone present at either wave;
#'   \code{"inner"} keeps only people present at both. See the Missing data
#'   section.
#' @param fill Optional named list of values to replace \code{NA} with, by
#'   column, applied after merging, e.g. \code{list(leader = 0)} when an
#'   experience file lists only the people who had the experience.
#' @param date Optional name of a date or date-time column, present in both
#'   \code{T1} and \code{T2}, used to compute \code{interval_days}. Supply
#'   \code{c(T1 = "start", T2 = "finish")} if the name differs between waves.
#'   Character dates are parsed in month-day-year or year-month-day order,
#'   with or without times.
#'
#' @return A data frame with one row per person, carrying attributes
#'   \code{items} (the item names) and \code{id} (the ID column name), which
#'   \code{\link{stabilityPaths}} uses as defaults.
#'
#' @seealso \code{\link{stabilityPaths}}
#'
#' @examples
#' d <- stabilityData(stabilitySim$T1, stabilitySim$T2, stabilitySim$experience,
#'                    fill = list(leader = 0))
#' attr(d, "items")
#' head(d)
#'
#' # everyone kept: people with no Time 2 data are still in the frame
#' colSums(!is.na(d[, c("dominant[T1]", "dominant[T2]")]))
#'
#' @export
stabilityData <- function(T1, T2, ..., id = "id", items = NULL,
                          join = c("full", "inner"), fill = NULL, date = NULL) {

  join   <- match.arg(join)
  extras <- list(...)
  if (length(extras) && !all(vapply(extras, is.data.frame, logical(1))))
    stop("Everything passed through `...` must be a data frame.")

  dup_why <- paste("Merging on a duplicated ID multiplies rows and silently",
                   "inflates every sample size.")
  checkUniqueIDs(T1, id, "T1", why = dup_why)
  checkUniqueIDs(T2, id, "T2", why = dup_why)
  for (i in seq_along(extras))
    checkUniqueIDs(extras[[i]], id, sprintf("... (data frame %d)", i), why = dup_why)

  ## ---- dates ----------------------------------------------------------------
  date_cols <- NULL
  if (!is.null(date)) {
    date_cols <- if (length(date) == 1L) c(T1 = date, T2 = date) else date
    if (is.null(names(date_cols)) || !all(c("T1", "T2") %in% names(date_cols)))
      stop("`date` must be one column name, or c(T1 = \"...\", T2 = \"...\").")
    if (!date_cols[["T1"]] %in% names(T1))
      stop("Date column \"", date_cols[["T1"]], "\" not found in `T1`.")
    if (!date_cols[["T2"]] %in% names(T2))
      stop("Date column \"", date_cols[["T2"]], "\" not found in `T2`.")
  }

  ## ---- items ----------------------------------------------------------------
  if (is.null(items)) {
    # shared numeric columns only: a shared date or text column is not an item
    shared <- setdiff(intersect(names(T1), names(T2)), c(id, date_cols))
    items  <- shared[vapply(shared, function(v)
      is.numeric(T1[[v]]) && is.numeric(T2[[v]]), logical(1))]
    if (!length(items))
      stop("`T1` and `T2` share no numeric columns besides `id`; name the ",
           "`items` explicitly.")
  } else {
    items <- as.character(items)
    for (w in c("T1", "T2")) {
      absent <- setdiff(items, names(get(w)))
      if (length(absent))
        stop("Item(s) not found in `", w, "`: ", paste(absent, collapse = ", "))
    }
  }

  t1_other <- setdiff(names(T1), c(id, items, date_cols[["T1"]]))
  t1 <- T1[, c(id, items, t1_other), drop = FALSE]
  names(t1)[match(items, names(t1))] <- paste0(items, "[T1]")
  t2 <- T2[, c(id, items), drop = FALSE]
  names(t2)[match(items, names(t2))] <- paste0(items, "[T2]")

  out <- merge(t1, t2, by = id, all = (join == "full"))

  ## ---- extra files ----------------------------------------------------------
  for (i in seq_along(extras)) {
    ex <- extras[[i]]
    clash <- intersect(setdiff(names(ex), id), names(out))
    if (length(clash))
      stop("Column(s) in data frame ", i, " of `...` already exist in the merged ",
           "data: ", paste(clash, collapse = ", "),
           ". Rename or drop them before merging.")
    out <- merge(out, ex, by = id, all.x = TRUE)
  }

  ## ---- fill -----------------------------------------------------------------
  if (!is.null(fill)) {
    if (!is.list(fill) || is.null(names(fill)) || any(!nzchar(names(fill))))
      stop("`fill` must be a named list, e.g. list(leader = 0).")
    absent <- setdiff(names(fill), names(out))
    if (length(absent))
      stop("`fill` names column(s) not in the merged data: ",
           paste(absent, collapse = ", "))
    for (v in names(fill)) out[[v]][is.na(out[[v]])] <- fill[[v]]
  }

  ## ---- measurement interval -------------------------------------------------
  # Aligned to `out` with match(): IDs are unique (checked above), so this is
  # unambiguous and keeps interval_days row-for-row with the analysis sample.
  if (!is.null(date_cols)) {
    ids <- out[[id]]
    d1 <- parseDates(T1[[date_cols[["T1"]]]][match(ids, T1[[id]])])
    d2 <- parseDates(T2[[date_cols[["T2"]]]][match(ids, T2[[id]])])
    out$interval_days <- as.numeric(difftime(d2, d1, units = "days"))
  }

  rownames(out) <- NULL
  attr(out, "items") <- items
  attr(out, "id")    <- id
  out
}

# Dates may arrive as Date/POSIXct or as text in a few common orders.
parseDates <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.POSIXct(x))
  suppressWarnings(lubridate::parse_date_time(
    as.character(x), orders = c("mdy HM", "mdy HMS", "ymd HMS", "ymd HM",
                                "mdy", "ymd")))
}
