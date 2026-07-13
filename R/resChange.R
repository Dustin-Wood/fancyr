#' Residualized Change Scores for a Set of Parallel Variables
#'
#' @description
#' For each variable in a set, regresses its score in \code{T2_data} (the
#' scores to be residualized) on the same-named score in \code{T1_data} (the
#' parallel scores to control for) plus a common set of control variables from
#' \code{cFile}, and returns the residuals as residualized change scores. The
#' parallel (T1) predictor shifts with each variable while the controls stay
#' fixed across all models:
#'
#' \preformatted{  x[T2] ~ x[T1] + gender + ethnicity + SAT ...   (for each x)}
#'
#' The three data frames are matched on \code{id_col}. Variables are paired by
#' column name: by default every non-ID column name common to \code{T1_data}
#' and \code{T2_data} is residualized, or supply \code{items} to select a
#' subset. Likewise every non-ID column of \code{cFile} is used as a control
#' unless \code{controls} names a subset.
#'
#' Each model is fit with \code{lm()} using \code{na.exclude}, and results are
#' aligned to \code{T2_data}: \code{$residuals} has one row per row of
#' \code{T2_data}, in the same order, so it can be \code{cbind()}-ed straight
#' back. Anyone missing their T2 score, T1 score, or any control (including
#' having no matching row in \code{T1_data} or \code{cFile}) gets \code{NA}
#' for that variable (listwise within each model).
#'
#' @param T2_data Data frame of scores to be residualized. Must include
#'   \code{id_col}.
#' @param T1_data Data frame of parallel scores to control for, with the same
#'   column names as \code{T2_data} for the variables in the set. Must include
#'   \code{id_col}.
#' @param cFile Data frame of control variables. Must include \code{id_col}.
#'   \code{NULL} (default) residualizes on the parallel T1 scores only.
#' @param items Character vector of variable names to residualize. Defaults to
#'   all non-ID column names present in both \code{T1_data} and \code{T2_data}.
#' @param controls Character vector of control variable column names in
#'   \code{cFile}. Defaults to all non-ID columns of \code{cFile}. Controls
#'   with fewer than 2 observed values (constant or all-NA columns) are dropped
#'   with a warning. Categorical controls (factor or character, e.g. ethnicity)
#'   are supported and entered as dummy variables; their coefficients appear as
#'   \code{NA} in \code{$summary} since they don't reduce to a single slope.
#' @param id_col Name of the participant ID column present in all supplied
#'   data frames. Defaults to \code{"id"}.
#' @param zResid Logical. If \code{TRUE}, z-standardize each column of
#'   residuals (mean 0, SD 1 among non-missing rows). Defaults to \code{FALSE},
#'   which leaves residuals on the T2 metric.
#'
#' @return A named list with the following components:
#' \item{residuals}{Data frame of residualized change scores with \code{id_col}
#'   as its first column, one row per row of \code{T2_data} (same order) and
#'   one column per variable in \code{items}.}
#' \item{summary}{Data frame with one row per variable: the T1 coefficient and
#'   p-value (\code{b_Y1}, \code{b_Y1_p}), each control's coefficient and
#'   p-value, \code{R2}, and \code{n} (observations used in that model).}
#'
#' @export
#' @importFrom stats lm as.formula na.exclude resid sd
#'
#' @examples
#' \dontrun{
#' rc <- resChange(T2_data = F25CB.4,
#'                 T1_data = P25CB.4,
#'                 cFile   = demographics,
#'                 items    = NL110.F25set,
#'                 controls = c("genderNum", "SAT Math"),
#'                 id_col   = "Random Id")
#' head(rc$residuals)
#' rc$summary
#'
#' # Residualize on the parallel T1 scores only (no control file)
#' rc <- resChange(F25CB.4, P25CB.4, id_col = "Random Id", zResid = TRUE)
#' }
resChange <- function(T2_data, T1_data, cFile = NULL,
                      items = NULL, controls = NULL,
                      id_col = "id", zResid = FALSE) {

  # ── 1. Validate IDs and resolve variable/control sets ────────────────────────
  for (nm in c("T2_data", "T1_data", if (!is.null(cFile)) "cFile")) {
    if (!id_col %in% names(get(nm)))
      stop("id_col '", id_col, "' not found in ", nm, ".")
  }
  if (is.null(cFile) && !is.null(controls))
    stop("'controls' supplied without a cFile to take them from.")

  if (is.null(items)) {
    items <- setdiff(intersect(names(T2_data), names(T1_data)), id_col)
    if (length(items) == 0)
      stop("No common variable names found between T1_data and T2_data.")
  }

  if (!is.null(cFile)) {
    if (is.null(controls)) controls <- setdiff(names(cFile), id_col)
    missing_ctrl <- setdiff(controls, names(cFile))
    if (length(missing_ctrl) > 0)
      stop("Control variable(s) not found in cFile: ",
           paste(missing_ctrl, collapse = ", "))

    # Constant or all-NA controls would make every lm fail with an obscure
    # contrasts error; drop them up front instead.
    degenerate <- vapply(controls, function(cn) {
      v <- cFile[[cn]]
      length(unique(v[!is.na(v)])) < 2
    }, logical(1))
    if (any(degenerate)) {
      warning("Dropping control(s) with fewer than 2 observed values: ",
              paste(controls[degenerate], collapse = ", "))
      controls <- controls[!degenerate]
    }
    if (length(controls) == 0) controls <- NULL
  }

  # ── 2. Match T1 and control rows to T2_data's rows (order-preserving) ────────
  ids <- T2_data[[id_col]]

  match_file <- function(df, nm) {
    if (anyDuplicated(df[[id_col]]))
      warning("Duplicated IDs in ", nm, "; first occurrence used.")
    match(ids, df[[id_col]])
  }

  t1_rows <- match_file(T1_data, "T1_data")
  t1_vals <- T1_data[t1_rows, , drop = FALSE]

  c_vals <- if (!is.null(controls)) {
    c_rows <- match_file(cFile, "cFile")
    out <- cFile[c_rows, controls, drop = FALSE]
    names(out) <- gsub("[^A-Za-z0-9_.]", "_", controls)  # lm-safe placeholder names
    out
  } else {
    NULL
  }
  ctrl_safe <- names(c_vals)

  fml <- stats::as.formula(paste("Y2 ~", paste(c("Y1", ctrl_safe), collapse = " + ")))

  # ── 3. Fit one lm per variable, collecting aligned residuals ─────────────────
  n_vars  <- length(items)
  res_mat <- matrix(NA_real_, nrow = nrow(T2_data), ncol = n_vars,
                    dimnames = list(NULL, items))
  sum_rows <- vector("list", n_vars)

  for (i in seq_len(n_vars)) {
    item <- items[i]

    if (!item %in% names(T2_data) || !item %in% names(T1_data)) {
      warning("Column '", item, "' not found in both T1_data and T2_data. Skipping.")
      next
    }

    d <- data.frame(Y2 = T2_data[[item]], Y1 = t1_vals[[item]])
    if (!is.null(c_vals)) d <- cbind(d, c_vals)

    fit <- tryCatch(
      stats::lm(fml, data = d, na.action = stats::na.exclude),
      error = function(e) {
        warning("Model failed for '", item, "': ", e$message)
        NULL
      }
    )
    if (is.null(fit)) next

    r <- stats::resid(fit)   # na.exclude pads to nrow(T2_data)
    if (zResid) {
      s <- stats::sd(r, na.rm = TRUE)
      if (isTRUE(s > 0)) r <- (r - mean(r, na.rm = TRUE)) / s
    }
    res_mat[, i] <- r

    co <- summary(fit)$coefficients
    get_coef <- function(term) {
      if (term %in% rownames(co)) c(co[term, "Estimate"], co[term, "Pr(>|t|)"])
      else c(NA_real_, NA_real_)
    }

    b_y1 <- get_coef("Y1")
    row  <- data.frame(item   = item,
                       b_Y1   = b_y1[1],
                       b_Y1_p = b_y1[2],
                       row.names = NULL)
    for (cn in ctrl_safe) {
      b_c <- get_coef(cn)
      row[[cn]]               <- b_c[1]
      row[[paste0(cn, "_p")]] <- b_c[2]
    }
    row$R2 <- summary(fit)$r.squared
    row$n  <- stats::nobs(fit)

    sum_rows[[i]] <- row
  }

  # ── 4. Assemble output ───────────────────────────────────────────────────────
  residuals_df <- cbind(T2_data[, id_col, drop = FALSE],
                        as.data.frame(res_mat))

  summary_df <- do.call(rbind, sum_rows)
  rownames(summary_df) <- NULL

  list(residuals = residuals_df,
       summary   = summary_df)
}
