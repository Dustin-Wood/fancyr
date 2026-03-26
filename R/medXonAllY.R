#' Mediation of Experience (X) on Change in All Y Variables
#' @description
#' For each item in \code{items}, fits a lavaan SEM to estimate the selection
#' effect (Y1 predicts X), the change effect (X predicts Y2 controlling for Y1),
#' and the indirect path from Y1 to Y2 through X (Y1 -> X -> Y2). Y1's direct
#' effect on Y2 controlling for X is the residual stability.
#'
#' The model treats X as a mediator of longitudinal change: selection into
#' experiences (bX1) and experience-driven change (b2X) combine to form an
#' indirect effect (bX1 * b2X). This partitions the Y1-Y2 stability coefficient
#' into a portion mediated by X and a residual direct path.
#'
#' Optionally, a set of control variables can be included as additional
#' predictors of both X and Y2, partialling their effects from the focal
#' selection and change coefficients.
#'
#' @param data A data frame containing all variables. Item columns should have
#'   \code{y1ind} and \code{y2ind} suffixes (e.g., \code{"item[T1]"} and
#'   \code{"item[T2]"}).
#' @param items Character vector of item base names (without T1/T2 suffixes).
#' @param X Name of the experience variable column in \code{data}.
#' @param y1ind Suffix identifying T1 item columns. Defaults to \code{"[T1]"}.
#' @param y2ind Suffix identifying T2 item columns. Defaults to \code{"[T2]"}.
#' @param controls Character vector of control variable column names in
#'   \code{data}. When provided, these variables are added as predictors of
#'   both X and Y2 in each item's model. Defaults to \code{NULL} (no controls).
#' @param zY Logical. If \code{TRUE}, standardize Y1 and Y2 columns before
#'   fitting models. Defaults to \code{FALSE}.
#' @param zX Logical. If \code{TRUE}, standardize the X column before fitting
#'   models. Defaults to \code{FALSE}.
#'
#' @return A named list with the following components:
#' \item{Y1onX_bX1}{Data frame of selection effects (Y1 -> X) for each item.}
#' \item{XonY2_b2X}{Data frame of change effects (X -> Y2 | Y1) for each item.}
#' \item{IndirectX_b2X1}{Data frame of indirect effects (Y1 -> X -> Y2) for each item.}
#' \item{resStability_b21.X}{Data frame of residual stability (Y1 -> Y2 | X) for each item.}
#' \item{nobs}{Data frame of sample sizes for each item model.}
#' \item{summary}{Wide data frame combining all effects for each item.}
#'
#' @export
#' @importFrom lavaan sem parameterestimates nobs
#' @importFrom purrr imap_dfr
medXonAllY <- function(data, items, X, y1ind = "[T1]", y2ind = "[T2]",
                       controls = NULL, zY = FALSE, zX = FALSE) {

  if (zX) data[[X]] <- scale(data[[X]])[, 1]

  # Sanitize control names for lavaan (replace non-alphanumeric with _).
  # Lavaan does not support backtick-quoting or spaces in variable names.
  ctrl_safe <- if (!is.null(controls)) {
    gsub("[^A-Za-z0-9_.]", "_", controls)
  } else {
    NULL
  }

  ctrl_str <- if (!is.null(ctrl_safe)) {
    paste("+", paste(ctrl_safe, collapse = " + "))
  } else {
    ""
  }

  results <- purrr::imap_dfr(items, function(item, idx) {
    y1col <- paste0(item, y1ind)
    y2col <- paste0(item, y2ind)

    if (!y1col %in% names(data) || !y2col %in% names(data)) {
      warning("Columns not found for item: ", item, ". Skipping.")
      return(NULL)
    }

    d_cols <- c(y1col, y2col, X, controls)
    d <- data[, d_cols, drop = FALSE]
    names(d)[1:3] <- c("Y1", "Y2", "X")
    # Rename control columns to sanitized names for lavaan
    if (!is.null(controls)) names(d)[4:ncol(d)] <- ctrl_safe

    if (zY) {
      d$Y1 <- scale(d$Y1)[, 1]
      d$Y2 <- scale(d$Y2)[, 1]
    }

    model <- paste0("
      X ~ bX1 * Y1", ctrl_str, "
      Y2 ~ b2X * X + b21.X * Y1", ctrl_str, "
      indirect := bX1 * b2X
    ")

    fit <- tryCatch(
      lavaan::sem(model, data = d, missing = "fiml"),
      error = function(e) { message("Model failed for item '", item, "': ", e$message); NULL }
    )

    if (is.null(fit)) {
      return(data.frame(
        item = item,
        bX1 = NA, bX1_p = NA,
        b2X = NA, b2X_p = NA,
        indirect = NA, indirect_p = NA,
        b21.X = NA, b21.X_p = NA,
        n = NA
      ))
    }

    pe <- lavaan::parameterestimates(fit)
    get_est <- function(lbl) {
      row <- pe[pe$label == lbl, ]
      if (nrow(row) == 0) return(c(est = NA_real_, p = NA_real_))
      c(est = row$est[1], p = row$pvalue[1])
    }

    bX1   <- get_est("bX1")
    b2X   <- get_est("b2X")
    indir <- get_est("indirect")
    b21.X <- get_est("b21.X")

    data.frame(
      item       = item,
      bX1        = bX1["est"],
      bX1_p      = bX1["p"],
      b2X        = b2X["est"],
      b2X_p      = b2X["p"],
      indirect   = indir["est"],
      indirect_p = indir["p"],
      b21.X      = b21.X["est"],
      b21.X_p    = b21.X["p"],
      n          = lavaan::nobs(fit),
      row.names  = NULL
    )
  })

  list(
    Y1onX_bX1          = results[, c("item", "bX1", "bX1_p")],
    XonY2_b2X          = results[, c("item", "b2X", "b2X_p")],
    IndirectX_b2X1     = results[, c("item", "indirect", "indirect_p")],
    resStability_b21.X = results[, c("item", "b21.X", "b21.X_p")],
    nobs               = results[, c("item", "n")],
    summary            = results
  )
}
