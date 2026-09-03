#' Run One Model Across a Whole Set of Y Variables
#' @description
#' Fits the same \code{\link{fancyModel}} once per variable in a set, sliding
#' each variable into the model's sliding role(s) while every other variable
#' stays fixed. Results are stacked into long-format data frames with an
#' \code{item} column.
#'
#' This is the generic engine behind \code{\link{allYstabilities}}. Use it
#' directly whenever you have a lavaan model you want to run across a large set
#' of parallel outcomes.
#'
#' @details
#' For each item, the column bound to each sliding role is built by pasting the
#' corresponding entry of \code{suffixes} onto the item's base name. A two-wave
#' model uses \code{suffixes = c(Y1 = "[T1]", Y2 = "[T2]")}; a model with a
#' single sliding outcome uses something like \code{suffixes = c(Y = "")}, in
#' which case the item names are the column names themselves.
#'
#' Items whose columns are absent, or whose model errors or fails to converge,
#' contribute \code{NA} rows rather than being dropped, so every returned data
#' frame stays rectangular across items. The \code{$status} component records
#' what happened to each item, following the same convention as
#' \code{\link{allICCs}}.
#'
#' \code{$summary} carries the same numbers in wide form, one row per item. Each
#' extracted parameter contributes a column named after the spec's \code{path}
#' annotation (or its lavaan label, if the spec declares no \code{path} column),
#' and each structural coefficient a column named \code{lhs_on_rhs} for a
#' regression or \code{lhs_with_rhs} for a covariance; every such column is
#' followed by its \code{_p} partner. A sliding role appears in those names by
#' its role name (\code{Y1}, \code{Y2}), since the column bound to it changes
#' from item to item; a fixed variable appears under its own name. For a
#' one-mediator stability model that gives \code{residual}, \code{via_<X>} and
#' \code{total} from the decomposition, plus \code{<X>_on_Y1} (selection),
#' \code{Y2_on_<X>} (change) and \code{Y2_on_Y1} (residual stability) from the
#' structural coefficients.
#'
#' @param spec A \code{\link{fancyModel}} object.
#' @param data A data frame containing the item columns and every column named
#'   in \code{spec$vars}.
#' @param items Character vector of item base names.
#' @param suffixes Named character vector giving the suffix appended to each
#'   item base name to form the column for each sliding role. Names must match
#'   \code{spec$slide}. Defaults to \code{c(Y1 = "[T1]", Y2 = "[T2]")}.
#' @param standardize Logical. If \code{TRUE}, z-standardize every bound column
#'   within each item's model. Defaults to \code{FALSE}.
#' @param return_estimates Logical. If \code{TRUE} (default), include the full
#'   per-item \code{\link{fitModel}} results in \code{$modelEstimates}.
#'
#' @return A named list with components:
#' \item{paths}{Long data frame, one row per item per extracted parameter,
#'   carrying the annotation columns declared in \code{spec$extract} plus
#'   \code{est}, \code{se}, \code{pvalue}, \code{ci.lower}, \code{ci.upper},
#'   and \code{propTotal} when the spec defines a total.}
#' \item{coefficients}{Long data frame of structural coefficients for every
#'   item, labelled with the original variable names.}
#' \item{summary}{Wide data frame, one row per item: \code{item}, \code{n},
#'   \code{status}, then an estimate column and a \code{_p} column for every
#'   extracted parameter and every structural coefficient. Same numbers as
#'   \code{$paths} and \code{$coefficients}, pivoted for reading across items;
#'   see Details for the column naming.}
#' \item{totalStability}{Data frame of the total per item, if the spec defines
#'   one; otherwise \code{NULL}.}
#' \item{nobs}{Data frame of sample sizes per item.}
#' \item{status}{Data frame with one row per item: \code{"Success"} or a short
#'   description of why that item was skipped.}
#' \item{modelEstimates}{Named list of per-item \code{fitModel} results, or
#'   \code{NULL} if \code{return_estimates = FALSE}.}
#'
#' @seealso \code{\link{fancyModel}}, \code{\link{fitModel}},
#'   \code{\link{allYstabilities}}
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' d <- data.frame(
#'   `a[T1]` = rnorm(n), `b[T1]` = rnorm(n), G = rnorm(n), check.names = FALSE
#' )
#' d$`a[T2]` <- 0.5 * d$`a[T1]` + 0.2 * d$G + rnorm(n)
#' d$`b[T2]` <- 0.4 * d$`b[T1]` + 0.2 * d$G + rnorm(n)
#'
#' spec <- fancyModel(
#'   syntax  = "Y2 ~ b21*Y1 + bG*G\nY1 ~~ G",
#'   slide   = c("Y1", "Y2"),
#'   vars    = c(G = "G"),
#'   extract = data.frame(label = c("b21", "bG"),
#'                        path  = c("residual", "G_on_Y2"))
#' )
#' modelOnAllY(spec, d, items = c("a", "b"))$paths
#'
#' @export
modelOnAllY <- function(spec, data, items,
                        suffixes = c(Y1 = "[T1]", Y2 = "[T2]"),
                        standardize = FALSE, return_estimates = TRUE) {

  if (!inherits(spec, "fancyModel"))
    stop("`spec` must be a fancyModel object (see ?fancyModel).")
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!length(items)) stop("`items` must name at least one item.")
  items <- as.character(items)

  if (is.null(names(suffixes)) || any(!nzchar(names(suffixes))))
    stop("`suffixes` must be a named vector, e.g. c(Y1 = \"[T1]\", Y2 = \"[T2]\").")
  need <- setdiff(spec$slide, names(suffixes))
  if (length(need))
    stop("`suffixes` is missing the sliding role(s): ", paste(need, collapse = ", "),
         ". The model slides ", paste(spec$slide, collapse = ", "), ".")
  suffixes <- suffixes[spec$slide]

  ## ---- NA scaffold, so skipped items keep the output rectangular ----------
  na_paths <- spec$extract
  na_paths$label <- NULL
  for (cl in c("est", "se", "pvalue", "ci.lower", "ci.upper"))
    na_paths[[cl]] <- NA_real_
  has_total <- "type" %in% names(spec$extract) && any(spec$extract$type == "total")
  if (has_total) na_paths$propTotal <- NA_real_

  fits <- lapply(items, function(item) {
    bind <- stats::setNames(paste0(item, suffixes), names(suffixes))

    absent <- setdiff(bind, names(data))
    if (length(absent)) {
      warning("Columns not found for item: ", item, ". Skipping.")
      return(list(status = paste0("Skipped: column(s) not found (",
                                  paste(absent, collapse = ", "), ")")))
    }

    res <- tryCatch(
      fitModel(spec, data, bind = bind, standardize = standardize),
      error = function(e) {
        message("Model failed for item '", item, "': ", conditionMessage(e))
        list(status = paste("Model error:", conditionMessage(e)))
      }
    )
    res
  })
  names(fits) <- items

  ok <- function(r) isTRUE(r$converged)

  ## ---- long paths ---------------------------------------------------------
  paths <- do.call(rbind, lapply(items, function(item) {
    r   <- fits[[item]]
    blk <- if (ok(r)) r$paths else na_paths
    cbind(item = item, blk, stringsAsFactors = FALSE)
  }))
  rownames(paths) <- NULL

  ## ---- long structural coefficients ---------------------------------------
  coef_blocks <- lapply(items, function(item) {
    r <- fits[[item]]
    if (!ok(r) || is.null(r$coefficients) || !nrow(r$coefficients)) return(NULL)
    cbind(item = item, r$coefficients, stringsAsFactors = FALSE)
  })
  coef_blocks  <- Filter(Negate(is.null), coef_blocks)
  coefficients <- if (length(coef_blocks)) do.call(rbind, coef_blocks) else NULL
  if (!is.null(coefficients)) rownames(coefficients) <- NULL

  ## ---- per-item scalars ---------------------------------------------------
  totalStability <- if (has_total) {
    tot <- paths[paths$type == "total", ]
    out <- data.frame(item = tot$item, est = tot$est, se = tot$se,
                      pvalue = tot$pvalue, ci.lower = tot$ci.lower,
                      ci.upper = tot$ci.upper, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    out
  } else NULL

  nobs <- data.frame(
    item = items,
    n    = vapply(items, function(i) if (ok(fits[[i]])) as.integer(fits[[i]]$n)
                                     else NA_integer_, integer(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE)

  status <- data.frame(
    item   = items,
    status = vapply(items, function(i) {
               s <- fits[[i]]$status
               if (is.null(s)) "Skipped: no result" else s
             }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE)

  ## ---- wide one-row-per-item summary --------------------------------------
  # A convenience view: everything in $paths and $coefficients, pivoted so each
  # item is a row and each quantity a pair of columns (estimate, `_p`). Nothing
  # here is new information; it is the shape that reads well in a spreadsheet
  # and that sorting/filtering across items wants.

  # Extracted-parameter stems: the spec's `path` annotation when it has one,
  # otherwise the lavaan label.
  path_stems <- if ("path" %in% names(spec$extract)) as.character(spec$extract$path)
                else spec$extract$label
  path_stems <- make.unique(path_stems)

  # Coefficient stems. A sliding role keeps its role name (Y1, Y2), since the
  # column bound to it differs per item; a fixed role keeps its own variable
  # name, which is constant across items. Either way the stem is stable, which
  # is what lets one column mean the same thing down the whole table.
  coef_stems <- function(cf, varmap) {
    role <- varmap$internal[match(cf$lhs, varmap$original)]
    lhs  <- ifelse(!is.na(role) & role %in% spec$slide, role, cf$lhs)
    role <- varmap$internal[match(cf$rhs, varmap$original)]
    rhs  <- ifelse(!is.na(role) & role %in% spec$slide, role, cf$rhs)
    paste0(lhs, ifelse(cf$op == "~", "_on_", "_with_"), rhs)
  }

  ok_items  <- items[vapply(items, function(i) ok(fits[[i]]), logical(1))]
  ref       <- if (length(ok_items)) fits[[ok_items[1]]] else NULL
  coef_cols <- if (!is.null(ref) && !is.null(ref$coefficients) &&
                   nrow(ref$coefficients))
                 make.unique(coef_stems(ref$coefficients, ref$varmap))
               else character(0)

  blank <- function(nc, nms) matrix(NA_real_, length(items), nc,
                                    dimnames = list(NULL, nms))
  path_est <- blank(length(path_stems), path_stems)
  path_p   <- path_est
  coef_est <- blank(length(coef_cols), coef_cols)
  coef_p   <- coef_est

  for (i in seq_along(items)) {
    r <- fits[[items[i]]]
    if (!ok(r)) next
    # $paths rows come back in spec$extract order for every item, including the
    # NA scaffold, so position is a safe key here.
    path_est[i, ] <- r$paths$est
    path_p[i, ]   <- r$paths$pvalue
    if (length(coef_cols) && !is.null(r$coefficients) && nrow(r$coefficients)) {
      hit <- match(coef_cols, coef_stems(r$coefficients, r$varmap))
      coef_est[i, ] <- r$coefficients$est[hit]
      coef_p[i, ]   <- r$coefficients$pvalue[hit]
    }
  }

  weave <- function(est, pv) {
    if (!ncol(est)) return(NULL)
    cols <- vector("list", 2L * ncol(est))
    nms  <- character(2L * ncol(est))
    for (j in seq_len(ncol(est))) {
      cols[[2L * j - 1L]] <- est[, j]; nms[2L * j - 1L] <- colnames(est)[j]
      cols[[2L * j]]      <- pv[, j];  nms[2L * j]      <- paste0(colnames(est)[j], "_p")
    }
    stats::setNames(as.data.frame(cols, stringsAsFactors = FALSE), nms)
  }

  summary_df <- data.frame(item = items, n = nobs$n, status = status$status,
                           stringsAsFactors = FALSE)
  for (blk in list(weave(path_est, path_p), weave(coef_est, coef_p)))
    if (!is.null(blk)) summary_df <- cbind(summary_df, blk)
  rownames(summary_df) <- NULL

  list(
    paths          = paths,
    coefficients   = coefficients,
    summary        = summary_df,
    totalStability = totalStability,
    nobs           = nobs,
    status         = status,
    modelEstimates = if (return_estimates) fits else NULL
  )
}
