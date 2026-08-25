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

  list(
    paths          = paths,
    coefficients   = coefficients,
    totalStability = totalStability,
    nobs           = data.frame(
      item = items,
      n    = vapply(items, function(i) if (ok(fits[[i]])) as.integer(fits[[i]]$n)
                                       else NA_integer_, integer(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE),
    status         = data.frame(
      item   = items,
      status = vapply(items, function(i) {
                 s <- fits[[i]]$status
                 if (is.null(s)) "Skipped: no result" else s
               }, character(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE),
    modelEstimates = if (return_estimates) fits else NULL
  )
}
