#' Fit a fancyModel Specification to One Set of Variables
#' @description
#' Binds columns to the roles named in a \code{\link{fancyModel}} spec, fits the
#' model with \code{\link[lavaan]{sem}}, and returns tidy output labelled with
#' the original variable names.
#'
#' @details
#' Columns are copied into a working frame and renamed to their role names
#' before fitting, so variable names that lavaan cannot parse (spaces,
#' punctuation, bracket suffixes such as \code{"item[T1]"}) are handled
#' transparently. The mapping is recorded in \code{$varmap} and reversed on
#' output, so \code{$coefficients} refers to your columns, not to roles.
#'
#' A model that errors or fails to converge does not stop the caller: the
#' function returns the same list shape with \code{NA} estimates and
#' \code{converged = FALSE}, and \code{$status} explains what happened.
#'
#' @param spec A \code{\link{fancyModel}} object.
#' @param data A data frame containing every column named in \code{bind} and in
#'   \code{spec$vars}.
#' @param bind Named character vector mapping the spec's sliding roles to
#'   columns, e.g. \code{c(Y1 = "item[T1]", Y2 = "item[T2]")}. Every role in
#'   \code{spec$slide} must be present.
#' @param standardize Logical. If \code{TRUE}, z-standardize every bound column
#'   before fitting. Defaults to \code{FALSE}.
#' @param return_fit Logical. If \code{TRUE}, include the fitted lavaan object
#'   in \code{$fit}. Defaults to \code{FALSE}.
#'
#' @return A named list with components:
#' \item{paths}{Data frame of the extracted parameters, carrying any annotation
#'   columns declared in \code{spec$extract}, plus \code{est}, \code{se},
#'   \code{pvalue}, \code{ci.lower}, \code{ci.upper}. A \code{propTotal} column
#'   is added when the spec extracts a row of type \code{"total"}.}
#' \item{coefficients}{Data frame of all structural coefficients
#'   (\code{~} and off-diagonal \code{~~}), labelled with original names.}
#' \item{totalStability}{The \code{"total"} estimate if the spec defines one,
#'   otherwise \code{NA}.}
#' \item{n}{Number of observations used.}
#' \item{converged}{Logical.}
#' \item{status}{\code{"Success"}, or a short description of the failure.}
#' \item{syntax}{The model syntax that was fitted.}
#' \item{varmap}{Data frame mapping role names to original columns.}
#' \item{fit}{The lavaan fit object, if \code{return_fit = TRUE}.}
#'
#' @seealso \code{\link{fancyModel}}, \code{\link{modelOnAllY}},
#'   \code{\link{stabilityPaths}}
#'
#' @export
#' @importFrom lavaan sem parameterestimates nobs lavInspect
#' @importFrom stats setNames
fitModel <- function(spec, data, bind, standardize = FALSE, return_fit = FALSE) {

  if (!inherits(spec, "fancyModel"))
    stop("`spec` must be a fancyModel object (see ?fancyModel).")
  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  ## ---- bind roles to columns ---------------------------------------------
  if (is.null(names(bind)) || any(!nzchar(names(bind))))
    stop("`bind` must be a named vector mapping roles to column names.")
  bind <- stats::setNames(as.character(bind), names(bind))

  need <- setdiff(spec$slide, names(bind))
  if (length(need))
    stop("`bind` is missing the sliding role(s): ", paste(need, collapse = ", "))
  bind <- bind[spec$slide]

  roles   <- c(names(bind), names(spec$vars))
  columns <- c(unname(bind), unname(spec$vars))

  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols))
    stop("Column(s) not found in `data`: ", paste(missing_cols, collapse = ", "))
  if (anyDuplicated(columns))
    stop("A column is bound to more than one role: ",
         paste(unique(columns[duplicated(columns)]), collapse = ", "))

  # Semantic role labels (e.g. "mediator", "control") when the spec supplies
  # them; downstream consumers such as plotMedX dispatch on these.
  role_lbl <- c(rep("slide", length(bind)), rep("fixed", length(spec$vars)))
  if (length(spec$roles)) {
    hit <- match(roles, names(spec$roles))
    role_lbl[!is.na(hit)] <- unname(spec$roles)[hit[!is.na(hit)]]
  }

  varmap <- data.frame(
    internal = roles,
    original = columns,
    role     = role_lbl,
    stringsAsFactors = FALSE
  )

  d <- data[, columns, drop = FALSE]
  names(d) <- roles
  d[] <- lapply(d, as.numeric)
  if (standardize) d[] <- lapply(d, function(z) as.vector(scale(z)))

  ## ---- path scaffold, used for both success and failure -------------------
  path_rows <- spec$extract
  has_total <- "type" %in% names(path_rows) && any(path_rows$type == "total")

  fail <- function(msg) {
    for (cl in c("est", "se", "pvalue", "ci.lower", "ci.upper"))
      path_rows[[cl]] <- NA_real_
    if (has_total) path_rows$propTotal <- NA_real_
    path_rows$label <- NULL
    rownames(path_rows) <- NULL
    list(paths = path_rows, coefficients = NULL, totalStability = NA_real_,
         n = NA_integer_, converged = FALSE, status = msg,
         syntax = spec$syntax, varmap = varmap)
  }

  fit <- tryCatch(
    do.call(lavaan::sem,
            c(list(model = spec$syntax, data = d), spec$sem_args)),
    error = function(e)
      structure(list(msg = conditionMessage(e)), class = "fancyFitFail"))
  if (inherits(fit, "fancyFitFail"))
    return(fail(paste("Model error:", fit$msg)))
  if (!lavaan::lavInspect(fit, "converged"))
    return(fail("Skipped: model did not converge"))

  pe <- lavaan::parameterestimates(fit)
  grab <- function(lbl, col) {
    v <- pe[[col]][pe$label == lbl]
    if (!length(v)) NA_real_ else v[1]
  }

  for (cl in c("est", "se", "pvalue", "ci.lower", "ci.upper"))
    path_rows[[cl]] <- vapply(path_rows$label, grab, numeric(1), col = cl)

  total <- if (has_total) path_rows$est[path_rows$type == "total"][1] else NA_real_
  if (has_total) path_rows$propTotal <- path_rows$est / total
  path_rows$label <- NULL
  rownames(path_rows) <- NULL

  ## ---- structural coefficients, relabelled to original names --------------
  lookup <- stats::setNames(varmap$original, varmap$internal)
  keep <- c("lhs", "rhs", "est", "se", "pvalue", "ci.lower", "ci.upper")
  reg <- pe[pe$op == "~", keep]
  cvs <- pe[pe$op == "~~" & pe$lhs != pe$rhs, keep]
  # cbind() on a zero-row frame errors, so tag only the non-empty pieces
  tag <- function(df, opv) if (nrow(df)) cbind(df, op = opv) else NULL
  parts <- Filter(Negate(is.null), list(tag(reg, "~"), tag(cvs, "~~")))
  coefs <- if (length(parts)) do.call(rbind, parts) else cbind(reg, op = character(0))
  coefs$lhs <- unname(lookup[coefs$lhs])
  coefs$rhs <- unname(lookup[coefs$rhs])
  coefs <- coefs[, c("lhs", "op", "rhs", "est", "se", "pvalue",
                     "ci.lower", "ci.upper")]
  rownames(coefs) <- NULL

  out <- list(
    paths          = path_rows,
    coefficients   = coefs,
    totalStability = total,
    n              = lavaan::nobs(fit),
    converged      = TRUE,
    status         = "Success",
    syntax         = spec$syntax,
    varmap         = varmap
  )
  if (return_fit) out$fit <- fit
  out
}
