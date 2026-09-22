#' Fit a fancyModel Specification to One Set of Variables
#' @description
#' Binds columns to the roles named in a \code{\link{fancyModel}} spec, fits the
#' model with \code{\link[lavaan]{sem}}, and returns tidy output labelled with
#' the original variable names. Any role can optionally be modelled as a latent
#' variable measured by its column with a known reliability.
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
#' @section Correcting for unreliability:
#' Supplying \code{reliability = c(Y1 = .7)} turns role \code{Y1} into a latent
#' variable with its observed column as the single indicator:
#'
#' \preformatted{  Y1     =~ 1*Y1_obs
#'   Y1_obs ~~ e*Y1_obs      # e = (1 - rel) * var(Y1_obs), fixed}
#'
#' The model syntax in the spec is untouched -- the role name simply now refers
#' to the latent variable -- so every structural path is estimated between
#' error-free constructs. Because one free observed variance is exchanged for
#' one free latent variance, a just-identified model stays just-identified, and
#' any decomposition defined in the spec still sums exactly.
#'
#' Fixing the error variance (rather than fixing the loading at
#' \eqn{\sqrt{rel}} with a unit latent variance) gives identical standardized
#' results, but also works for an endogenous role such as \code{Y2}, whose
#' variance is not a free parameter, and keeps raw-metric estimates in the
#' column's own units. The error variance uses the column's variance among its
#' observed cases.
#'
#' Two caveats. Standard errors treat the reliability as known, so they are
#' somewhat optimistic; see \code{\link{reliabilitySensitivity}} for how much the
#' results depend on the value assumed. And a reliability lower than the
#' variable's correlations with other variables imply can produce an
#' inadmissible solution (e.g. a latent correlation above 1, or a negative
#' residual variance). Such fits are returned with their estimates, but with
#' \code{admissible = FALSE} and an explanatory \code{status}.
#'
#' @param spec A \code{\link{fancyModel}} object.
#' @param data A data frame containing every column named in \code{bind} and in
#'   \code{spec$vars}.
#' @param bind Named character vector mapping the spec's sliding roles to
#'   columns, e.g. \code{c(Y1 = "item[T1]", Y2 = "item[T2]")}. Every role in
#'   \code{spec$slide} must be present.
#' @param reliability Optional named numeric vector giving a reliability for any
#'   role, sliding or fixed, e.g. \code{c(Y1 = .7, Y2 = .7, C1 = .9)}. Each role
#'   named with a value below 1 is modelled as a latent variable (see the
#'   section below). Values of 1 or \code{NA} leave the role observed.
#'   Defaults to \code{NULL} (every role observed).
#' @param metric \code{"raw"} (default) reports estimates in the variables' own
#'   units; \code{"std"} reports fully standardized estimates
#'   (\code{\link[lavaan]{standardizedSolution}}, \code{type = "std.all"}), with
#'   delta-method standard errors. With latent roles, \code{"std"} standardizes
#'   the latent variables, so correlations are disattenuated.
#' @param return_fit Logical. If \code{TRUE}, include the fitted lavaan object
#'   in \code{$fit}. Defaults to \code{FALSE}.
#'
#' @return A named list with components:
#' \item{paths}{Data frame of the extracted parameters, carrying any annotation
#'   columns declared in \code{spec$extract}, plus \code{est}, \code{se},
#'   \code{pvalue}, \code{ci.lower}, \code{ci.upper}. A \code{share} column
#'   (each estimate divided by the total) is added when the spec extracts a row
#'   of type \code{"total"}.}
#' \item{coefficients}{Data frame of all structural coefficients
#'   (\code{~} and off-diagonal \code{~~}), labelled with original names, in
#'   the requested metric.}
#' \item{total}{The \code{"total"} estimate if the spec defines one, otherwise
#'   \code{NA}.}
#' \item{n}{Number of observations used.}
#' \item{converged}{Logical.}
#' \item{admissible}{Logical: \code{FALSE} if the solution has negative
#'   variances or a standardized total outside [-1, 1].}
#' \item{status}{\code{"Success"}, or a short description of the problem.}
#' \item{metric}{The metric used.}
#' \item{syntax}{The model syntax that was fitted, including any measurement
#'   lines added for latent roles.}
#' \item{varmap}{Data frame mapping role names to original columns, with each
#'   role's semantic label and reliability (\code{NA} when observed).}
#' \item{fit}{The lavaan fit object, if \code{return_fit = TRUE}.}
#'
#' @seealso \code{\link{fancyModel}}, \code{\link{modelOnAllY}},
#'   \code{\link{stabilityPaths}}
#'
#' @examples
#' spec <- stabilityModel(X = "leader", controls = "ses")
#' d <- stabilityData(stabilitySim$T1, stabilitySim$T2, stabilitySim$experience,
#'                    keep = "T1", fill = list(leader = 0))
#' bind <- c(Y1 = "dominant[T1]", Y2 = "dominant[T2]")
#'
#' # observed variables
#' fitModel(spec, d, bind, metric = "std")$paths
#'
#' # the same model with Y1 and Y2 corrected for unreliability
#' fitModel(spec, d, bind, reliability = c(Y1 = .7, Y2 = .7), metric = "std")$paths
#'
#' @export
#' @importFrom lavaan sem parameterestimates standardizedSolution nobs lavInspect
#' @importFrom stats setNames var
fitModel <- function(spec, data, bind, reliability = NULL,
                     metric = c("raw", "std"), return_fit = FALSE) {

  if (!inherits(spec, "fancyModel"))
    stop("`spec` must be a fancyModel object (see ?fancyModel).")
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  metric <- match.arg(metric)

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

  rel <- checkReliability(reliability, roles)

  # Semantic role labels (e.g. "mediator", "control") when the spec supplies
  # them; downstream consumers such as the plot method dispatch on these.
  role_lbl <- c(rep("slide", length(bind)), rep("fixed", length(spec$vars)))
  if (length(spec$roles)) {
    hit <- match(roles, names(spec$roles))
    role_lbl[!is.na(hit)] <- unname(spec$roles)[hit[!is.na(hit)]]
  }

  varmap <- data.frame(
    internal    = roles,
    original    = columns,
    role        = role_lbl,
    reliability = unname(rel[roles]),
    stringsAsFactors = FALSE
  )

  d <- data[, columns, drop = FALSE]
  names(d) <- roles
  d[] <- lapply(d, as.numeric)

  ## ---- measurement model for latent roles ---------------------------------
  # A latent role keeps its name in the structural syntax; its column is
  # renamed <role>_obs and becomes the single indicator with fixed error.
  latent <- roles[!is.na(rel[roles])]
  meas <- character(0)
  for (r in latent) {
    obs <- paste0(r, "_obs")
    v   <- stats::var(d[[r]], na.rm = TRUE)
    if (!is.finite(v) || v <= 0)
      stop("Cannot model role `", r, "` as latent: its column has no variance.")
    names(d)[names(d) == r] <- obs
    meas <- c(meas,
              sprintf("%s =~ 1*%s", r, obs),
              sprintf("%s ~~ %s*%s", obs, format((1 - rel[[r]]) * v, digits = 15), obs))
  }
  syntax <- if (length(meas))
    paste(c("# measurement: single indicators with known reliability", meas,
            "# structural", spec$syntax), collapse = "\n")
  else spec$syntax

  ## ---- path scaffold, used for both success and failure -------------------
  path_rows <- spec$extract
  has_total <- "type" %in% names(path_rows) && any(path_rows$type == "total")

  fail <- function(msg) {
    for (cl in c("est", "se", "pvalue", "ci.lower", "ci.upper"))
      path_rows[[cl]] <- NA_real_
    if (has_total) path_rows$share <- NA_real_
    path_rows$label <- NULL
    rownames(path_rows) <- NULL
    list(paths = path_rows, coefficients = NULL, total = NA_real_,
         n = NA_integer_, converged = FALSE, admissible = NA, status = msg,
         metric = metric, syntax = syntax, varmap = varmap)
  }

  # lavaan warns (rather than errors) about inadmissible solutions; collect
  # those warnings into the status instead of letting them escape per item.
  warns <- character(0)
  fit <- tryCatch(
    withCallingHandlers(
      do.call(lavaan::sem, c(list(model = syntax, data = d), spec$sem_args)),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e)
      structure(list(msg = conditionMessage(e)), class = "fancyFitFail"))
  if (inherits(fit, "fancyFitFail"))
    return(fail(paste("Model error:", fit$msg)))
  if (!lavaan::lavInspect(fit, "converged"))
    return(fail("Skipped: model did not converge"))

  pe <- if (metric == "std") {
    s <- lavaan::standardizedSolution(fit, type = "std.all")
    names(s)[names(s) == "est.std"] <- "est"
    s
  } else lavaan::parameterestimates(fit)

  grab <- function(lbl, col) {
    v <- pe[[col]][!is.na(pe$label) & pe$label == lbl]
    if (!length(v)) NA_real_ else v[1]
  }

  for (cl in c("est", "se", "pvalue", "ci.lower", "ci.upper"))
    path_rows[[cl]] <- vapply(path_rows$label, grab, numeric(1), col = cl)

  total <- if (has_total) path_rows$est[path_rows$type == "total"][1] else NA_real_
  if (has_total) path_rows$share <- path_rows$est / total
  path_rows$label <- NULL
  rownames(path_rows) <- NULL

  ## ---- admissibility -------------------------------------------------------
  problems <- character(0)
  if (!isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check"))))
    problems <- c(problems, "negative variance estimate")
  if (metric == "std" && has_total && isTRUE(abs(total) > 1))
    problems <- c(problems, "standardized total stability exceeds 1")
  if (length(latent) && !length(problems) &&
      any(grepl("not positive definite|negative", warns)))
    problems <- c(problems, "non-positive-definite latent covariance matrix")
  admissible <- !length(problems)
  status <- if (admissible) "Success" else
    paste0("Inadmissible: ", paste(problems, collapse = "; "),
           if (length(latent)) " (is the reliability lower than the data imply?)")

  ## ---- structural coefficients, relabelled to original names --------------
  lookup <- stats::setNames(varmap$original, varmap$internal)
  keep <- c("lhs", "rhs", "est", "se", "pvalue", "ci.lower", "ci.upper")
  is_struct <- pe$lhs %in% roles & pe$rhs %in% roles
  reg <- pe[pe$op == "~" & is_struct, keep]
  cvs <- pe[pe$op == "~~" & pe$lhs != pe$rhs & is_struct, keep]
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
    paths        = path_rows,
    coefficients = coefs,
    total        = total,
    n            = lavaan::nobs(fit),
    converged    = TRUE,
    admissible   = admissible,
    status       = status,
    metric       = metric,
    syntax       = syntax,
    varmap       = varmap
  )
  if (return_fit) out$fit <- fit
  out
}

# Validate a role-keyed reliability vector; return it named by role, with NA
# for every role left observed (unnamed, NA, or exactly 1).
checkReliability <- function(reliability, roles) {
  out <- stats::setNames(rep(NA_real_, length(roles)), roles)
  if (is.null(reliability) || !length(reliability)) return(out)
  if (!is.numeric(reliability) || is.null(names(reliability)) ||
      any(!nzchar(names(reliability))))
    stop("`reliability` must be a named numeric vector keyed by role, ",
         "e.g. c(Y1 = .7, Y2 = .7).")
  unknown <- setdiff(names(reliability), roles)
  if (length(unknown))
    stop("`reliability` names role(s) not in the model: ",
         paste(unknown, collapse = ", "))
  bad <- !is.na(reliability) & (reliability <= 0 | reliability > 1)
  if (any(bad))
    stop("Reliabilities must be in (0, 1]; got ",
         paste(sprintf("%s = %s", names(reliability)[bad], reliability[bad]),
               collapse = ", "))
  keep <- !is.na(reliability) & reliability < 1
  out[names(reliability)[keep]] <- reliability[keep]
  out
}
