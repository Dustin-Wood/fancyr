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
#'   in \code{$fit}. Defaults to \code{FALSE}. With \code{rescale = TRUE}, that
#'   object was fitted to the SD-scaled columns.
#' @param binary How to express, in the standardized metric, the
#'   coefficients of a \emph{binary} variable: any fixed role (such as an
#'   experience or control, not a sliding item) with exactly two distinct
#'   values, analysed as observed. \code{"sd"} (default) standardizes them like
#'   every other variable, so all coefficients share one metric and their
#'   sizes can be compared. \code{"unit"} instead reports them per difference
#'   between the two values, i.e. per 1 vs 0 for 0/1 or
#'   \code{FALSE}/\code{TRUE} coding: a binary predictor's coefficient is the
#'   expected difference in the outcome, in the outcome's SDs, between the two
#'   groups; a binary outcome's coefficient is the change in the proportion
#'   coded 1 per SD of the predictor. These read naturally but aren't on the
#'   same scale as the other coefficients. Covariances stay correlations, and pathways,
#'   totals and shares are the same either way. The raw metric is unaffected.
#'   \code{$varmap$binary} records which variables were treated as binary.
#' @param rescale Logical. Fit the model to columns divided by their standard
#'   deviations, then convert raw-metric estimates back to the columns' own
#'   units? Default \code{TRUE}. The results are the same either way, but
#'   variables on very different scales (e.g. a 0/1 dummy beside a score in
#'   the tens of thousands) can otherwise make the information matrix
#'   numerically singular, so that no standard errors can be computed. Set
#'   \code{FALSE} for a hand-written spec that fixes parameters to nonzero
#'   values in the columns' own units.
#'
#' @return A named list with components:
#' \item{paths}{Data frame of the extracted parameters, carrying any annotation
#'   columns declared in \code{spec$extract}, plus \code{est}, \code{se},
#'   \code{pvalue}, \code{ci.lower}, \code{ci.upper}. A \code{share} column
#'   (each estimate divided by the total) is added when the spec extracts a row
#'   of type \code{"total"}. If \code{spec$extract} has an \code{outcome}
#'   column, as when a spec decomposes more than one total, each row's share
#'   is taken of the total with the same \code{outcome}.}
#' \item{coefficients}{Data frame of all structural coefficients
#'   (\code{~} and off-diagonal \code{~~}), labelled with original names, in
#'   the requested metric.}
#' \item{total}{The \code{"total"} estimate if the spec defines one (the first,
#'   if it defines several), otherwise \code{NA}.}
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
#' spec <- stabilityModel(X = "power[T1]", controls = "tenure")
#' d <- stabilityData(powerTraits$T1, powerTraits$T2, powerTraits$people,
#'                    commonItems = c("power", "Powerful_role"))
#' bind <- c(Y1 = "Powerful_role[T1]", Y2 = "Powerful_role[T2]")
#'
#' # observed variables
#' fitModel(spec, d, bind, metric = "std")$paths
#'
#' # the same model with Y1 and Y2 adjusted for retest reliability
#' fitModel(spec, d, bind, reliability = c(Y1 = .75, Y2 = .75), metric = "std")$paths
#'
#' @export
#' @importFrom lavaan sem parameterestimates standardizedSolution nobs lavInspect
#' @importFrom stats setNames var
fitModel <- function(spec, data, bind, reliability = NULL,
                     metric = c("raw", "std"), return_fit = FALSE,
                     rescale = TRUE, binary = c("sd", "unit")) {

  if (!inherits(spec, "fancyModel"))
    stop("`spec` must be a fancyModel object (see ?fancyModel).")
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  metric <- match.arg(metric)
  binary <- match.arg(binary)

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

  # Binary variables: fixed roles (not the sliding items) with exactly two
  # distinct values, analysed as observed. In the standardized metric their
  # coefficients are reported per difference between the two values (per
  # 1 vs 0) rather than per SD; see below. `binD` is that difference.
  binD <- vapply(roles, function(r) {
    u <- unique(d[[r]][!is.na(d[[r]])])
    if (!(r %in% spec$slide) && length(u) == 2L && is.na(rel[[r]])) abs(diff(u))
    else NA_real_
  }, numeric(1))
  varmap$binary <- !is.na(binD[roles])

  # Fit on SD-scaled columns, so that variables on very different scales
  # (e.g. a 0/1 dummy beside a score in the tens of thousands) don't leave
  # the information matrix numerically singular. Scaling only (no centering)
  # keeps every raw-metric estimate a simple multiple of its scaled value;
  # see unscaleEstimates().
  sc <- stats::setNames(rep(1, length(roles)), roles)
  if (rescale) for (r in roles) {
    s <- stats::sd(d[[r]], na.rm = TRUE)
    if (is.finite(s) && s > 0) { sc[[r]] <- s; d[[r]] <- d[[r]] / s }
  }

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

  # Standardized estimates don't depend on the scaling; raw ones are
  # converted back to the columns' own units. A latent role and its
  # indicator share their column's scale.
  # lavaan repeats its "could not compute standard errors" warning when the
  # results are extracted; it is reported in $status (below), so collect it
  # here rather than printing it once per item
  quietSE <- function(expr) withCallingHandlers(expr, warning = function(w) {
    if (grepl("could not compute standard errors", conditionMessage(w), ignore.case = TRUE)) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  })
  pe <- quietSE(if (metric == "std") {
    s <- lavaan::standardizedSolution(fit, type = "std.all")
    names(s)[names(s) == "est.std"] <- "est"
    if (binary == "unit" && any(!is.na(binD))) {
      # std.all puts every variable in SD units. For a binary variable B,
      # re-express its regression coefficients per difference between its two
      # values: multiply by D/SD(B) where B predicts, and by SD(B)/D where B
      # is predicted (its implied SD, in the columns' own units). Covariances
      # stay correlations, and defined parameters are untouched: in every
      # pathway a variable's scale cancels.
      imp <- lavaan::lavInspect(fit, "implied")$cov
      bin <- names(binD)[!is.na(binD)]
      g <- stats::setNames(sqrt(diag(imp)[bin]) * sc[bin] / binD[bin], bin)
      gl <- g[s$lhs]; gr <- g[s$rhs]
      gl[is.na(gl)] <- 1; gr[is.na(gr)] <- 1
      # applied to the reported coefficients only (below): the pathways stay
      # fully standardized, so a decomposition still adds to its total even
      # when one of its parts is a coefficient between binary variables
      s$binFactor <- ifelse(s$op == "~", unname(gl / gr), 1)
    }
    s
  } else {
    p <- lavaan::parameterestimates(fit)
    if (rescale) {
      units <- sc
      if (length(latent)) units[paste0(latent, "_obs")] <- sc[latent]
      p <- tryCatch(unscaleEstimates(p, fit, units),
                    error = function(e) structure(list(msg = conditionMessage(e)),
                                                  class = "fancyFitFail"))
      if (inherits(p, "fancyFitFail"))
        return(fail(paste0("Model error: ", p$msg, " Use rescale = FALSE.")))
    }
    p
  })

  grab <- function(lbl, col) {
    v <- pe[[col]][!is.na(pe$label) & pe$label == lbl]
    if (!length(v)) NA_real_ else v[1]
  }

  for (cl in c("est", "se", "pvalue", "ci.lower", "ci.upper"))
    path_rows[[cl]] <- vapply(path_rows$label, grab, numeric(1), col = cl)

  # A spec may decompose more than one total (e.g. a cross-lag model decomposes
  # the stability of both variables); an `outcome` column says which total
  # each row belongs to, and shares are taken within outcome.
  grp <- if ("outcome" %in% names(path_rows)) path_rows$outcome
         else rep("", nrow(path_rows))
  totals <- if (has_total) path_rows$est[path_rows$type == "total"] else NA_real_
  total  <- totals[1]
  if (has_total) {
    is_tot <- path_rows$type == "total"
    path_rows$share <- path_rows$est / path_rows$est[is_tot][match(grp, grp[is_tot])]
  }
  path_rows$label <- NULL
  rownames(path_rows) <- NULL

  ## ---- admissibility -------------------------------------------------------
  problems <- character(0)
  if (!isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check"))))
    problems <- c(problems, "negative variance estimate")
  if (metric == "std" && has_total && isTRUE(any(abs(totals) > 1)))
    problems <- c(problems, "standardized total stability exceeds 1")
  if (length(latent) && !length(problems) &&
      any(grepl("not positive definite|negative", warns)))
    problems <- c(problems, "non-positive-definite latent covariance matrix")
  admissible <- !length(problems)
  status <- if (admissible) "Success" else
    paste0("Inadmissible: ", paste(problems, collapse = "; "),
           if (length(latent)) " (is the reliability lower than the data imply?)")

  # Estimates can exist without standard errors (lavaan couldn't invert the
  # information matrix); say so rather than reporting plain success.
  has_est <- !is.na(path_rows$est)
  se_failed <- any(grepl("could not compute standard errors", warns,
                         ignore.case = TRUE)) ||
    (any(has_est) && all(is.na(path_rows$se[has_est])))
  if (se_failed)
    status <- paste0(if (admissible) "Estimated" else status,
                     "; standard errors could not be computed (information ",
                     "matrix not invertible; check X and controls for ",
                     "redundancy, no variance, or pairs never observed together)")

  ## ---- structural coefficients, relabelled to original names --------------
  if (!is.null(pe$binFactor))
    for (cl in c("est", "se", "ci.lower", "ci.upper")) pe[[cl]] <- pe[[cl]] * pe$binFactor
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

# Convert raw-metric estimates from a fit to SD-scaled columns back to the
# columns' own units. `units` gives each variable's scale factor (its SD).
# Scaling without centering makes each parameter a fixed multiple of its
# scaled value: a regression coefficient by SD(lhs)/SD(rhs), a covariance by
# SD(lhs)*SD(rhs), an intercept by SD(lhs), a loading by SD(rhs)/SD(lhs).
# A defined (:=) parameter's multiple is found by evaluating lavaan's
# definition function at two parameter vectors, one in scaled and one in raw
# units, and checked at a second pair: it must be the same, which holds for
# any definition built from products and ratios of parameters in consistent
# units (all of this package's specs). SEs and CIs scale with the estimate;
# z and p are unchanged.
unscaleEstimates <- function(pe, fit, units) {
  mult <- function(op, lhs, rhs) {
    ul <- units[lhs]; ur <- units[rhs]
    ul[is.na(ul)] <- 1; ur[is.na(ur)] <- 1
    unname(ifelse(op == "~", ul / ur, ifelse(op == "~~", ul * ur,
           ifelse(op == "~1", ul, ifelse(op == "=~", ur / ul, NA_real_)))))
  }
  f <- mult(pe$op, pe$lhs, pe$rhs)

  isdef <- pe$op == ":="
  if (any(isdef)) {
    pt <- lavaan::parTable(fit)
    fr <- pt[pt$free > 0, ]
    fr <- fr[order(fr$free), ]
    u  <- mult(fr$op, fr$lhs, fr$rhs)
    if (anyNA(u)) stop("cannot convert a parameter of this model back to raw units.")
    deff <- fit@Model@def.function
    k <- seq_len(nrow(fr))
    ratio <- function(p) deff(p * u) / deff(p)
    r1 <- ratio(.6 + (k %% 7) / 10)
    r2 <- ratio(1.1 + (k %% 5) / 7)
    if (any(!is.finite(r1)) || !isTRUE(all.equal(r1, r2, tolerance = 1e-8)))
      stop("a defined parameter can't be converted back to raw units by rescaling.")
    f[isdef] <- r1[match(pe$lhs[isdef], names(r1))]
  }
  for (cl in intersect(c("est", "se", "ci.lower", "ci.upper"), names(pe)))
    pe[[cl]] <- pe[[cl]] * f
  pe
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
