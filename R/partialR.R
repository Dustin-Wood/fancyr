#' Partial Correlations of Predictors with One or More Outcomes
#' @description Computes zero-order or partial correlations (controlling for a set of covariates)
#' between a set of predictor variables and one or more outcome variables.
#' @param data A data frame containing all variables.
#' @param predictors Character vector of predictor variable names.
#' @param dv Character vector of one or more outcome (dependent) variable names.
#' @param controls Character vector of control variable names whose variance is removed
#' from all other variables before correlating, or \code{NULL} (default) for zero-order
#' correlations.
#' @param adjust Method for p-value correction, passed to \code{\link[psych]{corr.test}}
#' (zero-order case) or \code{\link[psych]{corr.p}} (partial case). Default is
#' \code{"none"}. Common options: \code{"holm"}, \code{"fdr"}, \code{"bonferroni"}.
#' See \code{\link[stats]{p.adjust}} for all options.
#' @param include_controls Logical. If \code{TRUE}, control variables are included as rows
#' in the summary tables alongside the predictors. Default is \code{FALSE}.
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{summary}}{A named list of data frames, one per outcome in \code{dv}.
#'   Each data frame has columns \code{var}, \code{r}, \code{p}, and \code{n} for
#'   each predictor (and optionally each control variable).}
#'   \item{\code{cors}}{The full correlation object returned by \code{\link[psych]{corr.test}}
#'   (zero-order case) or \code{\link[psych]{corr.p}} (partial case), containing the complete
#'   r, p, t, and n matrices across all variables.}
#' }
#' @seealso \code{\link{setDepRDiffs}} for testing whether two outcomes differ in their
#' associations with a predictor set.
#' @importFrom psych corr.test partial.r corr.p
#' @export
#' @examples
#' \dontrun{
#' # Zero-order correlations of two predictors with one outcome
#' out <- partialR(data = mydata, predictors = c("x1", "x2"), dv = "y")
#' out$summary$y
#'
#' # Partial correlations controlling for age and sex
#' out <- partialR(data = mydata, predictors = c("x1", "x2"), dv = "y",
#'                 controls = c("age", "sex"))
#' out$summary$y
#' out$cors$r  # full partial r matrix
#'
#' # Multiple outcomes, Holm-adjusted p-values
#' out <- partialR(data = mydata, predictors = c("x1", "x2", "x3"),
#'                 dv = c("y1", "y2"), controls = c("age"), adjust = "holm")
#' out$summary$y1
#' out$summary$y2
#' }

partialR <- function(data, predictors, dv, controls = NULL, adjust = "none",
                     include_controls = FALSE) {
  all_vars <- c(predictors, dv, controls)
  test_data <- data[, all_vars, drop = FALSE]

  if (is.null(controls)) {
    cors <- psych::corr.test(test_data, adjust = adjust, ci = FALSE)
  } else {
    rs   <- psych::corr.test(test_data, ci = FALSE)
    pars <- psych::partial.r(data = test_data, x = 1:ncol(test_data), y = controls)
    cors <- psych::corr.p(pars, n = rs$n - length(controls), adjust = adjust, ci = FALSE)
  }

  row_vars <- if (include_controls && !is.null(controls)) {
    c(predictors, controls)
  } else {
    predictors
  }

  summary <- lapply(dv, function(d) {
    n_val <- if (is.null(dim(cors$n))) {
      rep(cors$n, length(row_vars))
    } else {
      cors$n[row_vars, d]
    }
    data.frame(
      var = row_vars,
      r   = cors$r[row_vars, d],
      p   = cors$p[row_vars, d],
      n   = n_val,
      row.names = NULL,
      check.names = FALSE
    )
  })
  names(summary) <- dv

  list(summary = summary, cors = cors)
}
