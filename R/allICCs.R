#' Estimate ICCs for a Large Set of Variables
#'
#' Runs a one-way random-effects model (\code{lmer(y ~ (1 | group))}) for each
#' variable and returns the ICC, its p-value (likelihood-ratio test via
#' \code{ranova()}), and the sample size used. Variables that cannot be modelled
#' (zero variance, too few cases, fewer than two groups, model errors) are
#' flagged with a \code{status} message rather than stopping the loop.
#'
#' @param vars Character vector of variable names to analyse.
#' @param data A data frame containing \code{vars} and \code{group}.
#' @param group Name of the grouping variable (character scalar).
#'
#' @return A \code{\link[tibble]{tibble}} with one row per variable and columns:
#'   \describe{
#'     \item{variable}{Variable name.}
#'     \item{N}{Number of complete cases used (after removing NAs in \code{y}
#'       and \code{group}).}
#'     \item{n_groups}{Number of distinct groups in those complete cases.}
#'     \item{ICC}{Intraclass correlation coefficient (between-group variance /
#'       total variance).}
#'     \item{pICC}{P-value for the ICC from a likelihood-ratio test
#'       (\code{lmerTest::ranova()}).}
#'     \item{status}{\code{"Success"} or a short description of why the
#'       variable was skipped.}
#'   }
#'
#' @export
allICCs <- function(vars, data, group) {
  purrr::map_dfr(vars, function(v) {

    # 1. Complete cases for this variable + the grouping column
    d <- data[!is.na(data[[v]]) & !is.na(data[[group]]),
              c(v, group)]
    names(d) <- c("y", "grp")

    # Strip haven_labelled so var() and lmer() receive plain numerics
    if (inherits(d$y,   "haven_labelled")) d$y   <- as.numeric(d$y)
    if (inherits(d$grp, "haven_labelled")) d$grp <- as.numeric(d$grp)

    n        <- nrow(d)
    n_groups <- length(unique(d$grp))

    # 2. FLAG: zero data points
    if (n == 0) {
      return(tibble::tibble(variable = v, N = n, n_groups = n_groups,
                            ICC = NA_real_, pICC = NA_real_,
                            status = "Skipped: 0 non-NA cases"))
    }

    # 3. FLAG: too few rows or zero variance
    if (n < 3 || var(d$y) == 0) {
      return(tibble::tibble(variable = v, N = n, n_groups = n_groups,
                            ICC = NA_real_, pICC = NA_real_,
                            status = "Skipped: Too few rows or zero variance"))
    }

    # 4. FLAG: fewer than 2 groups
    if (n_groups < 2) {
      return(tibble::tibble(variable = v, N = n, n_groups = n_groups,
                            ICC = NA_real_, pICC = NA_real_,
                            status = "Skipped: Less than 2 distinct groups"))
    }

    # 5. RUN MODEL
    tryCatch({
      m <- lme4::lmer(y ~ (1 | grp), data = d)

      vc      <- as.data.frame(lme4::VarCorr(m))
      between <- vc$vcov[vc$grp == "grp"]
      within  <- vc$vcov[vc$grp == "Residual"]
      icc     <- between / (between + within)

      ra   <- lmerTest::ranova(m)
      pval <- ra$`Pr(>Chisq)`[2]

      tibble::tibble(variable = v, N = n, n_groups = n_groups,
                     ICC = icc, pICC = pval, status = "Success")

    }, error = function(e) {
      tibble::tibble(variable = v, N = n, n_groups = n_groups,
                     ICC = NA_real_, pICC = NA_real_,
                     status = paste("Model Error:", e$message))
    })
  })
}
