#' Number of Orthogonal Factors Measured by Set
#' @description
#' For a specified set of variables, estimate the 'effective number of orthogonal
#' factors' measured. The statistic decreases monotonically as variables become
#' more redundant: a set of \eqn{p} variables that are perfectly reliable
#' (diagonal = 1) and uncorrelated returns \code{nFK = p}; a set that is perfectly
#' reliable and perfectly intercorrelated returns \code{nFK = 1}.
#' @param rMat Correlation/covariance matrix with reliability estimates on the
#'   diagonal (ideally retest correlations over the same measurement interval as
#'   the inter-item correlations; see Wood, Lowman, Armstrong, & Harms, 2022).
#'   Diagonals can range from 0 to 1; off-diagonals are observed inter-item
#'   correlations. Variable names must be unique and contain no spaces.
#' @param rotate Rotation method passed to \code{psych::principal()}. Defaults to
#'   \code{"none"}.
#' @details
#' nFK is computed in closed form as
#' \deqn{\mathrm{nFK} = \sum_k R^2_k \cdot \frac{\lambda_k}{\lambda_{\max}},}
#' where \eqn{\lambda_k} is the \eqn{k}-th eigenvalue of \code{rMat} and
#' \eqn{R^2_k = \mathbf{L}_{\cdot k}' \mathbf{C}_{\mathrm{obs}}^{-1} \mathbf{L}_{\cdot k}}
#' is the OLS \eqn{R^2} for predicting the (standardized) \eqn{k}-th principal
#' component from the observed items. \eqn{\mathbf{L}} are the PCA loadings of
#' \code{rMat} (with \code{covar = TRUE}) and \eqn{\mathbf{C}_{\mathrm{obs}}} is
#' \code{rMat} with the diagonal set to 1.
#'
#' The weighting by \eqn{\lambda_k / \lambda_{\max}} prevents PCs that capture
#' little of \code{rMat}'s variance (e.g., near-zero noise eigenvectors when
#' items are highly correlated) from inflating the count, which is why a
#' perfectly intercorrelated set yields nFK = 1 even when items are perfectly
#' reliable. When the diagonal contains reliability estimates rather than 1.0,
#' the \eqn{R^2_k} terms also discount each factor by how well the observed
#' items recover it.
#'
#' \code{C_obs} can be rank-deficient (e.g., when off-diagonals are 1); this
#' function uses an SVD-based pseudoinverse so those cases evaluate without
#' error.
#' @return A named list with:
#' \describe{
#'   \item{\code{PCAloadings}}{Matrix of PCA factor loadings (items x components).}
#'   \item{\code{r2}}{Named numeric vector of \eqn{R^2} values, one per principal component.}
#'   \item{\code{weights}}{Eigenvalue weights \eqn{\lambda_k / \lambda_{\max}}.}
#'   \item{\code{contributions}}{Per-PC contribution to nFK (\eqn{R^2_k \cdot \lambda_k / \lambda_{\max}}).}
#'   \item{\code{nFK}}{Scalar — the effective number of orthogonal factors.}
#' }
#' @importFrom psych principal
#' @export

nFK <- function(rMat, rotate = "none") {
  p <- ncol(rMat)

  rfactors <- psych::principal(rMat, covar = TRUE, nfactors = p, rotate = rotate)
  L  <- unclass(rfactors$loadings)
  ev <- rfactors$values

  C_obs <- rMat
  diag(C_obs) <- 1
  C_inv <- pinv(C_obs)

  # R^2_k = L[,k]' %*% C_inv %*% L[,k]; equivalent to diag(t(L) %*% C_inv %*% L)
  # but skips the p^2 off-diagonal entries we don't use.
  R2 <- colSums(L * (C_inv %*% L))
  names(R2) <- colnames(L)

  lam_max <- max(ev)
  weights <- if (lam_max > 0) ev / lam_max else rep(0, p)
  contributions <- as.numeric(R2) * weights

  list(PCAloadings   = L,
       r2            = R2,
       weights       = weights,
       contributions = contributions,
       nFK           = sum(contributions))
}

# Moore-Penrose pseudoinverse via SVD (handles rank-deficient C_obs, e.g. Mat 1).
pinv <- function(m, tol = sqrt(.Machine$double.eps)) {
  s <- svd(m)
  thresh <- tol * max(s$d)
  d_inv <- ifelse(s$d > thresh, 1 / s$d, 0)
  s$v %*% (d_inv * t(s$u))
}
