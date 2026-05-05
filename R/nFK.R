#' Number of Orthogonal Factors Measured by Set
#' @description
#' Estimate the effective number of orthogonal factors measured by a set of
#' items, weighted by how reliably each factor is recovered. Conceptually,
#' \deqn{\mathrm{nFK} = \sum_k \omega_{\max,k},}
#' where \eqn{\omega_{\max,k}} (Bentler, 1968; Lord, 1958) is the maximum
#' reliability achievable by the optimal linear composite of items measuring
#' the \eqn{k}-th principal component. When items measuring a factor are
#' tau-equivalent (uniform loadings, uniform within-cluster correlation),
#' \eqn{\omega_{\max}} collapses to Cronbach's \eqn{\alpha}, and for symmetric
#' scales further to Spearman-Brown.
#'
#' Use \code{nFK} as a reliability-aware dimensionality summary — an
#' alternative to procedures that ignore measurement error (Kaiser rule,
#' parallel analysis on the unaltered correlation matrix).
#' @param rMat Correlation matrix with reliability or communality estimates on
#'   the diagonal (range 0 to 1). Off-diagonals are observed inter-item
#'   correlations. Reliability on the diagonal — ideally retest correlations
#'   over the same measurement interval as the inter-item correlations
#'   (Wood, Lowman, Armstrong, & Harms, 2022) — is what makes the statistic
#'   reliability-aware rather than just counting components. Setting the
#'   diagonal to 1.0 returns \code{nFK = p} regardless of off-diagonals (every
#'   item is its own perfectly-measured composite), which is correct under the
#'   \eqn{\omega_{\max}} reading but typically not the answer the user wants.
#'   Variable names must be unique and contain no spaces.
#' @param rotate Orthogonal rotation applied to the principal-component
#'   loadings before the joint matrix is built. One of \code{"none"} (default)
#'   or \code{"varimax"}. Total \code{nFK} is invariant under orthogonal
#'   rotation, but per-component reliabilities in \code{r2} redistribute and
#'   may be of independent interest. Use \code{rotate = "varimax"} when the
#'   goal is the reliability of each rotated factor (e.g., Big-Five solutions)
#'   rather than total dimensionality.
#' @details
#' \strong{Special cases.}
#' \itemize{
#'   \item \emph{\eqn{p} orthogonal items, each at reliability \eqn{\rho}}:
#'     \eqn{\mathrm{nFK} = p \rho}.
#'   \item \emph{One factor measured by \eqn{k} tau-equivalent items at
#'     within-cluster correlation \eqn{r}, diagonal \eqn{= r}}:
#'     \eqn{\mathrm{nFK} = \mathrm{SB}(k, r) = k r / (1 + (k-1) r)}.
#'   \item \emph{Rank-deficient matrices}: components with zero loadings
#'     contribute zero, so the matrix rank is the upper bound on \code{nFK}.
#' }
#'
#' \strong{Method.} Eigendecompose \code{rMat} to get loadings \eqn{\mathbf{L}
#' = \mathbf{V} \mathbf{D}^{1/2}}; optionally rotate. Form the \eqn{2p \times
#' 2p} joint matrix
#' \deqn{\mathbf{B} = \begin{bmatrix} \mathbf{R} & \mathbf{L} \\
#' \mathbf{L}' & \mathbf{0} \end{bmatrix},}
#' fix \eqn{\mathrm{diag}(\mathbf{B}) = 1}, and smooth to the nearest
#' correlation matrix via \code{Matrix::nearPD()} (Higham, 2002). The joint
#' matrix is rank-deficient by construction (components are exact linear
#' combinations of items), and this smoothing is what drives \eqn{R^2_k} below
#' 1 in collinear cases. From the smoothed item block \eqn{\mathbf{R}_{xx}}
#' and item-component block \eqn{\mathbf{L}^*}, compute the OLS \eqn{R^2_k =
#' \mathbf{L}^{*\prime}_{\cdot k} \mathbf{R}_{xx}^{-1} \mathbf{L}^*_{\cdot k}};
#' each \eqn{R^2_k} equals \eqn{\omega_{\max,k}} for the \eqn{k}-th component,
#' and \code{nFK} is their sum.
#' @return A named list with:
#' \describe{
#'   \item{\code{PCAloadings}}{Unsmoothed PCA loadings (items x components),
#'     after rotation if requested.}
#'   \item{\code{r2}}{Named numeric vector of \eqn{\omega_{\max,k}} — the
#'     maximum reliability of the optimal composite for each component.}
#'   \item{\code{nFK}}{Scalar — total effective number of orthogonal factors.}
#' }
#' @references
#' Bentler, P. M. (1968). Alpha-maximized factor analysis (alphamax): Its
#' relation to alpha and canonical factor analysis. \emph{Psychometrika},
#' \emph{33}(3), 335-345.
#'
#' Higham, N. J. (2002). Computing the nearest correlation matrix - a problem
#' from finance. \emph{IMA Journal of Numerical Analysis}, \emph{22}(3),
#' 329-343.
#'
#' Lord, F. M. (1958). Some relations between Guttman's principal components
#' of scale analysis and other psychometric theory. \emph{Psychometrika},
#' \emph{23}(4), 291-296.
#' @importFrom Matrix nearPD
#' @export

nFK <- function(rMat, rotate = "none") {
  rotate <- match.arg(rotate, c("none", "varimax"))
  p <- ncol(rMat)

  eig <- eigen(rMat, symmetric = TRUE)
  L <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = p)
  if (rotate == "varimax") {
    L <- unclass(stats::varimax(L, normalize = TRUE)$loadings)
  }
  rownames(L) <- rownames(rMat)
  colnames(L) <- paste0(if (rotate == "none") "PC" else "RC", seq_len(p))

  # Joint item+component system: top-left = rMat, top-right = L,
  # bottom-left = L', bottom-right = 0. Setting the full diagonal to 1
  # fixes both item and (standardized) component variances at 1.
  bigmat <- rbind(cbind(rMat, L),
                  cbind(t(L), matrix(0, p, p)))
  diag(bigmat) <- 1

  # Smooth to PSD. The joint matrix is rank-deficient by construction
  # (components are exact linear combinations of items), so this step
  # introduces the perturbation that drives R^2_k below 1 in collinear cases.
  bigmat <- as.matrix(Matrix::nearPD(bigmat, corr = TRUE, keepDiag = TRUE)$mat)

  R_xx        <- bigmat[1:p, 1:p, drop = FALSE]
  L_smoothed  <- bigmat[1:p, (p + 1):(2 * p), drop = FALSE]

  R2 <- colSums(L_smoothed * solve(R_xx, L_smoothed))
  names(R2) <- colnames(L)

  list(PCAloadings = L,
       r2          = R2,
       nFK         = sum(R2))
}
