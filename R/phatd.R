#' Reliability-adjusted Correlations Over a Measurement Interval (d)
#' @description
#' Estimate the degree to which correlations between variables over
#' measurement interval (d; generally a period of time, or number of items apart)
#' are smaller than the retest correlations of the variables
#' over that interval.  This is estimated via an understanding of Spearman's
#' adjustment("correction") for measurement unreliability.
#'
#' For instance: \code{phat_xy(d) = .80} indicates that the correlation between
#' \code{x} and \code{y} over measurement interval t is 80\% of the retest correlations of X and Y
#' (i.e., their correlations with themselves) over the same measurement interval.
#'
#' Importantly: \code{x1} and \code{x2} should be the same set of variables, given in the same
#' order, but rated at different times.  For instance, \code{x1} and \code{x2} may be the first
#' and second administrations of the items in some personality inventory a year
#' apart (or: a month, a day, etc).
#' @usage
#' phat.t(x1, x2)
#' phat.t(x1, x2, posdef = "Higham")
#' phat.t(x1, x2, adjust = F, posdef = "Higham") #don't adjust for retest reliability
#' @param x1 first matrix to be used to estimate correlations
#' @param x2 second matrix to be used (all variables should be the same as x1, and in the same order!!)
#' @param adjust adjust correlations for retest reliability? Defaults to TRUE
#' @param posdef adjust correlations to make the matrix positive definite? ("Bock", "Higham", and default = FALSE)
#'
#' @export

phatd <- function(x1, x2, adjust = T, posdef = F) {
  library(psych) #this function utilizes two functions from 'psych' package

  r_x1x2 <- corr.test(x1, x2, use="complete.obs", method="pearson", ci=F)
  r_xy_d <- (r_x1x2$r+t(r_x1x2$r))/2

  #extract the reliabilities over the measurement period here:
  r_xx_d <- diag(r_xy_d)
  names(r_xx_d) <- rownames(r_xy_d)

  if (adjust == T) {
    #adjust the r_xy_d matrix for retest correlations over that interval

    r_ExEy_d <- solve(diag(r_xx_d^.5))%*%r_xy_d%*%solve(diag(r_xx_d^.5))
    dimnames(r_ExEy_d)<-dimnames(r_xy_d)
  }

  if (adjust != T) {
    r_ExEy_d <- r_xy_d
  }

  #the resulting matrix is unlikely to be positive-definite, so if you want
  #to do other analyses with it, you may need to 'smooth' the correlations
  #into a positive definite matrix
  if (posdef == "Higham") {
    #smooth into a positive-definite matrix using Higham's (2002) adjustment
    posDMat<-Matrix::nearPD(r_ExEy_d, corr=FALSE, keepDiag = TRUE)
    r_ExEy_d<-data.matrix(posDMat$mat)
  }

  if(posdef == "Bock"){
    #smooth into positive-definite matrix using Bock, Gibbons and Muraki (1988) and Wothke (1993) adjustment
    #note: this particular adjustment doesn't seem to play well with the unadjusted r_xy_d matrix (i.e., where diagonals do not equal 1)
    #it seems it is not designed to handle covariance rather than correlation matrices.
    r_ExEy_d<-cor.smooth(r_ExEy_d)
  }

  return(r_ExEy_d)
}
