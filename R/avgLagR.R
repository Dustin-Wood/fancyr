#' Average Lag Correlation over Over Measurement Interval, r_|d|
#' @description
#' Estimate the average correlation between the variables within the set over a two-wave
#' interval (d; generally a period of time, or number of items apart).  Diagonals will be retest
#' correlations over that interval.
#'
#' Note: Can be converted to a sort of 'reliability-adjusted matrix' by \code{cov2cor(rd)},
#' which will index the degree to which correlations between variables over
#' measurement interval (d; generally a period of time, or number of items apart)
#' are smaller than the retest correlations of the variables
#' over that interval (see Wood, Lowman, Armstrong, Harms, 2022)
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
#' avgLagR(x1, x2)
#' phat.t(x1, x2, posdef = "Higham")
#' @param x1 first matrix to be used to estimate correlations
#' @param x2 second matrix to be used (all variables should be the same as x1, and in the same order!!)
#' @param posdef adjust correlations to make the matrix positive definite? ("Bock", "Higham", and default = FALSE)
#'
#' @export

avgLagR <- function(x1, x2, posdef = F) {
  library(psych) #this function utilizes two functions from 'psych' package

  r_x1x2 <- corr.test(x1, x2, use="complete.obs", method="pearson", ci=F)
  r_xy_d <- (r_x1x2$r+t(r_x1x2$r))/2

  #variable names will be taken from x1
  dimnames(r_xy_d) <- list(colnames(x1),colnames(x1))

  #the resulting matrix is unlikely to be positive-definite, so if you want
  #to do other analyses with it, you may need to 'smooth' the correlations
  #into a positive definite matrix
  if (posdef == "Higham") {
    #smooth into a positive-definite matrix using Higham's (2002) adjustment
    library(Matrix)
    posDMat<-nearPD(r_xy_d, corr=FALSE, keepDiag = TRUE)
    r_xy_d<-data.matrix(posDMat$mat)
  }

  if(posdef == "Bock"){
    #smooth into positive-definite matrix using Bock, Gibbons and Muraki (1988) and Wothke (1993) adjustment
    #note: this particular adjustment doesn't seem to play well with the unadjusted r_xy_d matrix (i.e., where diagonals do not equal 1)
    #it seems it is not designed to handle covariance rather than correlation matrices.
    r_xy_d<-cor.smooth(r_xy_d)
  }

  return(r_xy_d)
}
