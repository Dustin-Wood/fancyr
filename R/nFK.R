#' Number of Orthogonal Factors Measured by Set
#' @description
#' For a specified set of variables, estimate the 'effective number of orthogonal factors' measured.
#'
#' Note that 'measured' is intended to mean that a factor is only measured if it has an
#' R^2 value of 1.00.  So statistic provides the sum of R2 for orthogonal dimensions, via PCA.
#' @param rMat Correlation matrix (with appropriate reliabilities on diagonal)
#'  The correlation matrix should have a unique name for each variable, and these
#'  variable names should be a single word or string (no spaces).  Can check with \code{colnames(rMat)}
#' @details The correlation matrix should have correct reliability values on the diagonals (ideally,
#' the value will be estimates of the retest values over the same measurement interval as typical for
#' inter-item correlations within the matrix; see Wood, Lowman, Armstrong, & Harms, 2022).
#' If values of 1.0 are used on the diagonal rather than correct reliability estimates,
#' then the resulting nFK estimate will be inflated - often substantially. However, it may
#' still be useful to estimate nFK in this case to provide an 'upper-bound' estimate
#' of nFK.
#'
#' There is also some code that is a work in progress to try to do this through 'setCor', although
#' it is commented out as it has problems at the moment.
#'
#' @return returns:
#' (1) the PCA loadings,
#' (2) the R^2 for each separate factor, and
#' (3) the nFK for the entire item set (sum of R^2)
#' @export


nFK <- function(rMat, rotate = "none") {

#Step 1: conduct principal components analysis (PCA)
rfactors<-psych::principal(rMat,covar=T, nfactors = ncol(rMat), rotate = rotate)
  floadings<-unclass(rfactors$loadings) #extract factor loadings

#Step 2: bind factor loadings to original correlation matrix
bigmat<-cbind(rMat,floadings)
bigmat<-rbind(bigmat,cbind(t(floadings),matrix(0,ncol(floadings),nrow(floadings))))
dimnames(bigmat)<-list(colnames(bigmat),colnames(bigmat))

#Step 3: Predict the nK factors as well as possible, from the loadings
#3.1 - whatever the diagonals were before, replace them with 1
    #I think this means that we are returning fully
    #to the realm of working with observed scores
diag(bigmat)<-1

#3.2 - smooth into a positive-definite matrix using Higham's (2002) adjustment
  #(*I found this needed to be done in order to have lavaan run on example datasets involving perfect correlations)
bigmat<-data.matrix(Matrix::nearPD(bigmat, corr=T, keepDiag = TRUE)$mat)

#3.3 - Predict the factor as well as possible from set K
  #Creation of model for lavaan to predict all factors from all variables in K as predictors
  x<-paste0(colnames(rMat)[-ncol(rMat)]," + ", collapse = "")
  model<-paste0(colnames(floadings)," ~ ",x,colnames(rMat)[ncol(rMat)]," \n")

#predict each principal component from all other items in K
fit<-lavaan::sem(model,sample.cov = bigmat, sample.nobs = 1000)
    #coef(fit) # unstandardised coefficients
    #standardizedSolution(fit) # Standardised coefficients
    #inspect(fit, 'r2') # r-squared



#this will probably *ultimately* be a MUCH more efficient way to do what lavaan is doing.
    #setrs<-psych::setCor(c((ncol(rMat)+1):(ncol(rMat)*2)),c(1:ncol(rMat)),bigmat,z=NULL,n.obs=NULL,use="pairwise",std=TRUE,square=FALSE,
    #                     main="Regression Models",plot=TRUE,show=FALSE,zero=TRUE, alpha = .05,part=FALSE)
    #setrs$coefficients
    #nFK <- sum(setrs$R2)
  #...However, at the moment, it produces VERY problematic results with real data!
    #Specifically, for most cases, it insists that...
          #nFK = nK
        #...and before this, that...
          #R2 = 1 for every factor
        #and I can't figure out why!!
#there may be alternative ways to do regression analysis from solve() function
  #perhaps this can be usefully adapted...
  #https://stats.stackexchange.com/questions/107597/is-there-a-way-to-use-the-covariance-matrix-to-find-coefficients-for-multiple-re

results<-list(floadings,lavaan::inspect(fit, 'r2'),sum(lavaan::inspect(fit, 'r2')))
names(results)<-c("PCAloadings","r2","nFK")

return(results)
}
