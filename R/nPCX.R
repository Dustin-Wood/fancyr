#' Number of Orthogonal Dimensions Measured by Set
#' @description
#' For a specified set of variables, estimate the number of orthogonal dimensions measured.
#'
#' Note that 'measured' is intended to mean here that a dimension is only measured if it has an
#' R^2 value of 1.00.  So statistic provides the sum of R2 for orthogonal dimensions, via PCA.
#' @param rMat Correlation matrix
#' @details The correlation matrix should have correct reliability values on the diagonals (ideally,
#' the value will be estimates of the retest values over the same measurement interval as typical for
#' inter-item correlations within the matrix; see Wood, Lowman, Armstrong, & Harms, 2022).  If
#' it does not, then the resulting estimate will be inflated - often substantially!!!
#'
#' There is also some code that is a work in progress to try to do this through 'setCor', although
#' it is commented out as it has problems at the moment.
#'
#' @return (describe)
#' @export
#' @examples
#' subdata <- subset(datafile, prMaxSD > .05)


nPCX <- function(rMat, rotate = "none") {
#conduct principal components analysis (PCA)
rfactors<-psych::principal(rMat,covar=T, nfactors = ncol(rMat), rotate = rotate)
  floadings<-unclass(rfactors$loadings) #extract factor loadings

#bind factor loadings to original correlation matrix
bigmat<-cbind(rMat,floadings)
bigmat<-rbind(bigmat,cbind(t(floadings),matrix(0,ncol(floadings),nrow(floadings))))
dimnames(bigmat)<-list(colnames(bigmat),colnames(bigmat))
  #for the next part of analysis to run, we need to get all the diagonals to equal 1
  diag(bigmat)<-1

#automate the creation of lavaan code to maximally predict factors from item set
x<-paste0(colnames(rMat)[-ncol(rMat)]," + ", collapse = "")
model<-paste0(colnames(floadings)," ~ ",x,colnames(rMat)[ncol(rMat)]," \n")

#smooth into a positive-definite matrix using Higham's (2002) adjustment
  #(*I found this needed to be done in order to have lavaan run on example datasets involving perfect correlations)
bigmat<-data.matrix(Matrix::nearPD(bigmat, corr=T, keepDiag = TRUE)$mat)

#predict each principal component from all other items in the set
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

results<-list(floadings,lavaan::inspect(fit, 'r2'),sum(lavaan::inspect(fit, 'r2')))
names(results)<-c("loadingsPCA","r2","nFK")

return(results)
}
