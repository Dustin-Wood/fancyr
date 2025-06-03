#' Estimated Construct Scores
#' @description Translate participant item-level scores to participant construct-level scores
#'
#' Participant scores (for the p2i matrix) should be on a [-1,1] scale
#' prior to running this function; this can be
#' done through the \code{cx} function within the \code{fancyr} package
#' @param p2i participant scores on the rated items (persons-to-items); It is suggested that items are on a [-1,1] range
#' prior to analysis to decrease the variability in cluster scores produced by missing values
#'  (this can be done using the \code{cx} function)
#' @param i2c items' coded indicativeness of the construct, on a [-1,1] metric (items-to-construct)
#' @return Participant estimated construct scores, on [-1,1] range
#'
#' @export


conScores <- function(p2i, i2c) {

  p2i <- as.matrix(p2i)
  i2c <- as.matrix(i2c)

  #let people know if they're not using cx scores
  if (max(abs(p2i),na.rm = T) > 1) {
    warning("You are not using cx scores, which makes scores less interpretable.  Run cx function from package fancyr to place scores on [-1,1] scale before proceeding")
  }

  #indicate which items the person rated (where non-rated items are NA)
  p2iObs <- (1-is.na(p2i))

  #calculate construct scores
  p2i[is.na(p2i)] <- 0 #replace NAs with 0 to allow calculation to continue
  p2c.Sum<-p2i%*%i2c #these is the sum of cross-products.  Scale can differ wildly if there are missing values

  #indicate effectiveN separately for each person and construct
  nC<-p2iObs%*%abs(i2c)

  #this should return construct scores to a [-1,1] scale
  p2c <- round(p2c.Sum*(1/nC),3)

  return(p2c)
}

