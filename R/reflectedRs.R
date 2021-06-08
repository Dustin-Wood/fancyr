#' Correlation Matrix - Doubled w/ Reflections
#' @description
#' Artificially doubles the length of rows & columns in
#' correlation matrix by including all reverse-scored variable.
#' (Mainly used for cluster analysis)
#'
#' @param corrs the correlation matrix including all correlations between variables in the item set
#'
#' @export

reflectedRs <- function(corrs) {
  doubledCorrs1<-cbind(corrs,-corrs)
  doubledCorrs2<-cbind(-corrs,corrs)
  doubledCorrs<-rbind(doubledCorrs1,doubledCorrs2)
  #try to make the labels more clearly indicate reversals
  #(may need to touch this up...)
  colnames(doubledCorrs) <- c(colnames(corrs),paste0(colnames(corrs),".R"))
  rownames(doubledCorrs) <- c(colnames(corrs),paste0(colnames(corrs),".R"))
  return(doubledCorrs)
}
