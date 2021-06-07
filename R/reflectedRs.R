#' Correlation Matrix with all Reflected (or Reversed) Correlations included
#' @description
#' This is particularly useful for cluster analysis, where you can
#' include all reverse-scored variables to force the clusters to include
#' antonymous content (e.g., 'outgoing' and 'shy' may be on the same cluster)
#' @param  corrs Correlation matrix to reflect (or to include all reversals)
#' @return Original correlation matrix 'doubled' to include all reverse-score correlations
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

