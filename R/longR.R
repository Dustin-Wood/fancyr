#' Stretch (usually a Correlation) Matrix into an Edgelist Format
#' @description
#' Sometimes you just want all your correlations in one column.
#' @param corMat Matrix you want to convert (doesn't need to be a correlation matrix)
#' @usage
#' longR(corMat)
#' @return a dataframe with \code{c("varX","varY","rXY")} layout
#'
#' @export

longR <- function(corMat) {
  temp <- data.frame(rownames(corMat),as.data.frame(corMat))
  rsLong <- tidyr::pivot_longer(temp, cols = 2:ncol(temp), names_to = "var", values_to = "r", values_drop_na = TRUE)
  colnames(rsLong) <- c("varX","varY","rXY")
  return(rsLong)
}

