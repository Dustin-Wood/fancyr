#' Split Item-Order Information into Separate Variables
#' @description
#' Some programs, like Qualtrics, will provide item-order information in a relatively compact
#' single string like "##|##|##|..." to indicate order items were presented to participants
#'
#' This (pretty simple) code will allow you to more easily separate this item-order information
#' into separate variables that can be used for quantitative analyses
#' @param itemorder Order of which item presented the participant first, second, etc
#' @param delim Delimiter to use to split order string (defaults to
#' Qualtrics' use of '|' to separate variables)
#' @param numeric Convert to numeric ratings?  (Defaults to \code{T},
#' but may be \code{F} if itemorder presents variables names rather than numbers)
#' @usage
#' qualOrder(BFIorder) #e.g., standard 1-5 Likert scale
#' @details
#' Function computes Cohen-adjusted correlations (or avg. product of standardized deviations-from-scale-center)
#' @return Matrix of which item was presented first, second, etc)
#'
#' @export

itemOrder <- function(itemorder, delim = "\\|", numeric = T) {
  vorder<-stringr::str_split(itemorder, pattern = delim, simplify = T)
  if(numeric == T) {
    vorder2<- matrix(as.numeric(vorder), nrow = nrow(vorder), ncol = ncol(vorder))
  } else {
    vorder2<- matrix(vorder, nrow = nrow(vorder), ncol = ncol(vorder))
  }
  return(vorder2)
}
