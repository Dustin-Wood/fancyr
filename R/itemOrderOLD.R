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
#' itemOrder(BFIorder) #order in which person saw each BFI item, if presented randomly
#' @details
#' Function indicates which items the person saw when.
#' This can be used to estimate item distance and such matters
#' @return Matrix of which item was presented first, second, etc)
#'
#' @export

itemOrder <- function(itemorder, delim = "\\|", numeric = T) {
  if(numeric == T) {
    vorder2<-t(apply(itemorder,1,function(x) as.numeric(stringr::str_split(x, pattern = delim, simplify = T))))
  } else {
    vorder2<-t(apply(itemorder,1,function(x) stringr::str_split(x, pattern = delim, simplify = T)))
  }
  return(vorder2)
}

