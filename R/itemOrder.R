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
#' @param numeric Convert to numeric ratings?  (Defaults to \code{TRUE},
#' but may be \code{FALSE} if itemorder presents variable names rather than numbers)
#' @details
#' Function indicates which items the person saw when.
#' This can be used to estimate item distance and such matters
#' @return Matrix of which item was presented first, second, etc
#'
#' @export


itemOrder <- function(itemorder, delim = "\\|", numeric = TRUE) {
  if(numeric == TRUE) {
    vorder2<-as.matrix(stringr::str_split(itemorder, pattern = delim, simplify = TRUE))
  } else {
    #this needs to get fixed
    vorder2<-t(apply(itemorder,1,function(x) stringr::str_split(x, pattern = delim, simplify = TRUE)))
  }
  return(vorder2)
}
