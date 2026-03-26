#' Items in Common Between Two Sets
#' @description
#' Identifies which items from \code{items1} are also present in \code{items2}.
#' Useful for finding the common items across two survey administrations before
#' passing them to \code{\link{xEffects}} or \code{\link{medXonAllY}}.
#'
#' @param items1 Character vector of item names from the first set (e.g., T1).
#' @param items2 Character vector of item names from the second set (e.g., T2).
#'
#' @return A data frame with columns:
#' \item{item}{Item name from \code{items1}.}
#' \item{inBoth}{Logical; \code{TRUE} if the item is also in \code{items2}.}
#'
#' @export
inCommon <- function(items1, items2) {
  data.frame(
    item   = items1,
    inBoth = items1 %in% items2,
    stringsAsFactors = FALSE
  )
}
