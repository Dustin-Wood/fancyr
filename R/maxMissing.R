#' Exclude Cases with N Missing Values
#' @description
#' Exclude cases that pass some specified threshold for allowable missing cases
#' @param data Matrix to be transformed
#' @param maxNA Maximum number of missing values for row
#' @param nonX Number of leading non-score variables in datafile
#' @return Reduced dataset excluding people beyond threshold missing values
#' @details The dataset must be set up to have a format of identifier variables
#' FOLLOWED BY (and ENDING WITH) scores in response to the inventory to be executed
#' correctly
#'
#' @export

maxMissing <- function(data,maxNA=0, nonX) {
  data$nna <- apply(data, 1, function(x)  sum(is.na(x[(1+nonX):length(x)]))) #'nna' = number missing values
  data <- subset(data, nna <= maxNA)
  data <- data[-length(data)]
  return(data)
}
