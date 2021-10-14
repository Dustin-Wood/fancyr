#' Remove Respondents Below Minimum Response Variability
#' @description
#' Exclude cases that have less than a specified minimum level of variability
#' @param data Matrix to be transformed
#' @param smax Scale maximum
#' @param smin Scale minimum
#' @param minvar Minimum variance allowed
#' @param includeScreen Include the screening variable created (default to \code{F})
#' @details This is a data screen designed to remove participants that did
#' not vary their ratings - which at extreme levels is generally indicative
#' of insufficient effort responding.
#'
#' @return Dataset with cases failing to pass minimum variance screen removed
#' @export

rmvLowVar <- function(data, smax, smin, minvar,includeScreen=F){
  data$perVARx <- apply(data, 1, function(x)  (sd(as.matrix(x[2:length(x)], na.rm=F))*(sqrt((length(x)-1)/length(x)))/(smax-smin)/(.5))^2)
  data <- subset(data, perVARx > minvar)
if(includeScreen == F){data <- data[-length(data)]}
  return(data)
}
