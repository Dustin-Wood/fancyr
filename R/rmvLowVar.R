#' Remove Respondents Below Minimum Response Variability
#' @description
#' Exclude cases that have less than a specified minimum level of variability
#' @param data Matrix to be transformed
#' @param max Maximum response on the scale
#' @param min Minimum response on the scale
#' @return minvar Minimum variance
#' @details This is a data screen designed to remove participants that did
#' not vary their ratings - which at extreme levels is generally indicative
#' of insufficient effort responding.
#'
#' A minvar value of .10 was used by Wood & Harms (202x).
#' Note that a value of .0625 (from squaring sd = .25) will be obtained from
#' a respondent who alternates equally between two responses on a 5-point scale.
#'
#' @export

rmvLowVar <- function(data, max, min, minvar){
  data$perVARx <- apply(data, 1, function(x)  (sd(as.matrix(x[2:length(x)], na.rm=F)*(sqrt((length(x)-1)/length(x)))/(max-min)/(.5))^2))
  data <- subset(data, perVARx > minvar)
  data <- data[-length(data)]
  return(data)
}
