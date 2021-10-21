#' Estimate Percentage Variance from Max Possible
#' @description
#' For a specified set of variables (with a common range), estimate the percentage
#' of response variability that the person showed from maximum possible
#' @param data Matrix to be transformed
#' @param varSet Set of variables to estimate variance over (note: should all have same range)
#' @param smax Scale maximum
#' @param smin Scale minimum
#' @details This is frequently valuable to identify people who did not
#' vary their ratings substantially - which at extreme levels is generally indicative
#' of insufficient effort responding.
#' @return estimate of the proportion of the responding from max possible (range from 0 to 1)
#' @export
#' @examples
#' #combine with subset function to remove people with less than 5%
#' #of the maximum possible variance over this range (an indicator of invariant responding)
#' data$perVar <- perVar(data, varSet, 1, 5)
#' subdata <- subset(data, perVar > .05)

#Remove cases with negligible response variance
perVar<-function (data, varSet, smax, smin) {
  perMaxVar <- apply(data[varSet], 1, function(x) (sd(as.matrix(x, na.rm = F)) * (sqrt((length(x) - 1)/length(x)))/(smax - smin)/(0.5))^2)
  return(perMaxVar)
}
