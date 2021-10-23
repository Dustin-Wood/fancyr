#' Estimate Percentage Variance from Max Possible
#' @description
#' For a specified set of variables (with a common range), estimate the percentage
#' of response variability that the person showed from maximum possible
#' @param data Matrix to be transformed (often will need to specify subset of matrix)
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @details This is frequently valuable to identify people who did not
#' vary their ratings substantially - which at extreme levels is generally indicative
#' of insufficient effort responding.
#'
#' Note: this will currently provide inaccurate estimates for participants
#' with missing values within the profile
#' @return estimate of the proportion of the responding from max possible (range from 0 to 1)
#' @export
#' @examples
#' #combine with subset function to remove people with less than 5%
#' #of the maximum possible variance over this range (an indicator of invariant responding)
#' datafile$perVar <- perVar(datafile[varSet], 1, 5)
#' subdata <- subset(datafile, perVar > .05)

perVar2<-function (data, smin, smax) {
  perMaxVar <- apply(data, 1, function(x) (sd(as.matrix(x, na.rm = F)) * (sqrt((length(x) - 1)/length(x)))/(smax - smin)/(0.5))^2)
  return(perMaxVar)
}
