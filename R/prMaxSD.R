#' Estimate Proportion of Observed Standard Deviation from Max Possible
#' @description
#' For a specified set of variables (with a common range), estimate the percentage
#' of response variability that the person showed from maximum possible
#' @param data Matrix to be transformed (often will need to specify subset of larger dataframe)
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @details This is frequently valuable to identify people who did not
#' vary their ratings substantially - which at extreme levels is generally indicative
#' of insufficient effort responding.
#'
#' Note: prMaxSD = .25 would be observed on a 5-point scale from a person rating ALL
#' items as 50\% one number and 50\% the adjacent number (e.g., 50\% 1's and 50\% 2's, or
#' 50\% 3's and 50\% 4's).
#'
#' @return estimate of the proportion of the observed standard deviation
#' of the row scores from max possible (range from 0 to 1)
#' @export
#' @examples
#' #combine with subset function to remove people with
#' #less than 25% of the maximum possible SD over this range
#' #(an indicator of invariant or insufficient effort responding)
#' datafile$prMaxSD <- prMaxSD(datafile[varSet], 1, 5)
#' subdata <- subset(datafile, prMaxSD > .05)

prMaxSD <- function(data, smin, smax, dir=1) {
  sd.p <- function(x) { sd(as.matrix(x), na.rm = T)}
  prMaxSD <- apply(data, dir, function(x) sd.p(x)*sqrt((sum(!is.na(x))-1)/sum(!is.na(x)))/((smax-smin)/2))
  return(prMaxSD)
}


