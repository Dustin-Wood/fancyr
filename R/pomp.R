#' Proportion of Maximum Possible (POMP) Scores
#' @description
#' Transforms scores of interest from original metric to range of 0 to 1 (if
#' using \strong{proportion} scaling) or range of 0 to 100 (if using
#' \strong{percentage} scaling.
#' @param x matrix to be transformed
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @param p Return \emph{proportion} or \emph{percent}? (default proportion)
#' @usage
#' pomp(x,1,5) #usage for a standard 1-to-5 Likert scale
#' pomp(x,min(x),max(x)) #possible usage if you trust some raters hit minimum and maximum ratings at some point
#' @return Scores with maximum possible range of [0,1] (if \strong{proportion}
#' specified) or [0,100] (if \strong{percent} specified)
#'
#' @export

pomp <- function(x, smin, smax, p = "proportion") {
  if (p == "proportion") {
    px <- (x - smin)/(smax-smin)
}
  if (p == "percent") {
    px <- (x - smin)/(smax-smin)*100
  }
return(px)
}
