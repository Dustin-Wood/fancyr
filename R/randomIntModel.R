#' Make a 'Random Intercept Model' for MSEM analysis in lavaan
#' @description
#' This provides code that should run a pretty agnostic 'random intercept model' in
#' \code{lavaan}, in much the manner detailed by Mehta & Neale (2005, Figure 3).
#' @param varset the set of variables you want to include in the null model
#' @usage
#' randomIntModel(pmRatingData[3:ncol(pmRatingData)]) #if first two variables
#' are 'p' and 'm' and all the remaining variables are variable ratings
#' @details
#' Unfortunately, there is a good chance that if you have a large number of
#' variables (or perhaps a large number of clusters, or observations within clusters, or...
#' I don't know the parameters), that this will take basically forever to run.
#' Unfortunately I don't have any ideas about what causes this or how to fix it.
#' @return the text you need to put into \code{model} for \code{lavaan::sem(model, ...)}.
#' Note that original variables \code{x} will be relabeled \code{wx} and and \code{bx} to
#' indicate within- and between- levels, respectively.
#'
#' @export


randomIntModel <- function(varset) {
  step11<-paste0("\n w",colnames(varset)," =~ 1*",colnames(varset))
  step12<-paste(step11,collapse = '')
  step21<-paste0("\n b",colnames(varset)," =~ 1*",colnames(varset))
  step22<-paste(step21,collapse = '')
  model <- paste("\nlevel: 1",step12,"\n\nlevel: 2",step22,"\n")
  return(model)
}
