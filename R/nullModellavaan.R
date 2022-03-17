#' Make a 'Null' model for MSEM analysis in lavaan
#' @description
#' This simple little thing is just here to automate creation of 'null' models
#' that separate the within and between variance on scores for all variables in
#' a set, without then going and trying to say anything about how level1 or level2
#' variance is predicted by anything.
#' @param varset the set of variables you want to include in the null model
#' @usage
#' nullModellavaan(pmRatingData[3:ncol(pmRatingData)]) #if first two variables are 'p' and 'm' and all the remaining variables are variable ratings
#' @return the text you need to put into \code{model} for \code{lavaan::sem(model, ...)}
#'
#' @export


nullModellavaan <- function(varset) {

step1<-paste0("\n",colnames(varset)," ~~ ",colnames(varset))
step2<-paste(step1,collapse = '')
nullMSEMmodel <- paste("\nlevel: 1",step2,"\n\nlevel: 2",step2,"\n")
return(nullMSEMmodel)
}
