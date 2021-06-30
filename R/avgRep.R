#' Averages for a Set of Repeated Items
#' @description
#' Provide person-averages for each column, where repeated ratings are given
#' on separate rows.
#'
#' @param set1 first set of ratings
#' @param set2 second set of ratings
#' @param p person (or subject/case) identifier; should be given as first column
#'
#' @details Currently omits a person who has *any* NA for a single variable - which
#' would correspond to the person never rating the variable in either first or second repetition
#' @return Person's (p) average score for variable in each column
#' @export

avgRep <- function(set1,set2, p = "p") {
  colnames(set2) <- colnames(set1)
  t_set<-rbind(set1,set2)
  set<-ddply(t_set, .(p), function(x) colMeans(x[2:ncol(x)], na.rm = T))
  set<-set[complete.cases(set),]
  return(set)
}
