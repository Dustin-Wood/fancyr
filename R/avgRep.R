#' Averages for a Set of Repeated Items
#' @description
#' Provide person-averages for each column, where repeated ratings are given
#' on separate rows.
#'
#' @param set1 first set of ratings
#' @param set2 second set of ratings
#' @param p person (or subject/case) identifier; should be given as first column
#'  Note that if "p = 0" is given, then this will match on the row.names
#'  of the datasets
#'
#' @details Currently omits a person who has *any* NA for a single variable - which
#' would correspond to the person never rating the variable in either first or second repetition
#' @return Person's (p) average score for variable in each column
#' @export

avgRep <- function(set1,set2, p = "p") {
  colnames(set2) <- colnames(set1)
  if(p == 0) {
    set1 <- data.frame(row.names(set1),set1); colnames(set1)[1] <- "p"
    set2 <- data.frame(row.names(set2),set2); colnames(set2)[1] <- "p"
    p = "p"
  }
  t_set<-rbind(set1,set2)
  set<-plyr::ddply(t_set, c(p), function(x) colMeans(x[2:ncol(x)], na.rm = T))
  set<-set[complete.cases(set),]
  return(set)
}
