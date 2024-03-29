#' Invert a 'P-by-Order (with Vars)' Matrix to 'P-by-Var (with Orders)' Matrix
#' @description
#' If working with a matrix that shows which item each participant completed 1st, 2nd, etc,
#' it will often be more useful to invert this to show in which order the participant completed
#' item #1, #2, etc.
#' @param pByOrder dataframe containing participant ID as column 1, and then columns describing
#' which item was rated first, second, etc
#' @param vOrder specify the order you would like the variables to be ordered (this is necessarily
#' mainly if the cells of the pByOrder matrix are variable names rather than variable NUMBERS)
#' @usage
#' invertVarOrder(pByOrder)
#' invertVarOrder(pByOrder, vorder = pscores)
#' @return A participant-by-item matrix detailing when participant rated each item
#'
#' @export


invertVarOrder <- function(pByOrder, vorder = NA){
  longMat<-tidyr::pivot_longer(pByOrder, cols = 2:ncol(pByOrder), names_to = "order", values_to = "var")

  #remove problematic X's and V's that will get in the way of making scores numeric
  longMat$order<-stringr::str_replace(longMat$order, "X", "")
  longMat$order<-stringr::str_replace(longMat$order, "V", "")
  longMat$order<-as.numeric(longMat$order)

  #sort by variables to make place pivoted table in best order
  longMat<-longMat[order(longMat$var),]
  #remove any missing values before pivoting
  longMat <- subset(longMat, is.na(var)==0)
  longMat <- subset(longMat, var != "")

  pByVar <- as.data.frame(tidyr::pivot_wider(longMat, names_from = var, values_from = order))
  if(sum(is.na(vorder)) == 0){
    pByVar<-pByVar[,vorder]
  }
  return(pByVar)
}
