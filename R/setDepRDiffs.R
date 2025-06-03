#' Estimate How Two (Correlated) Variables Differ in Associations with a Set of Predictors
#' @description This basically runs the psych package's \code(r.test) function
#' through a set of 'predictor' variables to make it easier to find which variables
#' within that set differ significantly in their associations with the two variables
#' @param predset Set of 'Predictor' variables that will be associated with variables 2 and 3
#' @param dvset Set the Outcome variables will be coming from
#' @param var2 name of Variable 2 (as text, a la "variable")
#' @param var3 name of Variable 3 (as text, a la "variable")
#' @return Most importantly: statistical significance of the differences
#' between all r(i,2) and r(i,3) associations
#'
#'
#' @export

setDepRDiffs <- function(predset, dvset, controls = NULL) {
var2<-colnames(dvset)[1]
var3<-colnames(dvset)[2]
if(is.null(controls)) {
  cors<-corr.test(cbind(predset,dvset), use = "complete")
} else {
  test_data <- cbind(predset,dvset,controls)
  rs <- corr.test(test_data, ci = FALSE)
  pars <- partial.r(data = test_data, x = 1:ncol(test_data), y = colnames(controls))
  cors <- corr.p(pars, n = (rs$n - length(controls)), adjust = "none", ci = FALSE)
}

  #what contrast do you want?
  deprs <- list()
  for(i in 1:ncol(predset)) {
    deprs[[paste0("item",i)]] <- psych::r.test(n = min(cors$n),r12 = cors$r[i,var2],r13 = cors$r[i,var3],r23 = cors$r[var2,var3])
    deprs[[paste0("item",i)]] <- c(deprs[[paste0("item",i)]],
                                   list(n = min(cors$n),r12 = cors$r[i,var2],r13 = cors$r[i,var3],r23 = cors$r[var2,var3]))
  }
  #pull all into a single dataframe
  deprs2<-plyr::ldply(deprs, function(x) cbind((x$r12-x$r13),x$t,x$p,x$n,x$r12,x$r13,x$r23))
  colnames(deprs2) <- c("var","diff","t","p","n",var2,var3,"r23")
  #add in the differences?
  deprs2$dir <- deprs2$t/abs(deprs2$t)
  deprs2$sig <- 1*(deprs2$p < .05)
  deprs2$sig[deprs2$sig == 0] <- NA
  deprs2$var <- colnames(predset)
  return(deprs2)
}


