#' Clusters with Reflected Items
#' @description
#' Estimate the clusters from an item set, but do so only after including
#' all 'reflected items'.  This allows clusters to be formed containing
#' antonymous content
#'
#' @param corrs the correlation matrix including all correlations between variables in the item set
#'
#' @export
reflectedClusters <- function(itemRs, nclusters = 5) {
  dbl_rxy.t <- reflectedRs(itemRs)

  #convert to a 'distance' matrix, and do cluster analysis
  d<-as.dist(sqrt(1-dbl_rxy.t))
  fit <- hclust(d, method="ward.D2")
  tree <- as.dendrogram(fit, hang=0) #normally I would specify 'hang=1', but when labels are long, this is the only way I've figured to be able to see most of the full items

  #identify which items were placed in which cluster,
  #and whether the included variable was a reversal of the original variable
  groups <- data.frame(cutree(fit, k=nclusters*2))
  colnames(groups) <- c("group")
  #add the reversals' groups
  groups$revgroup<-groups$group[c((.5*nrow(groups)+1):nrow(groups),1:(.5*nrow(groups)))]
  #identify minimum cluster number
  groups$min <- as.numeric(apply(groups,1,min))
  #save 'original order with reversals
  groups$oowr <- row(groups[1])
  #indicate whether the item is reverse-scored within the cluster it is assigned
  groups$rev <- as.numeric(groups$oowr > .5*nrow(groups))
  #keep only one version of the item - the one in the first dendrogram
  groups2 <- subset(groups, group==min)

  #add back in the original variable names
  #place in the REAL original order of the variables
  groups2$oo <- ifelse(groups2$oowr <= nrow(groups2), groups2$oowr, groups2$oowr - nrow(groups2))
  groups2 <- groups2[order(groups2$oo),]
  #add the original variable names back in
  groups2$var <- colnames(itemRs)

  finalgroups <- data.frame(groups2$var,groups2$min,groups2$rev)
  colnames(finalgroups)<-c("variable","cluster","reversed")

  #add size of each cluster (i.e., number of items included in each)
  nmembers<-as.data.frame(table(finalgroups$cluster))
  colnames(nmembers)<- c("cluster","ClusterSize")

  finalgroups<-merge(finalgroups, nmembers, by.x = "cluster")

  reflectedClusters<-list(finalgroups,fit)
  names(reflectedClusters)<-c("clusters","dendro")
  return(reflectedClusters)
}

