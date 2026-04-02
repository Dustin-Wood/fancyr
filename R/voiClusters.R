#' Validity-Relevant Clusters
#' @description
#' This automates a set of procedures for finding clusters of items
#' that relate in similar ways to a variable of interest.
#'
#' This should produce a set of factors that relates somewhat independently to the variable of interest
#' @param x item set to be associated with the VOI.  It is suggested that items are placed on a [-1,1] range
#' prior to analysis to decrease the variability in cluster scores produced by missing values
#'  (this can be done using the \code{cx} function)
#' @param rvoi items' correlation with the 'variable of interest' (these can be partial correlations)
#' @param threshold minimum correlation to be included in the 'set of relevant items'
#' @param cname name of the cluster (defaults to 'c')
#' @param nk the number of clusters you ultimately wish to extract
#' @param weight should items receive unit-weight in clusters (after clearing threshold),
#' or should they be weighted by level of association with the VOI?
#' @param controls optional data frame of control variables; if provided, partial correlations
#' are used instead of zero-order correlations
#' @param item_labels optional data frame with a \code{var} column (matching item names) and
#' a label column for human-readable item descriptions. If \code{NULL} (default), item names
#' are used as-is.
#' @return cluster scores, cluster membership, and full dendrogram
#'
#' @export

voiClusters <- function(x, rvoi, threshold = .15, nk = 6, cname = "c", weight = "unit",
                        controls = NULL, item_labels = NULL) {

  rsub <- subset(rvoi, abs(rvoi) > threshold)
  #make a restricted x dataset to items over the threshold association
  xVOI <- x[rownames(rsub)]
  rev <- as.numeric(rsub > 0)
  rev[rev == 0] <- -1
  #score items in the restricted dataset by their direction of association with VOI
  if(weight == "unit") {
    xVOIrev <- t(t(xVOI)*rev)
  }
  if(weight == "voi") {
    xVOIrev <- t(t(xVOI)*as.numeric(rsub))
  }
  #indicate what items were reversed by this procedure
  rrev <- rsub*rev
  rev[rev == 1] <- ""
  rev[rev == -1] <- "(R)"
  rsubs <- data.frame(rownames(rrev), rrev, rev)
  colnames(rsubs) <- c("var", "rVOI", "rev")
  rownames(rsubs) <- NULL

  if (!is.null(item_labels)) {
    voiItems <- merge(item_labels, rsubs, by = "var")
    label_col <- setdiff(colnames(item_labels), "var")[1]
    voiItems$fullLabel_R <- paste0(voiItems[[label_col]], voiItems$rev)
  } else {
    voiItems <- rsubs
  }

  if (is.null(controls)) {
    cors <- psych::corr.test(xVOIrev)$r
  } else {
    combined <- data.frame(xVOIrev, controls)
    cors <- psych::partial.r(combined, colnames(xVOIrev), colnames(controls))
  }
  #place together profiles that were extracted similarly across the subsets
  fit <- hclust(as.dist((1-cors)), method="ward.D2")

  cluster <- paste0(cname, nk) #indicate the number of clusters extracted in this name
  bigCluster <- data.frame(cutree(fit, k=nk)) ; names(bigCluster) <- cluster
  voiItemsCluster <- merge(voiItems, bigCluster, by.x = "var", by.y = 0)
  #if you want to graph it:
  plot(fit, hang = -1)
  stats::rect.hclust(fit, k=nk, border="gray")

  test <- data.frame(bigCluster, t(xVOIrev))
  test2 <- plyr::ddply(test, cluster, function(x) colMeans(x, na.rm = TRUE))

  clusterScores <- t(test2[-1])

  y <- list(clusterScores, voiItemsCluster, fit)
  names(y) <- c("clusterScores", "clusterItems", "dendro")
  return(y)
}
