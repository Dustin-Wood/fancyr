# Package-level imports and global variable declarations

#' @importFrom graphics abline arrows hist lines par text
#' @importFrom stats as.dendrogram as.dist complete.cases cor cutree hclust quantile rect.hclust sd setNames var
#' @importFrom utils combn
NULL

# Suppress R CMD check notes about variables used in data-masking contexts
utils::globalVariables(c("d.nm", "nna", "var", "group"))
