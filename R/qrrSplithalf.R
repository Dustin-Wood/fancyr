#' Split-Half Correlation Vector Correlation Matrix
#' @description TBD
#' @param set Set of variables and scores you wish to perform analysis on
#' @param sims A numeric specifying the number of random splithalves to generate.
#' @returns List of all separate split-half correlation vector correlation matrices
#' @export


qrrSplithalf <-function(set, sims=100, graph=FALSE, CI=.95, minval=-1.0, seed=F) {
    comp.data <- subset(set, complete.cases(set))
    N <- nrow(comp.data)
    half.N <- N/2
    coded <- seq(1:N)
    LL <- (1 - CI) / 2
    UL <- 1 - LL

    if(seed!=F) {set.seed(seed)}

      # my adaptation, for saving all the resulting matrices.
      #  Changes noted, with rationale given:
      storer <- list() #' make a list instead of a vector
      for(i in 1:sims) {
        rand.assign <- sample(coded, N, FALSE)
        R1 <- as.matrix(cor(comp.data[rand.assign <= half.N,]))
        R2 <- as.matrix(cor(comp.data[rand.assign > half.N,]))
        # need to get rid of the diagonals to avoid big problems
        diag(R1) <- NA
        diag(R2) <- NA
        # calculate the correlations-of-vectors-of-correlations.
        #  There is no Spearman-Brown prophecy adjustment to the estimate as was done in S&W paper
        storer[[i]] <- cor(R1, R2, use = "pairwise.complete.obs")
      }
      #the rest is ancestral code that isn't used for this function currently,
        #and maybe doesn't apply if we are returning many matrices rather than a single correlation
      Up.r <- Reduce("+", storer) / length(storer)

      if(graph==T) {
        op <- par(las=1, font.main=1)
        hist(storer, main="Histogram of Split-Half rs for Corrs", xlab="Split-Half r Values", ylab="Frequency", col="cyan")
        abline(v=Up.r, col="red")
        abline(v=quantile(storer, LL), col="orange")
        abline(v=quantile(storer, UL), col="orange")
      }
#      Up.r <- ifelse(Up.r >= minval, Up.r, minval)
#      out <- cbind(N, Up.r, sd(storer), quantile(storer, LL), quantile(storer, UL))
#      colnames(out) <- c("N", "Split-Half r", "SE", "LL", "UL")
#      rownames(out) <- c("Corr")


  return(storer)
  }
