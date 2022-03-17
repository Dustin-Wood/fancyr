#' Make a Dataframe with all Combinations of Measurements Made on a Single Person
#' @description
#' This is a crude little function to try to estimate all (N^2-N)/2 combinations
#' of different measurements done on a single person. (Or (N^2-N), if doubled.)
#'
#' Note that the code is built on the assumption that a "person" and "measurement"
#' identifier are the first two columns of your dataset.
#' @param data The data that will be transformed
#' (should be in \code{c(p,m,ratings...)} format.)
#' @usage
#' allPairs(pmRatingData)
#' @return a dataframe where all possible pairwise combinations of different measurements
#' are given on separate lines
#'
#' @export

allPairs <- function(data, double = F) {
  #for each person, create numerical m
  #ranging from 1 to number of measurements
  #for person p (ordered first to last)

  #rename first two columns
  colnames(data)[1:2] <- c("p","m")
  data<-data[order(data[1],data[2]),] # order by ID and time rating was done
  nm = 1
  data$nm[1] <- nm
  for(i in 2:nrow(data)){
    ifelse(data[i,1]==data[i-1,1], nm <- nm+1, nm <- 1)
    data$nm[i] <- nm
  }

  max.nm = max(data$nm) #maximum number of measurements per a particular person
  for (i in 1:max.nm) {
    lagdata <- data

    #make column names unique
    colnames(lagdata)[2:ncol(lagdata)] <- paste0("d.",colnames(lagdata)[2:ncol(lagdata)])

    #provide the match interval to match with
    lagdata$nm <- lagdata$d.nm - i

    step_i <- merge(data,lagdata, by=c("p","nm"))
    ifelse(i==1,pdata <- step_i, pdata <- rbind(pdata,step_i))

  }

  #remove "nm" and "d.nm" columns that were added to help with matching
  pdata <- subset(pdata, select = -c(nm,d.nm))

  if(double == T) {
    yDbl <- data.frame(pdata[1],pdata[(((ncol(pdata)+1)/2)+1):ncol(pdata)],pdata[2:((ncol(pdata)+1)/2)])
    colnames(yDbl) <- colnames(pdata)
    pdata <- rbind(pdata,yDbl)
  }

  return(pdata)
}

