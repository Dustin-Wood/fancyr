#' Profile Correlation Analyses
#' @description
#' Estimate overall, normative, and distinctive profile correlations
#' between two profiles of scores (adapting procedures from Furr, 2008)
#'
#' @param profile1 First profile
#' @param profile2 Second profile
#' @param smin Scale minimum
#' @param smax Scale maximum
#' @param qc Should scale-center correlations be estimated? (defaults to \code{T})
#' @details Note: both profiles should have the same maximum and minimum values.
#' @return Returns range of profile correlation results
#' \item{profcors}{Person-level profile correlation results}
#' \item{m_q_x1x2.Same}{Average profile correlation when profiles 1 and 2 are from same person}
#' \item{m_q_x1x2.Diff}{Average profile correlation when profiles 1 and 2 are from different people}
#' \item{m_q_d1d2.Same}{Average distinctive profile correlation when profiles 1 and 2 are from same person}
#' \item{m_q_d1d2.Diff}{Average distinctive correlation when profiles 1 and 2 are from different people}
#' \item{m_qAll_x1.diff}{Average profile correlation (qc) for between profile 1 between any two individuals}
#' \item{m_qAll_x2.diff}{Average profile correlation (qc) for between profile 2 between any two individuals}
#' \item{qc}{Are the results from scale-centered (i.e., \code{qc}) correlations?}
#'
#' @export

profileAnalysis <- function(profile1, profile2, smin, smax, qc = T) {
  #transform both profiles 1 and 2 to a [-1,1] scale
  x1 <- fancyr::cx(profile1,smin,smax)
  x2 <- fancyr::cx(profile2,smin,smax)

  #add reflected profiles around 0 if qc = T
  if(qc == T) {
    x1 <- cbind(x1,-x1)
    x2 <- cbind(x2,-x2)
  }

  #compute means
  m1 <- colMeans(x1, na.rm = T)
  m2 <- colMeans(x2, na.rm = T)

  #compute mean deviations
  d1 <-  apply(x1, 2, function(x) x - mean(x, na.rm=TRUE))
  d2 <-  apply(x2, 2, function(x) x - mean(x, na.rm=TRUE))

  #estimate profile correlations
  #this is the 'business-as-usual profile correlation.  Uses person's own mean.
  q_x1x2<-diag(cor(t(x1),t(x2),use="complete.obs"))

  qAll_x1x2<-cor(t(x1),t(x2),use="complete.obs")
  m_q_x1x2 <-mean(diag(qAll_x1x2))
  q_x1x2 <- diag(qAll_x1x2)
  diag(qAll_x1x2) <- NA
  m_q_x1x2.diff<-mean(qAll_x1x2, na.rm = T)

  q_x1m2<-cor(t(x1),m2,use="complete.obs")
  q_x2m1<-cor(t(x2),m1,use="complete.obs")
  q_d1d2<-diag(cor(t(d2),t(d1),use="complete.obs"))

  qAll_d1d2<-cor(t(d2),t(d1),use="complete.obs")
  m_q_d1d2 <-mean(diag(qAll_d1d2))
  q_d1d2 <- diag(qAll_d1d2)
  diag(qAll_d1d2) <- NA
  m_q_d1d2.diff<-mean(qAll_d1d2, na.rm = T)

  q_x1m1<-cor(t(x1),m1,use="complete.obs")
  q_x2m2<-cor(t(x2),m2,use="complete.obs")

  qAll_x1<-cor(t(x1),use="complete.obs")
  diag(qAll_x1) <- NA
  m_qAll_x1.diff<-mean(qAll_x1, na.rm = T)

  qAll_x2<-cor(t(x2),use="complete.obs")
  diag(qAll_x2) <- NA
  m_qAll_x2.diff<-mean(qAll_x2, na.rm = T)

  profcors <- data.frame(q_x1x2,q_x1m2,q_x2m1,q_d1d2,q_x1m1,q_x2m2)
  results<-list(profcors,m_q_x1x2,m_q_x1x2.diff,m_q_d1d2,m_q_d1d2.diff,m_qAll_x1.diff,m_qAll_x2.diff, qc)
  names(results)<-c("profcors","m_q_x1x2.Same","m_q_x1x2.Diff","m_q_d1d2.Same","m_q_d1d2.Diff","m_qAll_x1.diff","m_qAll_x2.diff","qc")
  return(results)
}

