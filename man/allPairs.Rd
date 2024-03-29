% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allPairs.R
\name{allPairs}
\alias{allPairs}
\title{Make a Dataframe with all Combinations of Measurements Made on a Single Person}
\usage{
allPairs(pmRatingData)
}
\arguments{
\item{data}{The data that will be transformed
(should be in \code{c(p,m,ratings...)} format.)}
}
\value{
a dataframe where all possible pairwise combinations of different measurements
are given on separate lines
}
\description{
This is a crude little function to try to estimate all (N^2-N)/2 combinations
of different measurements done on a single person. (Or (N^2-N), if doubled.)

Note that the code is built on the assumption that a "person" and "measurement"
identifier are the first two columns of your dataset.
}
