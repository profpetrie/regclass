\name{EX3.ABALONE}
\alias{EX3.ABALONE}
\docType{data}
\title{
ABALONE dataset for Exercise D in Chapter 3 
}
\description{
ABALONE dataset for Exercise D in Chapter 3 
}
\usage{data("EX3.ABALONE")}
\format{
  A data frame with 1528 observations on the following 7 variables.
  \describe{
    \item{\code{Length}}{a numeric vector}
    \item{\code{Diameter}}{a numeric vector}
    \item{\code{Height}}{a numeric vector}
    \item{\code{Whole.Weight}}{a numeric vector}
    \item{\code{Meat.Weight}}{a numeric vector}
    \item{\code{Shell.Weight}}{a numeric vector}
    \item{\code{Rings}}{a numeric vector}
  }
}
\details{
Abalone are sea creatures that are considered a delicacy and have very pretty iridescent shells.  See \url{https://en.wikipedia.org/wiki/Abalone}.  Predicting the age of the abalone from physical measurements could be useful for harvesting purposes.  Dimensions are in mm and weights are in grams.  \code{Rings} is an indicator of the age of the abalone (Age is about 1.5 plus the number of rings).
}
\source{
Data is adapted from the abalone dataset on UCI Data Repository \url{https://archive.ics.uci.edu/ml/datasets/Abalone}.  Only the male abalone are represented in this dataset.
}
\references{
See page on UCI for full details of owner and donor of this data.
}
\keyword{datasets}
