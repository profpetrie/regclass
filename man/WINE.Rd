\name{WINE}
\alias{WINE}
\docType{data}
\title{
WINE data
}
\description{
Predicting the quality of wine based on its chemical characteristics
}
\usage{data("WINE")}
\format{
  A data frame with 2700 observations on the following 12 variables.
  \describe{
    \item{\code{Quality}}{a factor with levels \code{high} \code{low}}
    \item{\code{fixed.acidity}}{a numeric vector}
    \item{\code{volatile.acidity}}{a numeric vector}
    \item{\code{citric.acid}}{a numeric vector}
    \item{\code{residual.sugar}}{a numeric vector}
    \item{\code{chlorides}}{a numeric vector}
    \item{\code{free.sulfur.dioxide}}{a numeric vector}
    \item{\code{total.sulfur.dioxide}}{a numeric vector}
    \item{\code{density}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{sulphates}}{a numeric vector}
    \item{\code{alcohol}}{a numeric vector}
  }
}
\details{
This is the famous wine dataset from the UCI data repository \url{https://archive.ics.uci.edu/ml/datasets/Wine+Quality} with some modifications. Namely, the quality in the original data was a score between 0 and 10.  These has been coded as either high or low.  See description on UCI for description of variables.
}

\references{
P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. 
Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.}

\keyword{datasets}
