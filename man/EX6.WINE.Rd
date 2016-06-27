\name{EX6.WINE}
\alias{EX6.WINE}
\docType{data}
\title{
WINE data for Exercise 3 Chapter 6 }
\description{
WINE data for Exercise 3 Chapter 6 
}
\usage{data("EX6.WINE")}
\format{
  A data frame with 2700 observations on the following 12 variables.
  \describe{
    \item{\code{Quality}}{a factor with levels \code{High} \code{Low}}
    \item{\code{fixed.acidity}}{a numeric vector}
    \item{\code{volatile.acidity}}{a numeric vector}
    \item{\code{citric.acid}}{a numeric vector}
    \item{\code{residual.sugar}}{a numeric vector}
    \item{\code{free.sulfur.dioxide}}{a numeric vector}
    \item{\code{total.sulfur.dioxide}}{a numeric vector}
    \item{\code{density}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{sulphates}}{a numeric vector}
    \item{\code{alcohol}}{a numeric vector}
    \item{\code{chlorides}}{a factor with levels \code{Little} \code{Lots}}
  }
}
\details{
Adapted from the wine quality dataset at the UCI data repository.  In this case, the original quality metric has been recoded from a score between 0 and 10 to either \code{High} or \code{Low}, and the \code{chlorides} is treated here as a categorical variable instead of a quantitative variable.
}
\source{
\url{https://archive.ics.uci.edu/ml/datasets/Wine+Quality}

P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. 
In Decision Support Systems, Elsevier, 47(4):547-553, 2009.
}
\keyword{datasets}
