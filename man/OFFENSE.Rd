\name{OFFENSE}
\alias{OFFENSE}
\docType{data}
\title{
Some offensive statistics from \code{NFL} dataset
}
\description{
A subset of the \code{NFL} dataset contain some statistics of teams on offense
}
\usage{data("OFFENSE")}
\format{
  A data frame with 352 observations on the following 10 variables.
  \describe{
    \item{\code{Win}}{a numeric vector, number of wins of team over the season (0-16)}
    \item{\code{FirstDowns}}{a numeric vector, number of first downs made over the season}
    \item{\code{PassingYards}}{a numeric vector, number of passing yards over the season}
    \item{\code{Interceptions}}{a numeric vector, number of times ball was intercepted on offense}
    \item{\code{RushingYards}}{a numeric vector, number of rushing yards over the season}
    \item{\code{Fumbles}}{a numeric vector, number of fumbles made on offense}
    \item{\code{X1to19FGAttempts}}{a numeric vector, number of field goal attempts made from 1-19 yards}
    \item{\code{X20to29FGAttempts}}{a numeric vector, number of field goal attemps made from 20-29 yards}
    \item{\code{X30to39FGAttempts}}{a numeric vector}
    \item{\code{X40to50FGAttempts}}{a numeric vector}
  }
}
\details{
A small subset of the \code{NFL} dataset contain select statistics.  Seasons are from 2002-2012
}
\keyword{datasets}
