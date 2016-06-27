\name{CHARITY}
\alias{CHARITY}
\docType{data}
\title{
CHARITY dataset }
\description{
Charity data (adapted from a small section of a charity's donor database)

}
\usage{data("CHARITY")}
\format{
  A data frame with 15283 observations on the following 11 variables.
  \describe{
    \item{\code{Donate}}{a factor with levels \code{Donate} \code{No}}
    \item{\code{Homeowner}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{Gender}}{a factor with levels \code{F} \code{M}}
    \item{\code{UnlistedPhone}}{a factor with levels \code{No} \code{Yes}}

    \item{\code{ResponseProportion}}{a numeric vector giving the fraction of solications that resulted in a donation}
    \item{\code{NumResponses}}{a numeric vector giving the number of past donations}
    \item{\code{CardResponseCount}}{a numeric vector giving the number of past solicitations}
    \item{\code{MonthsSinceLastResponse}}{a numeric vector giving the number of months since last response to solicitation (which may have been declining to give)}
    \item{\code{LastGiftAmount}}{a numeric vector giving the amount of the last donation}
    \item{\code{MonthSinceLastGift}}{a numeric vector giving the number of months since last donation}
    \item{\code{LogIncome}}{a numeric vector giving the logarithm of a scaled and normalized yearly income}
  }
}
\details{
This dataset is adapted from a real-world database of donors to a charity.
}
\source{Unknown
}

\keyword{datasets}
