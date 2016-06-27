\name{TIPS}
\alias{TIPS}
\docType{data}
\title{
TIPS dataset
}
\description{
One waiter recorded information about each tip he received over a period of a few months working in one restaurant. He collected several variables:
}
\usage{data("TIPS")}
\format{
  A data frame with 244 observations on the following 8 variables.
  \describe{
    \item{\code{TipPercentage}}{a numeric vector, the tip written as a percentage (0-100) of the total bill}
    \item{\code{Bill}}{a numeric vector, the bill amount (dollars)}
    \item{\code{Tip}}{a numeric vector, the tip amount (dollars)}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}, gender of the payer of the bill}
    \item{\code{Smoker}}{a factor with levels \code{No} \code{Yes}, whether the party included smokers}
    \item{\code{Weekday}}{a factor with levels \code{Friday} \code{Saturday} \code{Sunday} \code{Thursday}, day of the week}
    \item{\code{Time}}{a factor with levels \code{Day} \code{Night}, rough time of day}
    \item{\code{PartySize}}{a numeric vector, number of people in party}
  }
}

\source{
This is the \code{Tips} dataset in package \code{reshape}, modified to include the tip percentage.
}

\keyword{datasets}
