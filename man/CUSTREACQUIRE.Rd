\name{CUSTREACQUIRE}
\alias{CUSTREACQUIRE}
\docType{data}
\title{
CUSTREACQUIRE dataset }
\description{
Customer reacquisition

}
\usage{data("CUSTREACQUIRE")}
\format{
  A data frame with 500 observations on the following 9 variables.
  \describe{
    \item{\code{Reacquire}}{a factor with levels \code{No} \code{Yes} indicating whether a customer who has previously churned was reacquired}
    \item{\code{Lifetime2}}{a numeric vector giving the days that the company was considered a customer}
    \item{\code{Value2}}{a numeric vector giving the lifetime value of the customer (related to the amount of money spent on reacquisition and the revenue brought in by the customer; can be negative)}
    \item{\code{Lifetime1}}{a numeric vector giving the days that the company was considered a customer before churning the first time}
    \item{\code{OfferAmount}}{a numeric vector giving the money equivalent of a special offer given to the former customer in an attempt to reacquire}
    \item{\code{Lapse}}{a numeric vector giving the number of days between the customer churning and the time of the offer}
    \item{\code{PriceChange}}{a numeric vector giving the percentage by which the typical product purchased by the customer has changed from the time they churned to the time the special offer was sent}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male} giving the gender of the customer}
    \item{\code{Age}}{a numeric vector giving the age of the customer}
  }
}
\details{
A company kept records of its success in reacquiring customers that had previously churned.  Data is based on a previous case study.

}
\source{Unknown}

\keyword{datasets}
