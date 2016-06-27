\name{PURCHASE}
\alias{PURCHASE}
\docType{data}
\title{
PURCHASE data}
\description{
Purchase habits of customers
}
\usage{data("PURCHASE")}
\format{
  A data frame with 27723 observations on the following 6 variables.
  \describe{
    \item{\code{Purchase}}{a factor with levels \code{Buy} \code{No}, whether the customer made a purchase in the following 30 days}
    \item{\code{Visits}}{a numeric vector, number of visits customer has made to the chain in last 90 days}
    \item{\code{Spent}}{a numeric vector, amount of money customer has spent at the chain the last 90 days}
    \item{\code{PercentClose}}{a numeric vector, the percentage of customers' purchases that occur within 5 miles of their home}
    \item{\code{Closest}}{a numeric vector, the distance between the customer's home and the nearest store in the chain}
    \item{\code{CloseStores}}{a numeric vector, the number of stores in the chain within 5 miles of the customer's home}
  }
}
\details{
A nation-wide chain is curious as to whether it can predict whether a former customer will make a purchase at one of its stores in the next 30 days based on the customer's spending habits.  Some variables are known by the chain (e.g., \code{Visits}) and some are available to purchase from credit card companies (e.g., \code{PercentClose}).  Is purchasing additional information about the customer worth it?
}
\source{
Adapted from real data on the condition that neither the name of the chain nor other parties be disclosed.
}

\keyword{datasets}
