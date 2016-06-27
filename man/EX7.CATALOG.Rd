\name{EX7.CATALOG}
\alias{EX7.CATALOG}
\docType{data}
\title{
CATALOG data for Exercise 2 in Chapters 7 and 8
}
\description{
CATALOG data for Exercise 2 in Chapters 7 and 8
}
\usage{data("EX7.CATALOG")}
\format{
  A data frame with 4000 observations on the following 7 variables.
  \describe{
    \item{\code{Buy}}{a factor with levels \code{No} \code{Yes}, whether customer made a purchase through the catalog next quarter}
    \item{\code{QuartersWithPurchase}}{a numeric vector, number of quarters where customer made a purchase through the catalog}
    \item{\code{PercentQuartersWithPurchase}}{a numeric vector, percentage of quarters where customer made a purchase through the catalog}
    \item{\code{CatalogsReceived}}{a numeric vector, total number of catalogs customer has received}
    \item{\code{DaysSinceLastPurchase}}{a numeric vector, number of days since customer placed his or her last order}
    \item{\code{AvgOrderSize}}{a numeric vector, the typical number of items per order when customers buys through the catalog}
    \item{\code{LifetimeOrder}}{a numeric vector, the number of orders the customer has placed through the catalog}
  }
}
\details{
The original source of this data is lost, but it is likely adapted from real data.
}

\keyword{datasets}
