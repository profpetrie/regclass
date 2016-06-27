\name{EX4.STOCKS}
\alias{EX4.STOCKS}
\docType{data}
\title{
Stock data for Exercise 2 in Chapter 4
}
\description{
Stock data for Exercise 2 in Chapter 4
}
\usage{data("EX4.STOCKS")}
\format{
  A data frame with 216 observations on the following 41 variables.
  \describe{
    \item{\code{AA}}{a numeric vector}
    \item{\code{AAPLlag2}}{a numeric vector}
    \item{\code{AXPlag2}}{a numeric vector}
    \item{\code{BAlag2}}{a numeric vector}
    \item{\code{BAClag2}}{a numeric vector}
    \item{\code{CATlag2}}{a numeric vector}
    \item{\code{CSCOlag2}}{a numeric vector}
    \item{\code{CVXlag2}}{a numeric vector}
    \item{\code{DDlag2}}{a numeric vector}
    \item{\code{DISlag2}}{a numeric vector}
    \item{\code{GElag2}}{a numeric vector}
    \item{\code{HDlag2}}{a numeric vector}
    \item{\code{HPQlag2}}{a numeric vector}
    \item{\code{IBMlag2}}{a numeric vector}
    \item{\code{INTClag2}}{a numeric vector}
    \item{\code{JNJlag2}}{a numeric vector}
    \item{\code{JPMlag2}}{a numeric vector}
    \item{\code{KOlag2}}{a numeric vector}
    \item{\code{MCDlag2}}{a numeric vector}
    \item{\code{MMMlag2}}{a numeric vector}
    \item{\code{MRKlag2}}{a numeric vector}
    \item{\code{MSFTlag2}}{a numeric vector}
    \item{\code{PFElag2}}{a numeric vector}
    \item{\code{PGlag2}}{a numeric vector}
    \item{\code{Tlag2}}{a numeric vector}
    \item{\code{TRVlag2}}{a numeric vector}
    \item{\code{UNHlag2}}{a numeric vector}
    \item{\code{VZlag2}}{a numeric vector}
    \item{\code{WMTlag2}}{a numeric vector}
    \item{\code{XOMlag2}}{a numeric vector}
    \item{\code{Australialag2}}{a numeric vector}
    \item{\code{Copperlag2}}{a numeric vector}
    \item{\code{DollarIndexlag2}}{a numeric vector}
    \item{\code{Europelag2}}{a numeric vector}
    \item{\code{Exchangelag2}}{a numeric vector}
    \item{\code{GlobalDowlag2}}{a numeric vector}
    \item{\code{HongKonglag2}}{a numeric vector}
    \item{\code{Indialag2}}{a numeric vector}
    \item{\code{Japanlag2}}{a numeric vector}
    \item{\code{Oillag2}}{a numeric vector}
    \item{\code{Shanghailag2}}{a numeric vector}
  }
}
\details{
The goal is to predict the closing price of Alcoa stock (\code{AA}) from the closing prices of other stocks and commodities two days prior (\code{IMBlag2}, \code{HongKonglag2}, etc.).  If this were possible, and if the association between the prices continued into the future, it would be possible to use this information to make smart trades.
}
\source{
Compiled from various sources on the internet, e.g., Yahoo historical prices.
}
\keyword{datasets}
