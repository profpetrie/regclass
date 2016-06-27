\name{BULLDOZER}
\alias{BULLDOZER}
\docType{data}
\title{
BULLDOZER data
}
\description{
Predicting the sales price of a bulldozer at auction
}
\usage{data("BULLDOZER")}
\format{
  A data frame with 924 observations on the following 6 variables.
  \describe{
    \item{\code{SalePrice}}{a numeric vector}
    \item{\code{YearsAgo}}{a numeric vector, the number of years ago (before present) that the sale occurred}
    \item{\code{YearMade}}{a numeric vector, year of manufacture of machine}
    \item{\code{Usage}}{a numeric vector, hours of usage at time of sale}
    \item{\code{Blade}}{a numeric vector, width of the bulldozer blade (feet)}
    \item{\code{Tire}}{a numeric vector, size of primary tires}
  }
}
\details{
The goal is to predict the sale price of a particular piece of heavy equiment at auction based on its usage, equipment type, and configuration.  The data represents a heavily modified version of competition data found on kaggle.com.  See original source for actual dataset}
\references{
\url{https://www.kaggle.com/c/bluebook-for-bulldozers}
}

\keyword{datasets}
