\name{AUTO}
\alias{AUTO}
\docType{data}
\title{
AUTO dataset
}
\description{
Characteristics of cars from 1991
}
\usage{data("AUTO")}
\format{
  A data frame with 82 observations on the following 5 variables.
  \describe{
    \item{\code{CabVolume}}{a numeric vector, cubic feet of cab space}
    \item{\code{Horsepower}}{a numeric vector, engine horsepower}
    \item{\code{FuelEfficiency}}{a numeric vector, average miles per gallon}
    \item{\code{TopSpeed}}{a numeric vector, miles per hour}
    \item{\code{Weight}}{a numeric vector, in units of 100 lbs}
  }
}
\details{
Although this is a popular dataset, there is some question as to the units of the fuel efficiency.  The source claims it to be in miles per gallon, but the numbers reported seem unrealistic.  However, the units do not appear to be in km/gallon or km/L.
}
\source{
Data provided by the U.S. Environmental Protection Agency and obtained from the Data and Story library \url{http://lib.stat.cmu.edu/DASL/Stories/carmpg.html}
}
\references{
R.M. Heavenrich, J.D. Murrell, and K.H. Hellman, Light Duty Automotive
Technology and Fuel Economy Trends Through 1991, U.S. Environmental Protection
Agency, 1991 (EPA/AA/CTAB/91-02)}

\keyword{datasets}
