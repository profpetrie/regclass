\name{BULLDOZER2}
\alias{BULLDOZER2}
\docType{data}
\title{
Modified BULLDOZER data
}
\description{
The BULLDOZER dataset but with the year the dozer was made as a categorical variable
}
\usage{data("BULLDOZER2")}
\format{
  A data frame with 924 observations on the following 6 variables.
  \describe{
    \item{\code{Price}}{a numeric vector}
    \item{\code{YearsAgo}}{a numeric vector}
    \item{\code{Usage}}{a numeric vector}
    \item{\code{Tire}}{a numeric vector}
    \item{\code{Decade}}{a factor with levels \code{1960s and 1970s} \code{1980s} \code{1990s} \code{2000s}}
    \item{\code{BladeSize}}{a numeric vector}
  }
}
\details{
This is the \code{BULLDOZER} data except here \code{YearMade} has been coded into a four level categorical varaible called \code{Decade}
}

\keyword{datasets}
