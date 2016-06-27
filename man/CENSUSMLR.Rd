\name{CENSUSMLR}
\alias{CENSUSMLR}
\docType{data}
\title{
Subset of CENSUS data
}
\description{
A portion of the CENSUS dataset used for illustration
}
\usage{data("CENSUSMLR")}
\format{
  A data frame with 1000 observations on the following 7 variables.
  \describe{
    \item{\code{Response}}{a numeric vector, percentage 0-100 of household that mailed in the census form}
    \item{\code{Population}}{a numeric vector, the number of people living in the census block based on 2010 census}
    \item{\code{ACSPopulation}}{a numeric vector, the number of people living in the census block based on 2010 census}
    \item{\code{Rural}}{a numeric vector, the number of people living in a rural area (in that census block)}
    \item{\code{Males}}{a numeric vector, the number of males living in the census block}
    \item{\code{Elderly}}{a numeric vector, the number of people aged 65+ living in the census block}
    \item{\code{Hispanic}}{a numeric vector, the number of people who self-identify as Hispanic in the census block}
  }
}
\details{
See \code{CENSUS} data for more information.
}

\keyword{datasets}
