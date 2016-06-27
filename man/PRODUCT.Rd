\name{PRODUCT}
\alias{PRODUCT}
\docType{data}
\title{
Sales of a product one quarter after release
}
\description{
Sales of a product two quarters after release
}
\usage{data("PRODUCT")}
\format{
  A data frame with 2768 observations on the following 4 variables.
  \describe{
    \item{\code{Outcome}}{a factor with levels \code{fail} \code{success} indicating whether the product was deemed a success or failure }
    \item{\code{Category}}{a factor with levels \code{A} \code{B} \code{C} \code{D}, the type of item (e.g., kitchen, toys, consumables) }
    \item{\code{Trend}}{a factor with levels \code{down} \code{up}, indicating whether the sales over the first 13 weeks had an upward trend or downward trend according to a simple linear regression}
    \item{\code{SoldWeek13}}{a numeric vector, the number of items sold 13 weeks after release}
  }
}
\details{
Inspired by the dunnhumby hackathon hosted at \url{https://www.kaggle.com/c/hack-reduce-dunnhumby-hackathon}.  The goal is to predict whether a product will be a success or failure half a year after its release based on its characteristics and performance during the first quarter after its release.
}
\source{
Adapted from \url{https://www.kaggle.com/c/hack-reduce-dunnhumby-hackathon}
}
\keyword{datasets}
