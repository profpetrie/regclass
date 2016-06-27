\name{SALARY}
\alias{SALARY}
\docType{data}
\title{
Harris Bank Salary data
}
\description{
Harris Bank Salary data
}
\usage{data("SALARY")}
\format{
  A data frame with 93 observations on the following 5 variables.
  \describe{
    \item{\code{Salary}}{a numeric vector, starting monthly salary in dollars}
    \item{\code{Education}}{a numeric vector, years of schooling at the time of hire}
    \item{\code{Experience}}{a numeric vector, number of years of previous work experience}
    \item{\code{Months}}{a numeric vector, number of months after Jan 1 1969 that the individual was hired}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
  }
}
\details{
Real data used in a court lawsuit.  93 randomly selected employees of Harris Bank Chicago from 1977.  Values in this data have been scaled from the original values (e.g., \code{Experience} in years instead of months, \code{Education} starts at 0 instead of 8, etc.)
}
\source{
Adapted from the case study at \url{http://www.stat.ualberta.ca/statslabs/casestudies/sexdiscrimination.htm}
}
\keyword{datasets}
