\name{POISON}
\alias{POISON}
\docType{data}
\title{
Cockroach poisoning data
}
\description{
Dosages and mortality of cockroaches
}
\usage{data("POISON")}
\format{
  A data frame with 481 observations on the following 2 variables.
  \describe{
    \item{\code{Dose}}{a numeric vector indicated the dosage of the poison administered to the cockroach}
    \item{\code{Outcome}}{a factor with levels \code{Die} \code{Live}}
  }
}
\details{
Artificial data illustrating a dose-reponse curve.  The probability of dying is well-modeled by a logistic regression model.
}
\keyword{datasets}
