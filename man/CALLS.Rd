\name{CALLS}
\alias{CALLS}
\docType{data}
\title{
CALLS dataset
}
\description{
Summary of students' cell phone providers and relative frequency of dropped calls
}
\usage{data("CALLS")}
\format{
  A data frame with 579 observations on the following 2 variables.
  \describe{
    \item{\code{Provider}}{a factor with levels \code{ATT} \code{Sprint} \code{USCellular} \code{Verizon}}
    \item{\code{DropCallFreq}}{a factor with levels \code{Occasionally} \code{Often} \code{Rarely}}
  }
}
\details{
Data is self-reported by students.  The dropped call frequency is based on individuals' perceptions and not any independent quantititatve measure.  The data is a subset of \code{SURVEY09}.
}
\source{
Student survey from STAT 201, University of Tennessee Knoxville, Fall 2009
}
\keyword{datasets}
