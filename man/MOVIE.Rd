\name{MOVIE}
\alias{MOVIE}
\docType{data}
\title{
Movie grosses
}
\description{
Movie grosses from the late 1990s
}
\usage{data("MOVIE")}
\format{
  A data frame with 309 observations on the following 3 variables.
  \describe{
    \item{\code{Movie}}{a factor giving the name of the movie }
    \item{\code{Weekend}}{a numeric vector, the opening weekend gross (millions of dollars)}
    \item{\code{Total}}{a numeric vector, the total US gross (millions of dollars)}
  }
}
\details{
The goal is to predict the total gross of a movie based on its opening weekend gross.  
}
\source{
\url{imdb.com}
}
\keyword{datasets}
