\name{SOLD26}
\alias{SOLD26}
\docType{data}
\title{
Predicting future sales
}
\description{
Predicting future sales based on sales data in first quarter after release
}
\usage{data("SOLD26")}
\format{
  A data frame with 2768 observations on the following 16 variables.
  \describe{
    \item{\code{SoldWeek26}}{a numeric vector, the number of items sold 26 weeks after release and the quantity to predict}
    \item{\code{StoresSelling1}}{a numeric vector, the number of stores selling the item 1 week after release}
    \item{\code{StoresSelling3}}{a numeric vector}
    \item{\code{StoresSelling5}}{a numeric vector}
    \item{\code{StoresSelling7}}{a numeric vector}
    \item{\code{StoresSelling9}}{a numeric vector}
    \item{\code{StoresSelling11}}{a numeric vector}
    \item{\code{StoresSelling13}}{a numeric vector}
    \item{\code{StoresSelling26}}{a numeric vector, the planned number of stores selling the item 26 weeks after release}
    \item{\code{Sold1}}{a numeric vector, the number of items sold 1 week after release}
    \item{\code{Sold3}}{a numeric vector}
    \item{\code{Sold5}}{a numeric vector}
    \item{\code{Sold7}}{a numeric vector}
    \item{\code{Sold9}}{a numeric vector}
    \item{\code{Sold11}}{a numeric vector}
    \item{\code{Sold13}}{a numeric vector, the number of items sold 13 weeks after release}
  }
}
\details{
Inspired by the dunnhumby hackathon hosted at \url{https://www.kaggle.com/c/hack-reduce-dunnhumby-hackathon}.  The goal is to predict the number of items sold 26 weeks after released based on the characteristics of its sales during the first 13 weeks after release (along with information about how many stores are planning to sell the product 26 weeks after release). 
}
\source{
Adapted from \url{https://www.kaggle.com/c/hack-reduce-dunnhumby-hackathon}
}
\keyword{datasets}
