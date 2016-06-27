\name{ACCOUNT}
\alias{ACCOUNT}
\docType{data}
\title{
Predicting whether a customer will open a new kind of account
}
\description{
Customers were marketed a new type of account at a bank.  It is desired to model what factors seemed to be associated with the probability of opening the account to tune marketing strategy.
}
\usage{data("ACCOUNT")}
\format{
  A data frame with 24242 observations on the following 8 variables.
  \describe{
    \item{\code{Purchase}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{Tenure}}{a numeric vector, the number of years the customer has been with the bank}
    \item{\code{CheckingBalance}}{a numeric vector, amount currently held in checking (may be negative if overdrafted)}
    \item{\code{SavingBalance}}{a numeric vector, amount currently held in savings (0 or larger) }
    \item{\code{Income}}{a numeric vector, yearly income in thousands of dollars}
    \item{\code{Homeowner}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Area.Classification}}{a factor with levels \code{R} \code{S} \code{U} for rural, suburban, or urban}
  }
}
\details{
Who is more likely to open a new type of account that a bank wants to try to sell its customers?  Try logistic regression or partition models to see if you can develop a model that accurately classifies purchasers vs. non-purchasers.  Or, try to develop a model that does well in promoting to nearly all customers who would buy the account.
}

\keyword{datasets}
