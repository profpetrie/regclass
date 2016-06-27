\name{CUSTLOYALTY}
\alias{CUSTLOYALTY}
\docType{data}
\title{
CUSTLOYALTY dataset }
\description{
Customer database describing customer value (adapted from a former case study) and whether they have a loyalty card

}
\usage{data("CUSTLOYALTY")}
\format{
  A data frame with 500 observations on the following 9 variables.
  \describe{
      \item{\code{Gender}}{a factor with levels \code{Female} \code{Male} giving the customer's gender}
      \item{\code{Married}}{a factor with levels \code{Married} \code{Single} giving the customer's marital status}

    \item{\code{Income}}{a factor with levels \code{f0t30}, \code{f30t45}, \code{f45t60}, \code{f60t75}, \code{f75t90}, \code{f90toINF} giving the approximate yearly income of the customer.  The first level corresponds to 30K or less, the second level corresponds to 30K to 45K, and the last level corresponds to 90K or above
    }
    \item{\code{FirstPurchase}}{a numeric vector giving the amount of the customer's first purchase amount}
    
          \item{\code{LoyaltyCard}}{a factor with levels \code{No} \code{Yes} that gives whether the customer has a loyalty card for the store}

    \item{\code{WalletShare}}{a numeric vector giving the percentage from 0 to 100 of similar products that the customer makes at this store.  A value of 100 means the customer uses this store exclusively for such purchases.}
    \item{\code{CustomerLV}}{a numeric vector giving the lifetime value of the customer and reflects the amount spent acquiring and retaining the customer along with the revenue brought in by the customer}
    \item{\code{TotTransactions}}{a numeric vector giving the total number of consecutive months the customer has made a transaction in the last year}
    \item{\code{LastTransaction}}{a numeric vector giving the total amount of months since the customers last transaction}
  }
}
\details{
Each row corresponds to a customer of a local chain.  Does having a loyalty card increase the customer's value?
}
\source{Unknown}

\keyword{datasets}
