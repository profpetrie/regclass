\name{CUSTVALUE}
\alias{CUSTVALUE}
\docType{data}
\title{
CUSTVALUE dataset }
\description{
Customer database describing customer value (adapted from a former case study)

}
\usage{data("CUSTVALUE")}
\format{
  A data frame with 500 observations on the following 11 variables.
  \describe{
    \item{\code{Acquired}}{a factor with levels \code{No} \code{Yes} indicating whether a potential customer was acquired }
    \item{\code{Duration}}{a numeric vector giving the days that the company was considered a customer}
    \item{\code{LifetimeValue}}{a numeric vector giving the lifetime value of the customer (related to the amount of money spent on acquisition and the revenue brought in by the customer; can be negative)}
    \item{\code{AcquisitionCost}}{a numeric vector giving the amount of money spent attempting to acquire as a customer}
    \item{\code{RetentionCost}}{a numeric vector giving the average amount of money spent per year to retain the individual or company as a customer}
    \item{\code{NumPurchases}}{a numeric vector giving the total amount of purchases over the customer's lifetime}
    \item{\code{Categories}}{a numeric vector giving the number of product categories from which customer made a purchase of their lifetime}
    \item{\code{WalletShare}}{a numeric vector giving the percentage of purchases of similar products the customer makes with this company; a few values exceed 100 for some reason}

    \item{\code{EBiz}}{a factor with levels \code{No} \code{Yes} giving whether the customer was an e-business or not}

    \item{\code{CompanyRevenue}}{a numeric vector giving the company's revenue}
    \item{\code{CompanyEmployees}}{a numeric vector giving the number of employees working for the company}

  }
}
\details{
Each row corresponds to a (potential) customer of a Fortune 500 company.  These customers are businesses, which may or may not exclusively an e-business.
}
\source{Unknown}

\keyword{datasets}
