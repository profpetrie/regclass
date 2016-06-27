\name{CUSTCHURN}
\alias{CUSTCHURN}
\docType{data}
\title{
CUSTCHURN dataset }
\description{
Customer database describing customer churn (adapted from a former case study)

}
\usage{data("CUSTCHURN")}
\format{
  A data frame with 500 observations on the following 11 variables.
  \describe{
    \item{\code{Duration}}{a numeric vector giving the days that the company was considered a customer.  Note:  censored at 730 days, which is the value for someone who is currently a customer (not churned)}
    \item{\code{Churn}}{a factor with levels \code{N} \code{Y} giving whether the customer has churned or not}
    \item{\code{RetentionCost}}{a numeric vector giving the average amount of money spent per year to retain the individual or company as a customer}
    \item{\code{EBiz}}{a factor with levels \code{No} \code{Yes} giving whether the customer was an e-business or not}
    \item{\code{CompanyRevenue}}{a numeric vector giving the company's revenue}
    \item{\code{CompanyEmployees}}{a numeric vector giving the number of employees working for the company}
    \item{\code{Categories}}{a numeric vector giving the number of product categories from which customer made a purchase of their lifetime}
    \item{\code{NumPurchases}}{a numeric vector giving the total amount of purchases over the customer's lifetime}

  }
}
\details{
Each row corresponds to a customer of a Fortune 500 company.  These customers are businesses, which may or may not exclusively be an e-business.  Whether a customer is still a customer (or has churned) after 730 days is recorded.
}
\source{Unknown}

\keyword{datasets}
