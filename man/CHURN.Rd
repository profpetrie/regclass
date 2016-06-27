\name{CHURN}
\alias{CHURN}
\docType{data}
\title{
CHURN dataset }
\description{
Churn data (artificial based on claims similar to real world) from the UCI data repository

}
\usage{data("CHURN")}
\format{
  A data frame with 5000 observations on the following 18 variables.
  \describe{
    \item{\code{churn}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{accountlength}}{a numeric vector}
    \item{\code{internationalplan}}{a factor with levels \code{no} \code{yes}}
    \item{\code{voicemailplan}}{a factor with levels \code{no} \code{yes}}
    \item{\code{numbervmailmessages}}{a numeric vector}
    \item{\code{totaldayminutes}}{a numeric vector}
    \item{\code{totaldaycalls}}{a numeric vector}
    \item{\code{totaldaycharge}}{a numeric vector}
    \item{\code{totaleveminutes}}{a numeric vector}
    \item{\code{totalevecalls}}{a numeric vector}
    \item{\code{totalevecharge}}{a numeric vector}
    \item{\code{totalnightminutes}}{a numeric vector}
    \item{\code{totalnightcalls}}{a numeric vector}
    \item{\code{totalnightcharge}}{a numeric vector}
    \item{\code{totalintlminutes}}{a numeric vector}
    \item{\code{totalintlcalls}}{a numeric vector}
    \item{\code{totalintlcharge}}{a numeric vector}
    \item{\code{numbercustomerservicecalls}}{a numeric vector}
  }
}
\details{
This dataset is modified from the one stored at the UCI data repository (namely, the area code and phone number have been deleted).  This is artificial data similar to what is found in actual customer profiles.  Charges are in dollars.
}
\source{
Though originally on the UCI data repository, actual data was obtained via \url{https://www.sgi.com/tech/mlc/db/}
}

\keyword{datasets}
