\name{STUDENT}
\alias{STUDENT}
\docType{data}
\title{
STUDENT data
}
\description{
Data on the College GPAs of students in an introductory statistics class
}
\usage{data("STUDENT")}
\format{
  A data frame with 607 observations on the following 19 variables.
  \describe{
    \item{\code{CollegeGPA}}{a numeric vector}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{HSGPA}}{a numeric vector, can range up to 5 if the high school allowed it}
    \item{\code{ACT}}{a numeric vector, ACT score}
    \item{\code{APHours}}{a numeric vector, number of AP hours student took in HS}
    \item{\code{JobHours}}{a numeric vector, number of hours student currently works on average}
    \item{\code{School}}{a factor with levels \code{Private} \code{Public}, type of HS}
    \item{\code{Languages}}{a numeric vector}
    \item{\code{Honors}}{a numeric vector, number of honors classes taken in HS}
    \item{\code{Smoker}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{AffordCollege}}{a factor with levels \code{No} \code{Yes}, can the student and his/her family pay for the University of Tennessee without taking out loans?}
    \item{\code{HSClubs}}{a numeric vector, number of clubs belonged to in HS}
    \item{\code{HSJob}}{a factor with levels \code{No} \code{Yes}, whether the student maintained a job at some point while in HS }
    \item{\code{Churchgoer}}{a factor with levels \code{No} \code{Yes}, answer to the question Do you regularly attend chruch?}
    \item{\code{Height}}{a numeric vector (inches)}
    \item{\code{Weight}}{a numeric vector (lbs)}
    \item{\code{Class}}{a factor with levels \code{Junior} \code{Senior} \code{Sophomore}}
    \item{\code{Family}}{what position they are in the family, a factor with levels \code{Middle Child} \code{Oldest Child} \code{Only Child} \code{Youngest Child}}
    \item{\code{Pet}}{favorite pet, a factor with levels \code{Both} \code{Cat} \code{Dog} \code{Neither}}
  }
}
\details{
Same data as \code{EDUCATION} with the addition of the \code{Class} variable and with slighly different names for variables.
}
\source{
Responses are from students in an introductory statistics class at the University of Tennessee in 2010.
}
\keyword{datasets}
