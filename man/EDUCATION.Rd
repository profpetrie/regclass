\name{EDUCATION}
\alias{EDUCATION}
\docType{data}
\title{
EDUCATION data
}
\description{
Data on the College GPAs of students in an introductory statistics class
}
\usage{data("EDUCATION")}
\format{
  A data frame with 607 observations on the following 18 variables.
  \describe{
    \item{\code{CollegeGPA}}{a numeric vector}
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{HSGPA}}{a numeric vector, can range up to 5 if the high school allowed it}
    \item{\code{ACT}}{a numeric vector, ACT score}
    \item{\code{APHours}}{a numeric vector, number of AP hours student took in HS}
    \item{\code{JobHours}}{a numeric vector, number of hours student currently works on average}
    \item{\code{School}}{a factor with levels \code{Private} \code{Public}, type of HS}
    \item{\code{LanguagesSpoken}}{a numeric vector}
    \item{\code{HSHonorsClasses}}{a numeric vector, number of honors classes taken in HS}
    \item{\code{SmokeInHS}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{PayCollegeNoLoans}}{a factor with levels \code{No} \code{Yes}, can the student and his/her family pay for the University of Tennessee without taking out loans?}
    \item{\code{ClubsInHS}}{a numeric vector, number of clubs belonged to in HS}
    \item{\code{JobInHS}}{a factor with levels \code{No} \code{Yes}, whether the student maintained a job at some point while in HS }
    \item{\code{Churchgoer}}{a factor with levels \code{No} \code{Yes}, answer to the question Do you regularly attend chruch?}
    \item{\code{Height}}{a numeric vector (inches)}
    \item{\code{Weight}}{a numeric vector (lbs)}
    \item{\code{Family}}{what position they are in the family, a factor with levels \code{Middle Child} \code{Oldest Child} \code{Only Child} \code{Youngest Child}}
    \item{\code{Pet}}{favorite pet, a factor with levels \code{Both} \code{Cat} \code{Dog} \code{Neither}}
  }
}
\details{
Responses are from students in an introductory statistics class at the University of Tennessee in 2010.  One goal to try to predict someone's college GPA from some of the students' characteristics.  What information about a high school student could a college admission's counselor use to anticipate that student's performance in college?
}
\keyword{datasets}
