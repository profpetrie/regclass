\name{PIMA}
\alias{PIMA}
\docType{data}
\title{
Pima Diabetes dataset}
\description{
Diabetes among women aged 21+ with Pima heritage
}
\usage{data("PIMA")}
\format{
  A data frame with 392 observations on the following 8 variables.
  \describe{
    \item{\code{Pregnant}}{a numeric vector, number of times the woman has been pregnant}
    \item{\code{Glucose}}{a numeric vector, plasma glucose concentration }
    \item{\code{BloodPressure}}{a numeric vector, diastolic blood pressure in mm Hg}
    \item{\code{BodyFat}}{a numeric vector, a measurement of the triceps skinfold thickness which is an indicator of body fat percentage}
    \item{\code{Insulin}}{a numeric vector, 2-hour serum insulin}
    \item{\code{BMI}}{a numeric vector, body mass index}
    \item{\code{Age}}{a numeric vector, years}
    \item{\code{Diabetes}}{a factor with levels \code{No} \code{Yes}}
  }
}
\details{
Data on 768 women belonging to the Pima tribe.  The purpose is to study the associations between having diabetes and various physiological characteristics.  Although there are surely other factors (including genetic) that influence the chance of having diabetes, the hope is that by having women who are genetically similar (all from the Pima tribe), that these other factors are naturally accounted for.

}
\source{ Adapted from the UCI data repository \url{https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes}.  A variable measuring the ``diabetes pedigree function" has been omitted.}
\keyword{datasets}
