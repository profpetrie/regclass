\name{ATTRACTF}
\alias{ATTRACTF}
\docType{data}
\title{
Attractiveness Score (female)
}
\description{
The average attractiveness scores of 70 females along with physical attributes
}
\usage{data("ATTRACTF")}
\format{
  A data frame with 70 observations on the following 21 variables.
  \describe{
    \item{\code{Score}}{a numeric vector giving the average attractivness score compiled after 100 student ratings}
    \item{\code{Actual.Sexuality}}{a factor with levels \code{Gay} \code{Straight} indicating the self-reported sexuality of the person in the picture}
    \item{\code{ApparentRace}}{a factor with levels \code{black} \code{other} \code{white} indicating the consensus regarding the apparent race of the person}
    \item{\code{Chin}}{a factor with levels \code{pointed} \code{rounded} indicating the consensus regarding the shape of the person's chin} 
    \item{\code{Cleavage}}{a factor with levels \code{no} \code{yes} indicating the consensus regarding whether the pictured woman was prominently displaying cleavage}
    \item{\code{ClothingStyle}}{a factor with levels \code{conservative} \code{revealing} indicating the consensus regarding how the women was dressed}
    \item{\code{FaceSymmetryScore}}{a numeric vector indicating the number of people (out of 2) who agreed the woman's case was symmetric}
    \item{\code{FashionScore}}{a numeric vector indicating the number of people (out of 4) who agreed the woman was fashionable}
    \item{\code{FitnessScore}}{a numeric vector indicating the number of people (out of 4) who agreed the woman was physically fit}
    \item{\code{GayScore}}{a numeric vector indicating the number of people (out of 16) who agreed the woman was a lesbian}
    \item{\code{Glasses}}{a factor with levels \code{Glasses} \code{No Glasses}}
    \item{\code{GroomedScore}}{a numeric vector indicating the number of people (out of 4) who agreed the woman made a noticeable effort to look nice}
    \item{\code{HairColor}}{a factor with levels \code{dark} \code{light} indicating the consensus regarding the woman's hair color}
    \item{\code{HairstyleUniquess}}{a numeric vector indicating the number of people (out of 2) who agreed the woman had an unconventional haircut}
    \item{\code{HappinessRating}}{a numeric vector indicating the number of people (out of 2) who agreed the woman looked happy in her photo}
    \item{\code{LookingAtCamera}}{a factor with levels \code{no} \code{yes}}
    \item{\code{MakeupScore}}{a numeric vector indicating the number of people (out of 5) who agreed the woman was wearing a noticeable amount of makeup}
    \item{\code{NoseOddScore}}{a numeric vector indicating the number of people (out of 3) who agreed the woman had an unusually shaped nose}
    \item{\code{Selfie}}{a factor with levels \code{no} \code{yes}}
    \item{\code{SkinClearScore}}{a numeric vector indicating the number of people (out of 2) who agreed the woman's complexion was clear.}
    \item{\code{Smile}}{a factor with levels \code{no} \code{yes}}
  }
}
\details{
Students were asked to rate on a scale of 1 (very unattractive) to 5 (very attractive) the attractiveness of 70 college-aged women who had posted their photos on a dating website.  Of the nearly 100 respondents, most were straight males.  \code{Score} represents the average of these ratings.

In a separate survey, students (of both genders) were asked to rate characteristics of the woman by answering the questions:  what is her race, is she displaying her cleavage prominently, is she a lesbian, is she physically fit, etc.  The variables ending ``Score" represent the number of students who answered Yes to the question.  Other variables (such as \code{Selfie}, \code{Smile}) represent the consensus among the students.  The only attribute taken from the woman's profile was \code{Actual.Sexuality}.


}
\source{
Students in BAS 320 at the University of Tennessee from 2013-2015.
}
\keyword{datasets}
