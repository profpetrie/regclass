\name{ATTRACTM}
\alias{ATTRACTM}
\docType{data}
\title{
Attractiveness Score (male)
}
\description{
The average attractiveness scores of 70 males along with physical attributes
}
\usage{data("ATTRACTM")}
\format{
  A data frame with 70 observations on the following 23 variables.
  \describe{
    \item{\code{Score}}{a numeric vector giving the average attractivness score compiled after 60 student ratings}
    \item{\code{Actual.Sexuality}}{a factor with levels \code{Gay} \code{Straight}  indicating the self-reported sexuality of the person in the picture}
    \item{\code{ApparentRace}}{a factor with levels \code{black} \code{other} \code{white} indicating the consensus regarding the apparent race of the person}
    \item{\code{Chin}}{a factor with levels \code{pointed} \code{rounded} indicating the consensus regarding the shape of the person's chin}
    \item{\code{ClothingStyle}}{a factor with levels \code{conservative} \code{revealing} indicating the consensus regarding how the man was dressed}
    \item{\code{FaceSymmetryScore}}{a numeric vector indicating the number of people (out of 7) who agreed the woman's case was symmetric}
    \item{\code{FacialHair}}{a factor with levels \code{no} \code{yes} indicating the consensus regarding whether the man appeared to maintain facial hair}
    \item{\code{FashionScore}}{a numeric vector indicating the number of people (out of 7) who agreed the woman was fashionable}
    \item{\code{FitnessScore}}{a numeric vector indicating the number of people (out of 8) who agreed the woman was physically fit}
    \item{\code{GayScore}}{a numeric vector indicating the number of people (out of 16) who agreed the man was gay}
    \item{\code{Glasses}}{a factor with levels \code{no} \code{yes}}
    \item{\code{GroomedScore}}{a numeric vector indicating the number of people (out of 6) who agreed the woman made a noticeable effort to look nice}
    \item{\code{HairColor}}{a factor with levels \code{dark} \code{light} \code{unseen} indicating the consensus regarding the man's hair color}
    \item{\code{HairstyleUniquess}}{a numeric vector indicating the number of people (out of 4) who agreed the woman had an unconventional haircut}
    \item{\code{HappinessRating}}{a numeric vector indicating the number of people (out of 6) who agreed the man looked happy in her photo}
    \item{\code{Hat}}{a factor with levels \code{no} \code{yes}}
    \item{\code{LookingAtCamera}}{a factor with levels \code{no} \code{yes}}
    \item{\code{NoseOddScore}}{a numeric vector indicating the number of people (out of 3) who agreed the woman had an unusually shaped nose}
    \item{\code{Piercings}}{a factor with levels \code{no} \code{yes} indicating whether the man had visible piercings}
    \item{\code{Selfie}}{a factor with levels \code{no} \code{yes}}
    \item{\code{SkinClearScore}}{a numeric vector indicating the number of people (out of 2) who agreed the woman's complexion was clear.}
    \item{\code{Smile}}{a factor with levels \code{no} \code{yes}}
    \item{\code{Tattoo}}{a factor with levels \code{no} \code{yes}}
  }
}
\details{
Students were asked to rate on a scale of 1 (very unattractive) to 5 (very attractive) the attractiveness of 70 college-aged men who had posted their photos on a dating website.  Of the nearly 60 respondents, most were straight females.  \code{Score} represents the average of these ratings.

In a separate survey, students (of both genders) were asked to rate characteristics of the man by answering the questions:  what is his race, how symmetric does his face look, is he gay, is he physically fit, etc.  The variables ending ``Score" represent the number of students who answered Yes to the question.  Other variables (such as \code{Hat}, \code{Smile}) represent the consensus among the students.  The only attribute taken from the man's profile was \code{Actual.Sexuality}.
}
\source{
Students in BAS 320 at the University of Tennessee from 2013-2015.
}

\keyword{datasets}
