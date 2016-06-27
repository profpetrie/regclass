\name{SURVEY10}
\alias{SURVEY10}
\docType{data}
\title{Student survey 2010}
\description{
Characteristics of students in an introductory statistics class at the University of Tennessee in 2010
}
\usage{data("SURVEY10")}
\format{
  A data frame with 699 observations on the following 20 variables.
  \describe{
    \item{\code{Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{Height}}{a numeric vector}
    \item{\code{Weight}}{a numeric vector}
    \item{\code{DesiredWeight}}{a numeric vector}
    \item{\code{GPA}}{a numeric vector}
    \item{\code{TxtPerDay}}{a numeric vector}
    \item{\code{MinPerDayFaceBook}}{a numeric vector}
    \item{\code{NumTattoos}}{a numeric vector}
    \item{\code{NumBodyPiercings}}{a numeric vector}
    \item{\code{Handedness}}{a factor with levels \code{Ambidextrous} \code{Left} \code{Right}}
    \item{\code{WeeklyHrsVideoGame}}{a numeric vector}
    \item{\code{DistanceMovedToSchool}}{a numeric vector}
    \item{\code{PercentDateable}}{a numeric vector}
    \item{\code{NumPhoneContacts}}{a numeric vector}
    \item{\code{PercMoreAttractiveThan}}{a numeric vector}
    \item{\code{PercMoreIntelligentThan}}{a numeric vector}
    \item{\code{PercMoreAthleticThan}}{a numeric vector}
    \item{\code{PercFunnierThan}}{a numeric vector}
    \item{\code{SigificantOther}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{OwnAttractiveness}}{a numeric vector}
  }
}
\details{
Students answered 50 questions to generate data for a project in an introductory statistics class at the University of Tennessee in the Fall of 2010.  The data here represent a selection of the questions.  The responses have been somewhat cleaned (unlike \code{SURVEY09}) where obviously bogus responses have been omitted, but there may still be issue.

The selected questions were:

\code{Gender} Gender [Male, Female]
\code{Height} Your height (in inches) [48 to 96]
\code{Weight} Your weight (in pounds) [0 to 500]
\code{DesiredWeight} What is your desired weight (in pounds)? [0 to 1000]
\code{GPA} Thus far, what is your GPA (look up on CPO if you need to)? [0 to 4]
\code{TxtPerDay} How many text messages do you typically send on any given day? Type 0 if you
never send text messages. [0 to 1000]
\code{MinPerDayFaceBook} On average, how many minutes per day do you spend on internet social networks
(such as Facebook, MySpace, Twitter, LinkedIn, etc.)? [0 to 1440]
\code{NumTattoos} How many tattoos do you have? [0 to 100]
\code{NumBodyPiercings} How many body piercings do you have (do not include piercings you have let heal
up and are gone)? Count each piercing separately (i.e., pierced ears counts as 2
piercings). [0 to 100]
\code{Handedness} Are you right-handed, left-handed, or ambidextrous? [Right-Handed, Left-
Handed, Ambidextrous]
\code{WeeklyHrsVideoGame} About how many hours a week do you play video games? This includes console games like Wii, Playstation, Xbox, as well as gaming apps for your phone, online games in Facebook, general computer games, etc. [0 to 168]
\code{DistanceMovedToSchool} Go to maps.google.com or another website that provides maps. Get directions from your home address (the house/apartment/etc. you most recently lived in before coming to college) and the zip code 37996. How many miles does it say the trip is? Type the smallest number if offered multiple routes. Type 0 if you are unable to get driving directions for any reason. [0 to 5000]
\code{PercentDateable} What percentage of people around your age in your preferred gender do you
consider dateable? [0 to 100]
\code{NumPhoneContacts} How many contacts do you have in your cell phone? Answer 0 if you don't use a
cell phone, or have no contacts in your cell phone. [0 to 1000]
\code{PercMoreAttractiveThan} What percentage of people at UT of your own gender and class level do you think you are more attractive than? [0 to 100]
\code{PercMoreIntelligentThan} What percentage of people at UT of your own gender and class level do you think you are more intelligent than? [0 to 100]
\code{PercMoreAthleticThan} What percentage of people at UT of your own gender and class level do you think you are more athletic than? [0 to 100]
 \code{PercFunnierThan} What percentage of people at UT of your own gender and class level do you think you are funnier than? [0 to 100]
\code{SigificantOther}  Do you have a significant other? [Yes, No]
\code{OwnAttractiveness} On a scale of 1-100, with 100 being the most attractive, rate your own
attractiveness. [1 to 100]
}
\keyword{datasets}
