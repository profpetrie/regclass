\name{EX2.CENSUS}
\alias{EX2.CENSUS}
\docType{data}
\title{
CENSUS data for Exercise 5 in Chapter 2 
}
\description{
CENSUS data for Exercise 5 in Chapter 2 
}
\usage{data("EX2.CENSUS")}
\format{
  A data frame with 3534 observations on the following 41 variables.
  \describe{
    \item{\code{ResponseRate}}{a numeric vector}
    \item{\code{Area}}{a numeric vector}
    \item{\code{Urban}}{a numeric vector}
    \item{\code{Suburban}}{a numeric vector}
    \item{\code{Rural}}{a numeric vector}
    \item{\code{Male}}{a numeric vector}
    \item{\code{Female}}{a numeric vector}
    \item{\code{AgeLess5}}{a numeric vector}
    \item{\code{Age5to17}}{a numeric vector}
    \item{\code{Age18to24}}{a numeric vector}
    \item{\code{Age25to44}}{a numeric vector}
    \item{\code{Age45to64}}{a numeric vector}
    \item{\code{Age65plus}}{a numeric vector}
    \item{\code{Hispanics}}{a numeric vector}
    \item{\code{Whites}}{a numeric vector}
    \item{\code{Blacks}}{a numeric vector}
    \item{\code{NativeAmericans}}{a numeric vector}
    \item{\code{Asians}}{a numeric vector}
    \item{\code{Hawaiians}}{a numeric vector}
    \item{\code{Other}}{a numeric vector}
    \item{\code{RelatedHH}}{a numeric vector}
    \item{\code{MarriedHH}}{a numeric vector}
    \item{\code{NoSpouseHH}}{a numeric vector}
    \item{\code{FemaleHH}}{a numeric vector}
    \item{\code{AloneHH}}{a numeric vector}
    \item{\code{WithKidHH}}{a numeric vector}
    \item{\code{MedianHHIncomeBlock}}{a numeric vector}
    \item{\code{MedianHHIncomeCity}}{a numeric vector}
    \item{\code{OccupiedUnits}}{a numeric vector}
    \item{\code{VacantUnits}}{a numeric vector}
    \item{\code{RentingHH}}{a numeric vector}
    \item{\code{HomeownerHH}}{a numeric vector}
    \item{\code{MobileHomeUnits}}{a numeric vector}
    \item{\code{CrowdedUnits}}{a numeric vector}
    \item{\code{NoPhoneUnits}}{a numeric vector}
    \item{\code{NoPlumbingUnits}}{a numeric vector}
    \item{\code{NewUnits}}{a numeric vector}
    \item{\code{Population}}{a numeric vector}
    \item{\code{NumHH}}{a numeric vector}
    \item{\code{NumUnits}}{a numeric vector}
    \item{\code{logMedianHouseValue}}{a numeric vector}
  }
}
\details{
See \code{CENSUS} for variable descriptions (this data is nearly identical).  The goal is to predict \code{ResponseRate} from the other predictors.  \code{ResponseRate} is the percentage of households in a block group that mailed in the census forms.  A block group is on average about 40 blocks, each typically bounded by streets, roads, or water. The number of block groups per county in the US is typically between about 5 and 165 with a median of about 20.
}
\keyword{datasets}
