\name{CENSUS}
\alias{CENSUS}
\docType{data}
\title{
CENSUS data
}
\description{
Information from the 2010 US Census
}
\usage{data("CENSUS")}
\format{
  A data frame with 3534 observations on the following 39 variables.
  \describe{
    \item{\code{ResponseRate}}{a numeric vector, 0-100 representing the percentage of households in a block group that mailed in the form}
    \item{\code{Area}}{a numeric vector, land area in square miles}
    \item{\code{Urban}}{a numeric vector, percentage of block group in Urbanized area (50000 or greater)}
    \item{\code{Suburban}}{a numeric vector, percentage of block group in an Urban Cluster area (2500 to 49999)}
    \item{\code{Rural}}{a numeric vector, percentage of block group in an Urban Cluster area (2500 to 49999)}
    \item{\code{Male}}{a numeric vector, percentage of males}
    \item{\code{AgeLess5}}{a numeric vector, percentage of individuals aged less than 5 years old}
    \item{\code{Age5to17}}{a numeric vector}
    \item{\code{Age18to24}}{a numeric vector}
    \item{\code{Age25to44}}{a numeric vector}
    \item{\code{Age45to64}}{a numeric vector}
    \item{\code{Age65plus}}{a numeric vector}
    \item{\code{Hispanics}}{a numeric vector, percentage of individuals who identify as Hispanic}
    \item{\code{Whites}}{a numeric vector, percentage of individuals who identify as white (alone)}
    \item{\code{Blacks}}{a numeric vector}
    \item{\code{NativeAmericans}}{a numeric vector}
    \item{\code{Asians}}{a numeric vector}
    \item{\code{Hawaiians}}{a numeric vector}
    \item{\code{Other}}{a numeric vector, percentage of individuals who identify as another ethnicity}
    \item{\code{RelatedHH}}{a numeric vector, percentage of households where
at least 2 members are related by birth,
marriage, or adoption; same-sex couple
households with no relatives of the
householder present are not included}
    \item{\code{MarriedHH}}{a numeric vector, percentage of  households in
which the householder and his or her
spouse are listed as members of the same
household; does not include same-sex
married couples}
    \item{\code{NoSpouseHH}}{a numeric vector, percentage of  households with
no spousal relationship present}
    \item{\code{FemaleHH}}{a numeric vector, percentage of  households with a
female householder and no husband of
householder present}
    \item{\code{AloneHH}}{a numeric vector, percentage of  households where householder is living alone}

    \item{\code{WithKidHH}}{a numeric vector, percentage of  households which have at least one person under the age of 18}


    \item{\code{MedianHHIncomeBlock}}{a numeric vector, median income of households in the block group (from American Community Survey)}
    \item{\code{MedianHHIncomeCity}}{a numeric vector, median income of households in the tract}
    \item{\code{OccupiedUnits}}{a numeric vector, percentage of housing units that are occupied}
    \item{\code{RentingHH}}{a numeric vector, percentage of housing units occupied by renters}
    \item{\code{HomeownerHH}}{a numeric vector, percentage of housing units occupied by the owner}
    \item{\code{MobileHomeUnits}}{a numeric vector, percentage of housing units that are mobile homes (from American Community Survey)}
    \item{\code{CrowdedUnits}}{a numeric vector, percentage of housing units with more than 1 person per room on average}
    \item{\code{NoPhoneUnits}}{a numeric vector, percentage of housing units without a landline}
    \item{\code{NoPlumbingUnits}}{a numeric vector, percentage of housing units without active plumbing}
    \item{\code{NewUnits}}{a numeric vector, percentage of housing units constructed in 2010 or later}
    \item{\code{Population}}{a numeric vector, number of people in the block group}
    \item{\code{NumHH}}{a numeric vector, number of households in the block group}
    \item{\code{NumUnits}}{a numeric vector, number of housing units in the block group}
    \item{\code{logMedianHouseValue}}{a numeric vector, the logarithm of the median home value in the block group}
  }
}
\details{
The goal is to predict \code{ResponseRate} from the other predictors.  \code{ResponseRate} is the percentage of households in a block group that mailed in the census forms.  A block group is on average about 40 blocks, each typically bounded by streets, roads, or water. The number of block groups per county in the US is typically between about 5 and 165 with a median of about 20.
}

\references{
See \url{http://www.census.gov/research/data/planning_database/2014/docs/PDB_Block_2014-11-20a.pdf} for variable definitions.
}

\keyword{datasets}
