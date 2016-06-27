\name{DONOR}
\alias{DONOR}
\docType{data}
\title{
DONOR dataset
}
\description{
Adapted from the KDD-CUP-98 data set concerning data regarding donations made to a national veterans organization.
}
\usage{data("DONOR")}
\format{
  A data frame with 19372 observations on the following 50 variables.
  \describe{
    \item{\code{Donate}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{Donation.Amount}}{a numeric vector}
    \item{\code{ID}}{a numeric vector}
    \item{\code{MONTHS_SINCE_ORIGIN}}{a numeric vector, number of months donor has been in the database}
    \item{\code{DONOR_AGE}}{a numeric vector}
    \item{\code{IN_HOUSE}}{a numeric vector, 1 if person has donated to the charity's ``In House" program}
    \item{\code{URBANICITY}}{a factor with levels \code{?} \code{C} \code{R} \code{S} \code{T} \code{U}}
    \item{\code{SES}}{a factor with levels \code{?} \code{1} \code{2} \code{3} \code{4}, one of five possible codes indicating socioeconomic status}
    \item{\code{CLUSTER_CODE}}{a factor with levels \code{ .} \code{01} \code{02}  ... \code{53}, one of 54 possible cluster codes, which are
unique in terms of socioeconomic status,
urbanicity, ethnicity, and other demographic
characteristics}
    \item{\code{HOME_OWNER}}{a factor with levels \code{H} \code{U}}
    \item{\code{DONOR_GENDER}}{a factor with levels \code{A} \code{F} \code{M} \code{U}}
    \item{\code{INCOME_GROUP}}{a numeric vector, but in reality one of 7 possible income groups inferred from demographics}
    \item{\code{PUBLISHED_PHONE}}{a numeric vector, listed (1) vs not listed (0)}
    \item{\code{OVERLAY_SOURCE}}{a factor with levels \code{B} \code{M} \code{N} \code{P}, source from which the donor was match; B is both sources and N is neither}
    \item{\code{MOR_HIT_RATE}}{a numeric vector, number of known times donor has responded to a mailed solicitation from a group other than the charity}
    \item{\code{WEALTH_RATING}}{a numeric vector, but in reality one of 10 groups based on demographics}
    \item{\code{MEDIAN_HOME_VALUE}}{a numeric vector, inferred from other variables}
    \item{\code{MEDIAN_HOUSEHOLD_INCOME}}{a numeric vector, inferred from other variables}
    \item{\code{PCT_OWNER_OCCUPIED}}{a numeric vector, percent of owner-occupied housing near where person lives}
    \item{\code{PER_CAPITA_INCOME}}{a numeric vector, of neighborhood in which person lives}
    \item{\code{PCT_ATTRIBUTE1}}{a numeric vector, percent of residents in person's neighborhood that are male and active military}
    \item{\code{PCT_ATTRIBUTE2}}{a numeric vector, percent of residents in person's neighborhood that are male and veterans }
    \item{\code{PCT_ATTRIBUTE3}}{a numeric vector, percent of residents in person's neighborhood that are Vietnam veterans}
    \item{\code{PCT_ATTRIBUTE4}}{a numeric vector, percent of residents in person's neighborhood that are WW2 veterans}
    \item{\code{PEP_STAR}}{a numeric vector, 1 if has achieved STAR donor status and 0 otherwise}
    \item{\code{RECENT_STAR_STATUS}}{a numeric vector, 1 if achieved STAR within last 4 years}
    \item{\code{RECENCY_STATUS_96NK}}{a factor with levels \code{A} (active) \code{E} (inactive) \code{F} (first time) \code{L} (lapsing)\code{N} (new) \code{S} (star donor) as of 1996.}
    \item{\code{FREQUENCY_STATUS_97NK}}{a numeric vector indicating number of times donated in last period (but period is determined by RECENCY STATUS 96NK)}
    \item{\code{RECENT_RESPONSE_PROP}}{a numeric vector, proportion of responses to the individual to the
number of (card or other) solicitations from the
charitable organization since four years ago }
    \item{\code{RECENT_AVG_GIFT_AMT}}{a numeric vector, average donation from the individual to the charitable organization since four years ago}
    \item{\code{RECENT_CARD_RESPONSE_PROP}}{a numeric vector, number of times the individual has responded to
a card solicitation from the charitable
organization since four years ago }
    \item{\code{RECENT_AVG_CARD_GIFT_AMT}}{a numeric vector, average donation from the individual in response
to a card solicitation from the charitable
organization since four years ago }
    \item{\code{RECENT_RESPONSE_COUNT}}{a numeric vector, number of times the individual has responded to
a promotion (card or other) from the charitable
organization since four years ago }
    \item{\code{RECENT_CARD_RESPONSE_COUNT}}{a numeric vector, number of times the individual has responded to
a card solicitation from the charitable
organization since four years ago }
    \item{\code{MONTHS_SINCE_LAST_PROM_RESP}}{a numeric vector, number of months since the individual has
responded to a promotion by the charitable
organization }
    \item{\code{LIFETIME_CARD_PROM}}{a numeric vector, total number of card promotions sent to the
individual by the charitable organization }
    \item{\code{LIFETIME_PROM}}{a numeric vector, total number of promotions sent to the individual
by the charitable organization }
    \item{\code{LIFETIME_GIFT_AMOUNT}}{a numeric vector, total lifetime donation amount from the
individual to the charitable organization }
    \item{\code{LIFETIME_GIFT_COUNT}}{a numeric vector, total number of donations from the individual to
the charitable organization }
    \item{\code{LIFETIME_AVG_GIFT_AMT}}{a numeric vector, lifetime average donation from the
individual to the charitable organization}
    \item{\code{LIFETIME_GIFT_RANGE}}{a numeric vector, difference between maximum and minimum donation amounts from the individual}
    \item{\code{LIFETIME_MAX_GIFT_AMT}}{a numeric vector}
    \item{\code{LIFETIME_MIN_GIFT_AMT}}{a numeric vector}
    \item{\code{LAST_GIFT_AMT}}{a numeric vector}
    \item{\code{CARD_PROM_12}}{a numeric vector, number of card promotions sent to the individual
by the charitable organization in the last 12
months }
    \item{\code{NUMBER_PROM_12}}{a numeric vector, number of promotions (card or other) sent to the
individual by the charitable organization in the
last 12 months }
    \item{\code{MONTHS_SINCE_LAST_GIFT}}{a numeric vector}
    \item{\code{MONTHS_SINCE_FIRST_GIFT}}{a numeric vector}
    \item{\code{FILE_AVG_GIFT}}{a numeric vector, same as \code{LIFETIME_AVG_GIFT_AMT} }
    \item{\code{FILE_CARD_GIFT}}{a numeric vector, lifetime average donation from the
individual in response to all card solicitations
from the charitable organization }
  }
}
\details{
Originally, this data was used with the 1998 KDD competition (\url{https://kdd.ics.uci.edu/databases/kddcup98/kddcup98.html}).  This particular version has been adapted from the version available in SAS Enterprise Miner (\url{http://support.sas.com/documentation/cdl/en/emgsj/61207/PDF/default/emgsj.pdf} Appendix 2 for descriptions of variable names).  One goal is to determine whether a past donor donated in response to the 97NK mail solicitation and (if so), how much, based on  age, gender, most recent donation amount, total gift amount, etc.
}
\keyword{datasets}
