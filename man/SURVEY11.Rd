\name{SURVEY11}
\alias{SURVEY11}
\docType{data}
\title{
Student survey 2011
}
\description{
Characteristics of students in an introductory statistics class at the University of Tennessee in 2011
}
\usage{data("SURVEY11")}
\format{
  A data frame with 628 observations on the following 51 variables.
  \describe{
    \item{\code{X01.ID}}{a numeric vector}
    \item{\code{X02.Gender}}{a factor with levels \code{F} \code{M}}
    \item{\code{X03.Height}}{a numeric vector}
    \item{\code{X04.Weight}}{a numeric vector}
    \item{\code{X05.SatisfiedWithWeight}}{a factor with levels \code{No I Wish I Weighed Less} \code{No I Wish I Weighed More} \code{Yes}}
    \item{\code{X06.Class}}{a factor with levels \code{Freshman} \code{Junior} \code{Senior} \code{Sophomore}}
    \item{\code{X07.GPA}}{a numeric vector}
    \item{\code{X08.Greek}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X09.PoliticalBeliefs}}{a factor with levels \code{Conservative} \code{Liberal} \code{Mix}}
    \item{\code{X10.BornInTN}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X11.HairColor}}{a factor with levels \code{Black} \code{Blonde} \code{Brown} \code{Red}}
    \item{\code{X12.GrowUpInUS}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X13.NumberHousemates}}{a numeric vector}
    \item{\code{X14.FacebookFriends}}{a numeric vector}
    \item{\code{X15.NumPeopleTalkToOnPhone}}{a numeric vector}
    \item{\code{X16.MinutesTalkOnPhone}}{a numeric vector}
    \item{\code{X17.PeopleSendTextsTo}}{a numeric vector}
    \item{\code{X18.NumSentTexts}}{a numeric vector}
    \item{\code{X19.Computer}}{a factor with levels \code{Mac} \code{PC}}
    \item{\code{X20.Churchgoer}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X21.HoursAtJob}}{a numeric vector}
    \item{\code{X22.FastestCarSpeed}}{a numeric vector}
    \item{\code{X23.NumTimesBrushTeeth}}{a numeric vector}
    \item{\code{X24.SleepPerNight}}{a numeric vector}
    \item{\code{X25.MinutesExercisingDay}}{a numeric vector}
    \item{\code{X26.BooksReadMonth}}{a numeric vector}
    \item{\code{X27.ShowerLength}}{a numeric vector}
    \item{\code{X28.PercentRecordedTV}}{a numeric vector}
    \item{\code{X29.MostMilesRunOneDay}}{a numeric vector}
    \item{\code{X30.MorningPerson}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X31.PercentStudentsDateable}}{a numeric vector}
    \item{\code{X32.PercentYouAreMoreAttractive}}{a numeric vector}
    \item{\code{X33.PercentYouAreSmarter}}{a numeric vector}
    \item{\code{X34.RelationshipStatus}}{a factor with levels \code{Complicated} \code{Dating} \code{Married} \code{Single}}
    \item{\code{X35.AgeFirstKiss}}{a numeric vector}
    \item{\code{X36.WeaponAttractMate}}{a factor with levels \code{Humor} \code{Intelligence} \code{Looks} \code{Other}}
    \item{\code{X37.NumSignificantOthers}}{a numeric vector}
    \item{\code{X38.WeeksLongestRelationship}}{a numeric vector}
    \item{\code{X39.NumDrinksWeek}}{a numeric vector}
    \item{\code{X40.FavAlcohol}}{a factor with levels \code{Beer} \code{Liquor} \code{None} \code{Wine}}
    \item{\code{X41.SpeedingTickets}}{a numeric vector}
    \item{\code{X42.Smoker}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X43.IllegalDrugs}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X44.DefendantInCourt}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X45.NightInJail}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X46.BrokenBone}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X47.CentsCarrying}}{a numeric vector}
    \item{\code{X48.SawLastHarryPotter}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X49.NumHarryPotterRead}}{a numeric vector}
    \item{\code{X50.HoursContinuouslyAwake}}{a numeric vector}
    \item{\code{X51.NumCountriesVisited}}{a numeric vector}
  }
}
\details{
Students answered 51 questions to generate data for a project in an introductory statistics class at the University of Tennessee in the Fall of 2011.   The responses have been minimally modified or cleaned. The questions were:


1. What section are you in? (To be viewed only by the Stat 201 coordinator, and removed prior to distributing the data.)2. What is your gender? [M,F]3. What is your height (in inches)? [0,100]4. What is your weight (in pounds)? [0,1000]5. Are you satisfied with your current weight? [Yes, No I wish I weighed less, No I wish I weighed more]6. What is your class level? [Freshman, Sophomore, Junior, Senior, 5+ year senior, Non-traditional]7. What is your current GPA? [0,4]8. Are you a member of a fraternity/sorority? [Yes, No]9. Overall, do you consider your social/political beliefs to be: [more liberal, more conservative, a mix ofliberal and conservative views]10. Were you born in Tennessee? [Yes, No]11. What is your natural hair color? [Black, Brown, Red, Blond, Gray] ##There was a database errorrequiring Blond and Gray to be combined into one category.12. Did you grow up in the US? [Yes, No, Some time in the US but a significant time in another country]13. How many people share your current residence? Count yourself, so if you live alone, answer 1. Also, ifyou live in a dorm, count yourself plus just your roommates/suitemates. [1, 1000]14. How many Facebook friends do you currently have? (To see how many friends you have in Facebook,open a new tab or browser window and log in to Facebook, click the down arrow next to Account,select Edit Friends, and on the left of your screen your friends count is in parentheses.) [0,10000]15. How many people do you talk to on the phone in a typical day? [0,1000]16. How many MINUTES a day do you typically spend on the phone talking to people? [0,1440]17. How many different people do you typically send text messages to on a typical day? [0,1000]18. How many total texts do you think you send to people on a typical day? [0,5000]19. What type of computer do you use the most? [Mac, PC, Linux]20. Do you currently attend religious services at least once a month? [Yes, No]21. About how many HOURS PER WEEK do you work at a job? [0,168]22. What is the fastest speed you have achieved while driving a car (in miles per hour)? [0, 500]23. How many times per day do you typically brush your teeth? [0, 100]24. On a typical school night, how many HOURS do you sleep? [0, 24]25. How many MINUTES PER DAY do you typically engage in physical activity (e.g., walking to andfrom class, working out at the gym, sports practice, etc.)? [0, 1440]26. How many books have you read from cover to cover over the last month for pleasure? [0, 1000]27. How many MINUTES do you typically spend when you take a shower? [0, 1440]28. Advertisers are concerned that people are "fast forwarding" past their TV commercials, because moreand more people are recording broadcast television and watching it later (for example, on a DVR). Approximately what percent of the TV that you watch (that HAS commercials in it) is something you recorded, and therefore you can "fast forward" past the commercials? [0, 100]29. What is the longest that you've ever walked/run/hiked in a single day (in MILES)? [0,189]30. Do you consider yourself a "morning person"? [Yes, No]31. What percentage of UT students in your preferred gender do you think are dateable? [0, 100]32. What percentage of UT students do you think you are more attractive than? [0, 100]
33. What percentage of UT students do you think you are more intelligent than? [0, 100]34. What is your relationship status? [Single, Casually dating one or more people, Dating someone regularly, Engaged, Married, It's complicated]35. How old were you when you had your first romantic kiss? (Enter 0 if this has not yet happened.) [0, 99]36. Which of the following would you consider to be your main weapon for attracting a potential mate?[Looks, Intelligence, Sense of Humor, Other]37. How many boyfriends/girlfriends have you had? (We'll leave it up to you as to what constitutes aboyfriend or girlfriend.) [0, 1000]38. What is the longest amount of time (in WEEKS) that you have been in a relationship with a significantother? (A shortcut: take the number of months and multiply by 4, or the number of years and multiplyby 52.) [0, 4000]39. How many alcoholic beverages do you typically consume PER WEEK? (consider 1 alcoholic beveragea 12 oz. beer, a 4 oz. glass of wine, a 1 oz. shot of liquor, etc.) [0, 200]40. What is your favorite kind of alcoholic beverage? [I don't drink alcoholic beverages, Beer, Wine,Whiskey, Vodka, Gin, Tequila, Rum, Other]41. How may speeding tickets have you received? [0, 500]
42. Do you consider yourself a "smoker"? [Yes, No]43. Have you ever used an illegal/controlled substance? (Exclude alcohol/cigarettes consumed whenunderaged.) [Yes, No]44. Have you ever appeared before a judge/jury as a defendant? (Exclude speeding or parking tickets.)[Yes, No]45. Have you ever spent the night in a jail cell? [Yes, No]46. Have you ever broken a bone that required surgery or a cast (or both)? [Yes, No]47. Check your pockets and/or purse and report how much money in coins (in CENTS) that you currentlyare carrying. For example, if you have one quarter and one penny, type 26, not 0.26. [0, 1000]48. Have you seen the latest Harry Potter movie that came out in July 2011? [Yes, No]49. How many of the seven Harry Potter books have you completely read? [0, 7]50. Estimate the longest amount of time (in HOURS) that you have continuously stayed awake. [0, 450]51. How many countries have you ever stepped foot in outside an airport (include the US in your count)?[1, 196]
}

\keyword{datasets}
