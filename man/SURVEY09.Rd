\name{SURVEY09}
\alias{SURVEY09}
\docType{data}
\title{
Student survey 2009
}
\description{
Characteristics of students in an introductory statistics class at the University of Tennessee in 2009
}
\usage{data("SURVEY09")}
\format{
  A data frame with 579 observations on the following 47 variables.
  \describe{
    \item{\code{X01.ID}}{a numeric vector}
    \item{\code{X02.Gender}}{a factor with levels \code{Female} \code{Male}}
    \item{\code{X03.Weight}}{a numeric vector, estimated weight}
    \item{\code{X04.DesiredWeight}}{a numeric vector}
    \item{\code{X05.Class}}{a factor with levels \code{Freshman} \code{Junior} \code{Senior} \code{Sophmore}}
    \item{\code{X06.BornInTN}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X07.Greek}}{a factor with levels \code{No} \code{Yes}, if the student belongs to a fraternity/sorority}
    \item{\code{X08.UTFirstChoice}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X09.Churchgoer}}{a factor with levels \code{No} \code{Yes}, does student attend a religious service once a week}
    \item{\code{X10.ParentsMarried}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X11.GPA}}{a numeric vector}
    \item{\code{X12.SittingLocation}}{a factor with levels \code{Back} \code{Front} \code{Middle} \code{Varies}}
    \item{\code{X13.WeeklyHoursStudied}}{a numeric vector}
    \item{\code{X14.Scholarship}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X15.FacebookFriends}}{a numeric vector}
    \item{\code{X16.AgeFirstKiss}}{a numeric vector, age at which student had their first romantic kiss}
    \item{\code{X17.CarYear}}{a numeric vector}
    \item{\code{X18.DaysPerWeekAlcohol}}{a numeric vector, how many days a week student typically drinks}
    \item{\code{X19.NumDrinksParty}}{a numeric vector, how many drinks student typically has when he or she goes to a party}
    \item{\code{X20.CellProvider}}{a factor with levels \code{ATT} \code{Sprint} \code{USCellar} \code{Verizon}}
    \item{\code{X21.FreqDroppedCalls}}{a factor with levels \code{Occasionally} \code{Often} \code{Rarely}}
    \item{\code{X22.MarriedAt}}{a numeric vector, age by which student hopes to be married}
    \item{\code{X23.KidsBy}}{a numeric vector, age by which students hopes to have kids}
    \item{\code{X24.Computer}}{a factor with levels \code{Mac} \code{Windows}}
    \item{\code{X25.FastestDrivingSpeed}}{a numeric vector}
    \item{\code{X26.BusinessMajor}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X27.Major}}{a factor with levels \code{Business} \code{NonBusiness}}
    \item{\code{X28.TxtsPerDay}}{a numeric vector}
    \item{\code{X29.FootballGames}}{a numeric vector, games student hopes to attend}
    \item{\code{X30.HoursWorkOut}}{a numeric vector, per week}
    \item{\code{X31.MilesToSchool}}{a numeric vector, each day}
    \item{\code{X32.MoneyInBank}}{a numeric vector}
    \item{\code{X33.MoneyOnHaircut}}{a numeric vector}
    \item{\code{X34.PercentTuitionYouPay}}{a numeric vector}
    \item{\code{X35.SongsDownloaded}}{a numeric vector, songs typically downloaded (legally/illegally) a month}
    \item{\code{X36.ParentCollegeGraduate}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X37.HoursSleepPerNight}}{a numeric vector}
    \item{\code{X38.Last2DigitsPhone}}{a numeric vector}
    \item{\code{X39.NumClassesMissed}}{a numeric vector}
    \item{\code{X40.BooksReadThisYear}}{a numeric vector}
    \item{\code{X41.UseChopsticks}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X42.YourAttractiveness}}{a numeric vector, 1 (unattractive) to 5 (very attractive)}
    \item{\code{X43.Obama}}{a factor with levels \code{No} \code{NotVote} \code{Yes}}
    \item{\code{X44.HoursWorkedPerWeek}}{a numeric vector, at a job outside of a school}
    \item{\code{X45.MoviesInTheater}}{a numeric vector, number watched in theater this year}
    \item{\code{X46.KnowSomeoneH1N1}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X47.ReadBeacon}}{a factor with levels \code{No} \code{Yes}, the school newspaper}
  }
}
\details{
Students answered 47 questions to generate data for a project in an introductory statistics class at the University of Tennessee in the Fall of 2009.  The responses here have only had minimal cleaning (negative numbers omitted) so some data is bad (e.g., a weight of 16).  The questions were:

Stat 201 Fall 2009 Survey Questions
1. What section are you in? 
2. Gender [Male, Female]
3. Your weight (in pounds) [0 to 500]
4. What is your desired weight (in pounds)? [0 to 1000]
5. What year are you? [Freshman, Sophomore, Junior, Senior, Other]
6. Were you born in Tennessee? [Yes, No]
7. Are you a member of a Greek social society (i.e., a Fraternity/Sorority? [Yes, No]
8. Was UT your first choice? [Yes, No]
9. Do you usually attend a religious service once a week? [Yes, No]
10. Are your parents married? [Yes, No]
11. Thus far, what is your GPA (look up on CPO if you need to)? [0 to 4]
12. Given a choice, where do you like to sit in class? [The front row, Near the front,
Around the middle, Near the back, The back row, Somewhere different all the
time]
13. On average, how many hours per day do you study/do homework? [0 to 24]
14. Do you receive one or more scholarships? [Yes, No]
15. How many Facebook friends do you have? Type -1 if you dont use Facebook.
[-1 to 5000]
16. How old were you when you had your first romantic kiss? Type -1 if it has not
happened yet. [-1 to 100]
17. What is the year of the car you drive most often? Type a four digit number.
Enter 1908 if you never drive a car. [1908 to 2011]
18. On average, how many days per week do you consume one or more alcoholic
beverage? Type -1 if you never drink alcoholic beverages. [-1 to 7]
19. On average, how many alcoholic drinks do you have when you party? Type -1 if
you never drink alcoholic beverages. [-1 to 100]
20. Which cell phone provider do you use (the most, if you have multiple services)?
[ATT (Cingular), Cricket, Sprint, T-Mobile, U.S. Cellular, Verizon, Other, I
dont use a cell phone]
21. How often do you have dropped calls? [Never, Rarely, Sometimes, Often,
Constantly]
22. What is the age at which you hope to be married? Type -1 if you are already
married and type -2 if you never want to get married. [-2 to 100]
23. What is the age at which you hope to have your first child? Type -1 if you
already have one or more children, type -2 if you never want to have children. [-2
to 100]
24. What type of computer do you use most often? [PC running Windows, PC
running linux, Mac running Mac OS, Mac running linux, Mac running Windows,
Other, I dont understand the choices above]
25. What is the fastest speed (in miles per hour) you have ever achieved while driving
a car? [0 to 300]
26. Do you plan on going into the Business School? [Yes, No]
27. What is your desired (or actual) major? [Accounting, Economics, Finance,
Logistics, Marketing, Statistics, Other]
28. How many text messages do you typically send on any given day? Type -1 if you never send text messages. [-1 to 1000]
29. How many UT football games do you hope to attend this year? (Include games already attended this year. Do not include scrimmages.) [0 to 14]
30. How many hours a week do you work out/play sports/exercise, etc.? [0 to 168]
31. How many miles do you drive to school on a typical day? [0 to 500]
32. How much money do you have in your bank account? Type -999 if you think its
none of our business. [-999 to 10000000]
33. How much do you typically spend on a hair cut? [0 to 1000]
34. What percent of tuition are you personally responsible for? Type a number
between 0 and 100.  [0 to 100]
35. Typically, how many songs do you download a month (both legally and/or
illegally)? [0 to 10000]
36. Did at least one of your parents graduate from college? [Yes, No]
37. On average, how many hours do you sleep a night? [0 to 24]
38. What are the last two digits of your phone number? (Type 0 for 00, 1 for 01, 2 for
02, etc.) [0 to 99]
39. Approximately how many classes have you missed/skipped so far this semester?
(For all your courses, including absences for legitimate excuses) [0 to 150]
40. How many books (other than textbooks) have you read so far this year? [0 to
1000]
41. Are you proficient with a pair of chopsticks? [Yes, No]
42. How would you rate your attractiveness on a scale of 1 to 5, with 5 being the most
attractive? [1 to 5]
43. Did you vote for Barack Obama in last Novembers election? [Yes, No I voted
for someone else, No I didnt vote at all]
44. On average, how many hours do you work at a job per week? [0 to 168]
45. How many movies have you watched in theaters this year? [0 to 1000]
46. Do you personally know someone who has come down with H1N1 virus? [Yes,
No]
47. Do you read the Daily Beacon on a regular basis? [Yes, No]
}

\keyword{datasets}
