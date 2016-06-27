\name{EX3.NFL}
\alias{EX3.NFL}
\docType{data}
\title{
NFL data for Exercise A in Chapter 3
}
\description{
NFL data for Exercise A in Chapter 3
}
\usage{data("EX3.NFL")}
\format{
  A data frame with 352 observations on the following 137 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Team}}{a factor with levels \code{Arizona} \code{Atlanta} \code{Baltimore} \code{Buffalo} \code{Carolina} \code{Chicago} \code{Cincinnati} \code{Cleveland} \code{Dallas} \code{Denver} \code{Detroit} \code{GreenBay} \code{Houston} \code{Indianapolis} \code{Jacksonville} \code{KansasCity} \code{Miami} \code{Minnesota} \code{NewEngland} \code{NewOrleans} \code{NYGiants} \code{NYJets} \code{Oakland} \code{Philadelphia} \code{Pittsburgh} \code{SanDiego} \code{SanFrancisco} \code{Seattle} \code{St.Louis} \code{TampaBay} \code{Tennessee} \code{Washington}}
    \item{\code{Next.Years.Wins}}{a numeric vector}
    \item{\code{Wins}}{a numeric vector}
    \item{\code{X1.Off.Tot.Yds}}{a numeric vector}
    \item{\code{X2.Off.Tot.Plays}}{a numeric vector}
    \item{\code{X3.Off.Tot.Yds.per.Ply}}{a numeric vector}
    \item{\code{X4.Off.Tot.1st.Dwns}}{a numeric vector}
    \item{\code{X5.Off.Pass.1st.Dwns}}{a numeric vector}
    \item{\code{X6.Off.Rush.1st.Dwns}}{a numeric vector}
    \item{\code{X7.Off.Tot.Turnovers}}{a numeric vector}
    \item{\code{X8.Off.Fumbles.Lost}}{a numeric vector}
    \item{\code{X9.Off.1st.Dwns.by.Penalty}}{a numeric vector}
    \item{\code{X10.Off.Pass.Comp}}{a numeric vector}
    \item{\code{X11.Off.Pass.Comp.}}{a numeric vector}
    \item{\code{X12.Off.Pass.Yds}}{a numeric vector}
    \item{\code{X13.Off.Pass.Tds}}{a numeric vector}
    \item{\code{X14.Off.Pass.INTs}}{a numeric vector}
    \item{\code{X15.Off.Pass.INT.}}{a numeric vector}
    \item{\code{X16.Off.Pass.Longest}}{a numeric vector}
    \item{\code{X17.Off.Pass.Yds.per.Att}}{a numeric vector}
    \item{\code{X18.Off.Pass.Adj.Yds.per.Att}}{a numeric vector}
    \item{\code{X19.Off.Pass.Yds.per.Comp}}{a numeric vector}
    \item{\code{X20.Off.Pass.Yds.per.Game}}{a numeric vector}
    \item{\code{X21.Off.Passer.Rating}}{a numeric vector}
    \item{\code{X22.Off.Pass.Sacks.Alwd}}{a numeric vector}
    \item{\code{X23.Off.Pass.Sack.Yds}}{a numeric vector}
    \item{\code{X24.Off.Pass.Net.Yds.per.Att}}{a numeric vector}
    \item{\code{X25.Off.Pass.Adj.Net.Yds.per.Att}}{a numeric vector}
    \item{\code{X26.Off.Pass.Sack.}}{a numeric vector}
    \item{\code{X27.Off.Game.Winning.Drives}}{a numeric vector}
    \item{\code{X28.Off.Rush.Yds}}{a numeric vector}
    \item{\code{X29.Off.Rush.Tds}}{a numeric vector}
    \item{\code{X30.Off.Rush.Longest}}{a numeric vector}
    \item{\code{X31.Off.Rush.Yds.per.Att}}{a numeric vector}
    \item{\code{X32.Off.Rush.Yds.per.Game}}{a numeric vector}
    \item{\code{X33.Off.Fumbles}}{a numeric vector}
    \item{\code{X34.Off.Punt.Returns}}{a numeric vector}
    \item{\code{X35.Off.PR.Yds}}{a numeric vector}
    \item{\code{X36.Off.PR.Tds}}{a numeric vector}
    \item{\code{X37.Off.PR.Longest}}{a numeric vector}
    \item{\code{X38.Off.PR.Yds.per.Att}}{a numeric vector}
    \item{\code{X39.Off.Kick.Returns}}{a numeric vector}
    \item{\code{X40.Off.KR.Yds}}{a numeric vector}
    \item{\code{X41.Off.KR.Tds}}{a numeric vector}
    \item{\code{X42.Off.KR.Longest}}{a numeric vector}
    \item{\code{X43.Off.KR.Yds.per.Att}}{a numeric vector}
    \item{\code{X44.Off.All.Purpose.Yds}}{a numeric vector}
    \item{\code{X45.X1.19.yd.FG.Att}}{a numeric vector}
    \item{\code{X46.X1.19.yd.FG.Made}}{a numeric vector}
    \item{\code{X47.X20.29.yd.FG.Att}}{a numeric vector}
    \item{\code{X48.X20.29.yd.FG.Made}}{a numeric vector}
    \item{\code{X49.X1.29.yd.FG.}}{a numeric vector}
    \item{\code{X50.X30.39.yd.FG.Att}}{a numeric vector}
    \item{\code{X51.X30.39.yd.FG.Made}}{a numeric vector}
    \item{\code{X52.X30.39.yd.FG.}}{a numeric vector}
    \item{\code{X53.X40.49.yd.FG.Att}}{a numeric vector}
    \item{\code{X54.X40.49.yd.FG.Made}}{a numeric vector}
    \item{\code{X55.X50yd.FG.Att}}{a numeric vector}
    \item{\code{X56.X50yd.FG.Made}}{a numeric vector}
    \item{\code{X57.X40yd.FG.}}{a numeric vector}
    \item{\code{X58.Total.FG.Att}}{a numeric vector}
    \item{\code{X59.Off.Tot.FG.Made}}{a numeric vector}
    \item{\code{X60.Off.Tot.FG.}}{a numeric vector}
    \item{\code{X61.Off.XP.Att}}{a numeric vector}
    \item{\code{X62.Off.XP.Made}}{a numeric vector}
    \item{\code{X63.Off.XP.}}{a numeric vector}
    \item{\code{X64.Off.Times.Punted}}{a numeric vector}
    \item{\code{X65.Off.Punt.Yards}}{a numeric vector}
    \item{\code{X66.Off.Longest.Punt}}{a numeric vector}
    \item{\code{X67.Off.Times.Had.Punt.Blocked}}{a numeric vector}
    \item{\code{X68.Off.Yards.Per.Punt}}{a numeric vector}
    \item{\code{X69.Fmbl.Tds}}{a numeric vector}
    \item{\code{X70.Def.INT.Tds.Scored}}{a numeric vector}
    \item{\code{X71.Blocked.Kick.or.Missed.FG.Ret.Tds}}{a numeric vector}
    \item{\code{X72.Total.Tds.Scored}}{a numeric vector}
    \item{\code{X73.Off.2pt.Conv.Made}}{a numeric vector}
    \item{\code{X74.Def.Safeties.Scored}}{a numeric vector}
    \item{\code{X75.Def.Tot.Yds.Alwd}}{a numeric vector}
    \item{\code{X76.Def.Tot.Plays.Alwd}}{a numeric vector}
    \item{\code{X77.Def.Tot.Yds.per.Play.Alwd}}{a numeric vector}
    \item{\code{X78.Def.Tot.1st.Dwns.Alwd}}{a numeric vector}
    \item{\code{X79.Def.Pass.1st.Dwns.Alwd}}{a numeric vector}
    \item{\code{X80.Def.Rush.1st.Dwns.Alwd}}{a numeric vector}
    \item{\code{X81.Def.Turnovers.Created}}{a numeric vector}
    \item{\code{X82.Def.Fumbles.Recovered}}{a numeric vector}
    \item{\code{X83.Def.1st.Dwns.Alwd.by.Penalty}}{a numeric vector}
    \item{\code{X84.Def.Pass.Comp.Alwd}}{a numeric vector}
    \item{\code{X85.Def.Pass.Att.Alwd}}{a numeric vector}
    \item{\code{X86.Def.Pass.Comp..Alwd}}{a numeric vector}
    \item{\code{X87.Def.Pass.Yds.Alwd}}{a numeric vector}
    \item{\code{X88.Def.Pass.Tds.Alwd}}{a numeric vector}
    \item{\code{X89.Def.Pass.TDAlwd}}{a numeric vector}
    \item{\code{X90.Def.Pass.INTs}}{a numeric vector}
    \item{\code{X91.Def.Pass.INT.}}{a numeric vector}
    \item{\code{X92.Def.Pass.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X93.Def.Pass.Adj.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X94.Def.Pass.Yds.per.Comp.Alwd}}{a numeric vector}
    \item{\code{X95.Def.Pass.Yds.per.Game.Alwd}}{a numeric vector}
    \item{\code{X96.Def.Passer.Rating.Alwd}}{a numeric vector}
    \item{\code{X97.Def.Pass.Sacks}}{a numeric vector}
    \item{\code{X98.Def.Pass.Sack.Yds}}{a numeric vector}
    \item{\code{X99.Def.Pass.Net.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X100.Def.Pass.Adj.Net.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X101.Def.Pass.Sack.}}{a numeric vector}
    \item{\code{X102.Def.Rush.Yds.Alwd}}{a numeric vector}
    \item{\code{X103.Def.Rush.Tds.Alwd}}{a numeric vector}
    \item{\code{X104.Def.Rush.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X105.Def.Rush.Yds.per.Game.Alwd}}{a numeric vector}
    \item{\code{X106.Def.Punt.Returns.Alwd}}{a numeric vector}
    \item{\code{X107.Def.PR.Tds.Alwd}}{a numeric vector}
    \item{\code{X108.Def.Kick.Returns.Alwd}}{a numeric vector}
    \item{\code{X109.Def.KR.Yds.Alwd}}{a numeric vector}
    \item{\code{X110.Def.KR.Tds.Alwd}}{a numeric vector}
    \item{\code{X111.Def.KR.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X112.Def.Tot.FG.Att.Alwd}}{a numeric vector}
    \item{\code{X113.Def.Tot.FG.Made.Alwd}}{a numeric vector}
    \item{\code{X114.Def.Tot.FG..Alwd}}{a numeric vector}
    \item{\code{X115.Def.XP.Att.Alwd}}{a numeric vector}
    \item{\code{X116.Def.XP.Made.Alwd}}{a numeric vector}
    \item{\code{X117.Def.XP..Alwd}}{a numeric vector}
    \item{\code{X118.Def.Punts.Alwd}}{a numeric vector}
    \item{\code{X119.Def.Punt.Yds.Alwd}}{a numeric vector}
    \item{\code{X120.Def.Punt.Yds.per.Att.Alwd}}{a numeric vector}
    \item{\code{X121.Def.2pt.Conv.Alwd}}{a numeric vector}
    \item{\code{X122.Off.Safeties}}{a numeric vector}
    \item{\code{X123.Off.Rush.Success.Rate}}{a numeric vector}
    \item{\code{X124.Head.Coach.Disturbance.}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X125.QB.Disturbance}}{a factor with levels \code{No} \code{Yes}}
    \item{\code{X126.RB.Disturbance}}{a factor with levels \code{?} \code{No} \code{Yes}}
    \item{\code{X127.Off.Run.Pass.Ratio}}{a numeric vector}
    \item{\code{X128.Off.Pass.Ply.}}{a numeric vector}
    \item{\code{X129.Off.Run.Ply.}}{a numeric vector}
    \item{\code{X130.Off.Yds.Pt}}{a numeric vector}
    \item{\code{X131.Def.Yds.Pt}}{a numeric vector}
    \item{\code{X132.Off.Pass.Drop.rate}}{a numeric vector}
    \item{\code{X133.Def.Pass.Drop.Rate}}{a numeric vector}
  }
}
\details{
See \code{NFL} for more details.  This dataset is actually a more complete version of \code{NFL} and contains additional variables such as the year, team, next year's wins of the team, etc., and could be used in place of the \code{NFL} data}
\keyword{datasets}
