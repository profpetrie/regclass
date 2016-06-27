\name{NFL}
\alias{NFL}
\docType{data}
\title{
NFL database
}
\description{
Statistics for NFL teams from the 2002-2012 seasons
}
\usage{data("NFL")}
\format{
  A data frame with 352 observations on the following 113 variables.
  \describe{
    \item{\code{X4.Wins}}{a numeric vector, number of wins (0-16) of an NFL team for the season}
    \item{\code{X5.OffTotPlays}}{a numeric vector, number of total plays made on offense for the season}
    \item{\code{X6.OffTotYdsperPly}}{a numeric vector}
    \item{\code{X7.OffTot1stDwns}}{a numeric vector}
    \item{\code{X8.OffPass1stDwns}}{a numeric vector}
    \item{\code{X9.OffRush1stDwns}}{a numeric vector}
    \item{\code{X10.OffFumblesLost}}{a numeric vector}
    \item{\code{X11.OffPassComp}}{a numeric vector}
    \item{\code{X12.OffPassComp}}{a numeric vector}
    \item{\code{X13.OffPassYds}}{a numeric vector}
    \item{\code{X14.OffPassTds}}{a numeric vector}
    \item{\code{X15.OffPassTD}}{a numeric vector}
    \item{\code{X16.OffPassINTs}}{a numeric vector}
    \item{\code{X17.OffPassINT}}{a numeric vector}
    \item{\code{X18.OffPassLongest}}{a numeric vector}
    \item{\code{X19.OffPassYdsperAtt}}{a numeric vector}
    \item{\code{X20.OffPassAdjYdsperAtt}}{a numeric vector}
    \item{\code{X21.OffPassYdsperComp}}{a numeric vector}
    \item{\code{X22.OffPasserRating}}{a numeric vector}
    \item{\code{X23.OffPassSacksAlwd}}{a numeric vector}
    \item{\code{X24.OffPassSackYds}}{a numeric vector}
    \item{\code{X25.OffPassNetYdsperAtt}}{a numeric vector}
    \item{\code{X26.OffPassAdjNetYdsperAtt}}{a numeric vector}
    \item{\code{X27.OffPassSack}}{a numeric vector}
    \item{\code{X28.OffRushYds}}{a numeric vector}
    \item{\code{X29.OffRushTds}}{a numeric vector}
    \item{\code{X30.OffRushLongest}}{a numeric vector}
    \item{\code{X31.OffRushYdsperAtt}}{a numeric vector}
    \item{\code{X32.OffFumbles}}{a numeric vector}
    \item{\code{X33.OffPuntReturns}}{a numeric vector}
    \item{\code{X34.OffPRYds}}{a numeric vector}
    \item{\code{X35.OffPRTds}}{a numeric vector}
    \item{\code{X36.OffPRLongest}}{a numeric vector}
    \item{\code{X37.OffPRYdsperAtt}}{a numeric vector}
    \item{\code{X38.OffKRTds}}{a numeric vector}
    \item{\code{X39.OffKRLongest}}{a numeric vector}
    \item{\code{X40.OffKRYdsperAtt}}{a numeric vector}
    \item{\code{X41.OffAllPurposeYds}}{a numeric vector}
    \item{\code{X42.1to19ydFGAtt}}{a numeric vector}
    \item{\code{X43.1to19ydFGMade}}{a numeric vector}
    \item{\code{X44.20to29ydFGAtt}}{a numeric vector}
    \item{\code{X45.20to29ydFGMade}}{a numeric vector}
    \item{\code{X46.1to29ydFG}}{a numeric vector}
    \item{\code{X47.30to39ydFGAtt}}{a numeric vector}
    \item{\code{X48.30to39ydFGMade}}{a numeric vector}
    \item{\code{X49.30to39ydFG}}{a numeric vector}
    \item{\code{X50.40to49ydFGAtt}}{a numeric vector}
    \item{\code{X51.40to49ydFGMade}}{a numeric vector}
    \item{\code{X52.50ydFGAtt}}{a numeric vector}
    \item{\code{X53.50ydFGAtt}}{a numeric vector}
    \item{\code{X54.40ydFG}}{a numeric vector}
    \item{\code{X55.OffTotFG}}{a numeric vector}
    \item{\code{X56.OffXP}}{a numeric vector}
    \item{\code{X57.OffTimesPunted}}{a numeric vector}
    \item{\code{X58.OffPuntYards}}{a numeric vector}
    \item{\code{X59.OffLongestPunt}}{a numeric vector}
    \item{\code{X60.OffTimesHadPuntBlocked}}{a numeric vector}
    \item{\code{X61.OffYardsPerPunt}}{a numeric vector}
    \item{\code{X62.FmblTds}}{a numeric vector}
    \item{\code{X63.DefINTTdsScored}}{a numeric vector}
    \item{\code{X64.BlockedKickorMissedFGRetTds}}{a numeric vector}
    \item{\code{X65.Off2ptConvMade}}{a numeric vector}
    \item{\code{X66.DefSafetiesScored}}{a numeric vector}
    \item{\code{X67.DefTotYdsAlwd}}{a numeric vector}
    \item{\code{X68.DefTotPlaysAlwd}}{a numeric vector}
    \item{\code{X69.DefTotYdsperPlayAlwd}}{a numeric vector}
    \item{\code{X70.DefTot1stDwnsAlwd}}{a numeric vector}
    \item{\code{X71.DefPass1stDwnsAlwd}}{a numeric vector}
    \item{\code{X72.DefRush1stDwnsAlwd}}{a numeric vector}
    \item{\code{X73.DefFumblesRecovered}}{a numeric vector}
    \item{\code{X74.DefPassCompAlwd}}{a numeric vector}
    \item{\code{X75.DefPassAttAlwd}}{a numeric vector}
    \item{\code{X76.DefPassCompAlwd}}{a numeric vector}
    \item{\code{X77.DefPassYdsAlwd}}{a numeric vector}
    \item{\code{X78.DefPassTdsAlwd}}{a numeric vector}
    \item{\code{X79.DefPassTDAlwd}}{a numeric vector}
    \item{\code{X80.DefPassINTs}}{a numeric vector}
    \item{\code{X81.DefPassINT}}{a numeric vector}
    \item{\code{X82.DefPassYdsperAttAlwd}}{a numeric vector}
    \item{\code{X83.DefPassAdjYdsperAttAlwd}}{a numeric vector}
    \item{\code{X84.DefPassYdsperCompAlwd}}{a numeric vector}
    \item{\code{X85.DefPasserRatingAlwd}}{a numeric vector}
    \item{\code{X86.DefPassSacks}}{a numeric vector}
    \item{\code{X87.DefPassSackYds}}{a numeric vector}
    \item{\code{X88.DefPassNetYdsperAttAlwd}}{a numeric vector}
    \item{\code{X89.DefPassAdjNetYdsperAttAlwd}}{a numeric vector}
    \item{\code{X90.DefPassSack}}{a numeric vector}
    \item{\code{X91.DefRushYdsAlwd}}{a numeric vector}
    \item{\code{X92.DefRushTdsAlwd}}{a numeric vector}
    \item{\code{X93.DefRushYdsperAttAlwd}}{a numeric vector}
    \item{\code{X94.DefPuntReturnsAlwd}}{a numeric vector}
    \item{\code{X95.DefPRTdsAlwd}}{a numeric vector}
    \item{\code{X96.DefKickReturnsAlwd}}{a numeric vector}
    \item{\code{X97.DefKRTdsAlwd}}{a numeric vector}
    \item{\code{X98.DefKRYdsperAttAlwd}}{a numeric vector}
    \item{\code{X99.DefTotFGAttAlwd}}{a numeric vector}
    \item{\code{X100.DefTotFGAlwd}}{a numeric vector}
    \item{\code{X101.DefXPAlwd}}{a numeric vector}
    \item{\code{X102.DefPuntsAlwd}}{a numeric vector}
    \item{\code{X103.DefPuntYdsAlwd}}{a numeric vector}
    \item{\code{X104.DefPuntYdsperAttAlwd}}{a numeric vector}
    \item{\code{X105.Def2ptConvAlwd}}{a numeric vector}
    \item{\code{X106.OffSafeties}}{a numeric vector}
    \item{\code{X107.OffRushSuccessRate}}{a numeric vector}
    \item{\code{X108.OffRunPassRatio}}{a numeric vector}
    \item{\code{X109.OffRunPly}}{a numeric vector}
    \item{\code{X110.OffYdsPt}}{a numeric vector}
    \item{\code{X111.DefYdsPt}}{a numeric vector}
    \item{\code{X112.HeadCoachDisturbance}}{a factor with levels \code{No} \code{Yes}, whether the head coached changed between this season and the last}
    \item{\code{X113.QBDisturbance}}{a factor with levels \code{No} \code{Yes}, whether the quarterback changed between this season and the last}
    \item{\code{X114.RBDisturbance}}{a factor with levels \code{?} \code{No} \code{Yes}, whether the runningback changed between this seasons and the last}
    \item{\code{X115.OffPassDropRate}}{a numeric vector}
    \item{\code{X116.DefPassDropRate}}{a numeric vector}
  }
}
\details{
Data was collected from many sources on the internet by a student for use in an independent study in the spring of 2013.  Abbreviations for predictor variables typically follow the full name in prior variables, e.g., KR = kick returns, PR = punt returns, XP = extra point.  Data is organized by year, so rows 1-32 rows are from 2002, rows 33-64 are from 2003, etc.
}
\source{
Contact the originator Weller Ross (jwellerross@gmail.com) for further details.
}


\keyword{datasets}
