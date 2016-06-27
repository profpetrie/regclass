\name{JUNK}
\alias{JUNK}
\docType{data}
\title{
Junk-mail dataset
}
\description{
Building a junk mail classifier based on word and character frequencies
}
\usage{data("JUNK")}
\format{
  A data frame with 4601 observations on the following 58 variables.
  \describe{
    \item{\code{Junk}}{a factor with levels \code{Junk} \code{Safe}}
    \item{\code{make}}{a numeric vector, the percentage (0-100) of words in the email that are the word \code{make} }
    \item{\code{address}}{a numeric vector}
    \item{\code{all}}{a numeric vector}
    \item{\code{X3d}}{a numeric vector, the percentage (0-100) of words in the email that are the word \code{3d}}
    \item{\code{our}}{a numeric vector}
    \item{\code{over}}{a numeric vector}
    \item{\code{remove}}{a numeric vector}
    \item{\code{internet}}{a numeric vector}
    \item{\code{order}}{a numeric vector}
    \item{\code{mail}}{a numeric vector}
    \item{\code{receive}}{a numeric vector}
    \item{\code{will}}{a numeric vector}
    \item{\code{people}}{a numeric vector}
    \item{\code{report}}{a numeric vector}
    \item{\code{addresses}}{a numeric vector}
    \item{\code{free}}{a numeric vector}
    \item{\code{business}}{a numeric vector}
    \item{\code{email}}{a numeric vector}
    \item{\code{you}}{a numeric vector}
    \item{\code{credit}}{a numeric vector}
    \item{\code{your}}{a numeric vector}
    \item{\code{font}}{a numeric vector}
    \item{\code{X000}}{a numeric vector, the percentage (0-100) of words in the email that are the word \code{000}}
    \item{\code{money}}{a numeric vector}
    \item{\code{hp}}{a numeric vector}
    \item{\code{hpl}}{a numeric vector}
    \item{\code{george}}{a numeric vector}
    \item{\code{X650}}{a numeric vector}
    \item{\code{lab}}{a numeric vector}
    \item{\code{labs}}{a numeric vector}
    \item{\code{telnet}}{a numeric vector}
    \item{\code{X857}}{a numeric vector}
    \item{\code{data}}{a numeric vector}
    \item{\code{X415}}{a numeric vector}
    \item{\code{X85}}{a numeric vector}
    \item{\code{technology}}{a numeric vector}
    \item{\code{X1999}}{a numeric vector}
    \item{\code{parts}}{a numeric vector}
    \item{\code{pm}}{a numeric vector}
    \item{\code{direct}}{a numeric vector}
    \item{\code{cs}}{a numeric vector}
    \item{\code{meeting}}{a numeric vector}
    \item{\code{original}}{a numeric vector}
    \item{\code{project}}{a numeric vector}
    \item{\code{re}}{a numeric vector}
    \item{\code{edu}}{a numeric vector}
    \item{\code{table}}{a numeric vector}
    \item{\code{conference}}{a numeric vector}
    \item{\code{semicolon}}{a numeric vector, the percentage (0-100) of characters in the email that are semicolons}
    \item{\code{parenthesis}}{a numeric vector}
    \item{\code{bracket}}{a numeric vector}
    \item{\code{exclamation}}{a numeric vector}
    \item{\code{dollarsign}}{a numeric vector}
    \item{\code{hashtag}}{a numeric vector}
    \item{\code{capital_run_length_average}}{a numeric vector, average length of uninterrupted sequence of capital letters}
    \item{\code{capital_run_length_longest}}{a numeric vector, length of longest uninterrupted sequence of capital letters}
    \item{\code{capital_run_length_total}}{a numeric vector, total number of capital letters in the email}
  }
}
\details{
The collection of junk emails came from the postmaster and individuals who classified the email as junk.  The collection of safe emails were from work and personal emails.  Note that most of the variables are percents and can vary from 0-100, though most values are much less than 1 (1\%).  
}
\source{
Adapted from the Spambase Data Set at the UCI data repository \url{https://archive.ics.uci.edu/ml/datasets/Spambase}.  Creators:   Mark Hopkins, Erik Reeber, George Forman, Jaap Suermondt; Hewlett-Packard Labs, 1501 Page Mill Rd., Palo Alto, CA 94304.   Donor:  George Forman (gforman at nospam hpl.hp.com)
}
\keyword{datasets}
