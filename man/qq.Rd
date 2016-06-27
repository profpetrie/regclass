\name{qq}
\alias{qq}

\title{
QQ plot
}

\description{
A QQ plot designed with statistics students in mind
}

\usage{
qq(x,ax=NA,leg=NA,cex.leg=0.8)
}

\arguments{
  \item{x}{
A vector of data
}
  \item{ax}{
The name you want to call \code{x} for the x-axis (if omitted, defaults to what was passed as the first argument).  Useful if the variable is a column in a dataframe. 
}
 \item{leg}{Optional argument that places a legend in the top left of the plot with the text given by \code{leg}}
 
  \item{cex.leg}{Optional argument that gives the magnification of the text in the legend}

}

\details{
This function gives QQ plots that are better-suited for students.  Instead of plotting quantiles, it plots the observed values of \code{x} versus the values expected had \code{x} come from a Normal distribution.  The distribution can be considered approximately Normal if the points stay within the upper/lower dashed red lines (with the possible exception at the far left/right) and if there is no overall global curvature.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}


\examples{
   #Not Normal
   data(TIPS)
   qq(TIPS$Bill,ax="Bill")
   
   #Pretty Normal
   data(ATTRACTF)
   qq(ATTRACTF$Score,ax="Attractiveness Score")
   
   
	 }
