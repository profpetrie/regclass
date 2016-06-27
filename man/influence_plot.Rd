\name{influence_plot}
\alias{influence_plot}
\alias{influence.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Influence plot for regression diganostics
}
\description{
This function plots the leverage vs. deleted studentized residuals for a regression model, highlighting points that are influent based on these two factors as well as Cook's distance  
}
\usage{
influence_plot(M,large.cook)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A linear regression model fitted with lm()
}
  \item{large.cook}{
The threshold for a "large" Cook's distance.  If not specified, a default of 4/n is used.
}


}

\details{
A point is influential if its addition to the data changes the regression substantially.   One way of measuring influence is by looking at the point's leverage (distance from the center of the predictor's datacloud with respect to it shape) and deleted studentized residual (relative size of the residual with respect to a regression made without that point).  Points with leverages larger than 2(k+1)/n (where k is the number of predictors) and deleted studentized residuals larger than 2 in magnitude are considered influential.  

Influence can also be measured by Cook's distance, which essentially combines the above two measures.  This function considers the Cook's distances to be large when it exceeds 4/n, but the user can specify another cutoff.

The radius of a point is proportional to the square root of the Cook's distance.  Influential points according to leverage/residual criteria have an X through them while influential points according to Cook's distance are bolded.  

The function returns the row numbers of influential observations.
}
\value{
A list with the row numbers of influential points according to Cook's distance (\code{$Cooks}) and according to leverage/residual criteria (\code{$Leverage}).
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\seealso{ \code{\link{cooks.distance}}, \code{\link{hatvalues}}, \code{\link{rstudent}} }

\examples{
  data(TIPS)
  M <- lm(TipPercentage~.-Tip,data=TIPS)
	influence_plot(M)

	
	 }

