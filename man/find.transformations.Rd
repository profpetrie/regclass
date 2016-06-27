\name{find.transformations}
\alias{find.transformations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transformations for simple linear regression
}
\description{
This function takes a simple linear regression model and finds the transformation of x and y that results in the highest R2 
}
\usage{
find.transformations(M,powers=seq(from=-3,to=3,by=.25),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A simple linear regression model fitted with \code{\link{lm}}
}
  \item{powers}{
A sequence of powers to try for x and y.  By default this ranges from -3 to 3 in steps of 0.25.  If 0 is a valid power, then the logarithm is used instead.
}

  \item{...}{
Additional arguments to \code{plot} such as \code{pch} and \code{cex}.
}

}
\details{
The relationship between y and x may not be linear.  However, some transformation of y may have a linear relationship with some transformation of x.  This function considers simple linear regression with x and y raised to powers between -3 and 3 (in 0.25 increments) by default.  The function outputs a list of the top models as gauged by R^2 (all models within 0.02 of the highest R^2).  Note:  there is no guarantee that these "best" transformations are actually good, since a large R^2 can be produced by outliers created during transformations.  A plot of the transformation is also provided.

It is exceedingly rare that the "best" transformation is raising x and y to the 1 power (i.e., the original variables).  Transformations are typically used only when there are issues in the residuals plots, highly skewed variables, or physical/logical justifications.

}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\examples{
  #Straightforward example
  data(BULLDOZER)
	M <- lm(SalePrice~YearMade,data=BULLDOZER)
	find.transformations(M,pch=20,cex=0.3)

  #Results are very misleading since selected models have high R2 due to outliers
  data(MOVIE)
  M <- lm(Total~Weekend,data=MOVIE)
	find.transformations(M,powers=seq(-2,2,by=0.5))
	 }

