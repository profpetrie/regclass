\name{choose.order}
\alias{choose.order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Choosing order of a polynomial model
}
\description{
This function takes a simple linear regression model and displays the adjusted R^2 and AICc for the original model (order 1) and for polynomial models up to a specified maximum order and plots the fitted models.
}
\usage{
choose.order(M,max.order=6,sort=FALSE,loc="topleft",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A simple linear regression model fitted with lm()
}
  \item{max.order}{
The maximum order of the polynomial model to consider.
}

  \item{sort}{
How to sort the results.  If TRUE, "R2", "r2", "r2adj", or "R2adj", sorts from highest to lowest adjusted R^2.  If "AIC", "aic", "AICC", "AICc", sorts by AICc.
}

  \item{loc}{
Location of the legend.  Can also be "top", "topright", "bottomleft", "bottom", "bottomright", "left", "right", "center"
}
  \item{...}{
Additional arguments to plot(), e.g., pch }

}
\details{
The function outputs a table of the order of the polynomial and the according adjusted R^2 and AICc.  One strategy for picking the best order is to find the highest value of R^2 adjusted, then to choose the smallest order (simplest model) that has an R^2 adjusted within 0.005.  Another strategy is the find the lowest value of AICc, then to choose the smallest order that has an AICc no more than 2 higher.

The scatterplot of the data is provided and the fitted models are displayed as well.
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\examples{
  data(BULLDOZER)
	M <- lm(SalePrice~YearMade,data=BULLDOZER)
  #Unsorted list, messing with plot options to make it look alright
	choose.order(M,pch=20,cex=.3)
	
	#Sort by R2adj.  A 10th order polynomial is highest, but this seems overly complex
	choose.order(M,max.order=10,sort=TRUE)

	#Sort by AICc.  4th order is lowest, but 2nd order is simpler and within 2 of lowest
	choose.order(M,max.order=10,sort="aic")

	 }
