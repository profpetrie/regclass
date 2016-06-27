\name{see.interactions}
\alias{see.interactions}

\title{
Examining pairwise interactions between quantitative variables for a fitted regression model
}
\description{
Plots all pairwise interactions present in a regression model to allow for an informal assessment of their strength.  When both variables are quantitative, the implicit regression lines of y vs. x1 for a small, the median, and a large value of x2 are provided (and vice versa).  If one of the variables is categorical, the implicit regression lines of y vs. x as displayed for each level of the categorical variable. }

\usage{
see.interactions(M,pos="bottomright",many=FALSE,level=0.95,...) 
}

\arguments{
  \item{M}{
A fitted linear regression model with interactions between quantitative variables. 
}
  \item{pos}{
Where to put the legend, one of "topleft", "top", "topright", "left","center","right","bottomleft","bottom","bottomright"
}
  \item{many}{
If \code{TRUE}, will give one pair of interaction plots per page and prompt the user to go to the next set (useful if 3+ interactions).  If \code{FALSE}, tries to put all pairs on one plot (recommended when 1 or 2 interactions in model).
}

  \item{level}{
Defines what makes a "small" and "large" value of x1 and x2.  By default \code{level} is 0.95 so that a large value is the 95th percentile and a small value is the 5th percentile.
}
  \item{...}{
Additional arguments to \code{legend}, namely \code{cex} to make them smaller.
}
}

\details{
When determining the implicit regression lines, all variables not involved in the interaction are assumed to be equal 0 (if quantitative) or equal to the level that comes first alphabetically (if categorical).  Tickmarks on the y axis are thus irrelevant and are not displayed.  

The plots allow an informal assessment of the presence of an interaction between the variables x1 and x2 in the model, after accounting for the other predictors.  If the implicit regression lines are nearly parallel, then the interaction is weak if it exists at all.  If the implicit regression lines have noticeably different slopes, then the interaction is strong.

When an interaction is present, then the strength of the relationship between y and x1 depends on the value of x2.  In other words, the difference in the average value of y between two individuals who differ in x1 by 1 unit depends on their (common) value of x2 (sometimes the expected difference is large; sometimes it is small).

If one of the variables in the interaction is cateogorical, the presence of an interaction implies that the strength of the relationship between y and x is different between levels of the categorical variable.  In other words, sometimes the difference in the expected value of y between an individual with level A and an individual with level B is large and sometimes it is small (and this depends on the common value of x of the individuals we are comparing).

The command \code{visualize.model} gives a better representation when only two predictors are in the model.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}

\seealso{ \code{\link{visualize.model}} }
\examples{
  
  data(SALARY)
	M <- lm(Salary~.^2,data=SALARY)
	#see.interactions(M,many=TRUE)  #not run since it requires user input
	
	data(STUDENT)
	M <- lm(CollegeGPA~(Gender+HSGPA+Family)^2+HSGPA*ACT,data=STUDENT)
	see.interactions(M,cex=0.6)
	 }

