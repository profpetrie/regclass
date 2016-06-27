\name{extrapolation.check}
\alias{extrapolation.check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A crude check for extrapolation
}
\description{
This function computes the Mahalanobis distance of points as a check for potential extrapolation. }

\usage{
extrapolation.check(M,newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A fitted model that uses only quantitative variables
}
  \item{newdata}{
Data frame (that has the exact same columns as predictors used to fit the model \code{M}) whose Mahalanobis distances are to be calculated.
}

}

\details{
This function computes the shape of the predictor data cloud and calculates the distances of points from the center (with respect to the shape of the data cloud).  Extrapolation occurs at a combination of predictors that is far from combinations used to build the model.  An observation with a large Mahalanobis distance MAY be far from the observations used to build the model and thus MAY require extrapolation.

Note:  analysis assumes the predictor data cloud is roughly elliptical (this may not be a good assumptions).  

The function reports the percentiles of the Mahalanobis distances of the points in \code{newdata}.  Percentiles are the fraction of observations used in model that are CLOSER to
the center than the point(s) in question.  Large values of these percentages indicate a greater risk for extrapolation.  If \code{Percentile} is about 99% or higher
you may be extrapolating.  

The method is sensitive to outliers clusters of outliers and gives only a crude idea of the potential for extrapolation.
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\seealso{ \code{\link{mahalanobis}} } 
\examples{
  data(SALARY)
  M <- lm(Salary~Education*Experience+Months,data=SALARY)
  newdata <- data.frame(Education=c(0,5,10),Experience=c(15,15,15),Months=c(0,0,0))
  extrapolation.check(M,newdata) 
  #Individuals 1 and 3 are rather unusual (though not terribly) while individual 2 is typical.  
}

