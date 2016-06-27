\name{see.models}
\alias{see.models}

\title{
Examining model AICs from the "all possible" regressions procedure using regsubsets
}
\description{
This function takes the output of \code{regsubsets} and prints out a table of the top performing models based on AIC criteria. }

\usage{
see.models(ALLMODELS,report=0,aicc=FALSE,reltomin=FALSE) 
}

\arguments{
  \item{ALLMODELS}{
An object of class regsubsets created from \code{\link{regsubsets}} in package leaps. 
}
  \item{report}{
An optional argument specifying the number of top models to print out.  If left at a default of 0, the function reports all models whose AICs are within 4 of the lowest overall AIC.
}
  \item{aicc}{
Either \code{TRUE} or \code{FALSE}.  If \code{TRUE}, the AICc of a model is reported instead of the AIC.
}

  \item{reltomin}{
Either \code{TRUE} or \code{FALSE}, specifying whether the actual value of the AIC is reported (\code{FALSE}) or if AICs should be reported relative to the smallest overall AIC (\code{TRUE})
}
}

\details{
This function uses the \code{\link{summary}} function applied to the output of \code{\link{regsubsets}}.  The AIC is calculated to be the one obtained via \code{\link{extractAIC}} to allow for easy comparison with \code{\link{build.model}} and \code{\link{step}}.  

Although the model with the lowest AIC is typically chosen when making a descriptive model, models with AICs within 2 are essentially functionally equivalent.   Any model with an AIC within 2 of the smallest is a reasonable choice since there is no statistical reason to prefer one over the other.   The function returns a data frame of the AIC (or AICc), the number of variables, and the predictors in the "best" models.

Recall that the function \code{\link{regsubsets}} by default considers up to 8 predictors and does not preserve model hierarchy.  Interactions may appear without both component terms.  Further, only a subset of the indicator variables used to represent a categorical variable may appear.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}

\seealso{ \code{\link{regsubsets}},  \code{\link{extractAIC}}}
\examples{
  
  data(SALARY)
	ALL <- regsubsets(Salary~.^2,data=SALARY,method="exhaustive",nbest=4)
	see.models(ALL)
	
	#By default, regsubsets considers up to 8 predictors, here it looks at up to 15
	data(ATTRACTF)
	ALL <- regsubsets(Score~.,data=ATTRACTF,nvmax=15,nbest=1)
	see.models(ALL,aicc=TRUE,report=5)
	 }

