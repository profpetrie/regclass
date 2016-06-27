\name{VIF}
\alias{VIF}

\title{
Variance Inflation Factor
}
\description{
Calculates the variation inflation factors of all predictors in regression models}
\usage{
VIF(mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mod}{
A linear or logistic regression model
}

}

\details{
This function is a simple port of \code{vif} from the \code{car} package.  The VIF of a predictor is a measure for how easily it is predicted from a linear regression using the other predictors.  Taking the square root of the VIF tells you how much larger the standard error of the estimated coefficient is respect to the case when that predictor is independent of the other predictors.  

A general guideline is that a VIF larger than 5 or 10 is large, indicating that the model has problems estimating the coefficient.  However, this in general does not degrade the quality of predictions.  If the VIF is larger than 1/(1-R2), where R2 is the Multiple R-squared of the regression, then that predictor is more related to the other predictors than it is to the response.
}

\references{
Introduction to Regression and Modeling with R
}
\author{
Adam Petrie
}

\examples{
	#A case where the VIFs are small
	data(SALARY)
	M <- lm(Salary~.,data=SALARY)
	VIF(M)

  #A case where (some of) the VIFs are large
  data(BODYFAT)
  M <- lm(BodyFat~.,data=BODYFAT)
  VIF(M)
	 }
