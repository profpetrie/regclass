\name{check.regression}
\alias{check.regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear and Logistic Regression diagnostics
}
\description{
If the model is a linear regression, obtain tests of linearity, equal spread, and Normality as well as relevant plots (residuals vs. fitted values, histogram of residuals, QQ plot of residuals, and predictor vs. residuals plots).  If the model is a logistic regression model, a goodness of fit test is given. }
\usage{
check.regression(M,extra=FALSE,tests=TRUE,simulations=500,n.cats=10,seed=NA,prompt=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A regression model fitted with either \code{\link{lm}} or \code{\link{glm}}
}
  \item{extra}{
If \code{TRUE}, allows user to generate the predictor vs. residual plots for linear regression models.  
}
  \item{tests}{
If \code{TRUE}, performs statistical tests of assumptions.  If \code{FALSE}, only visual diagnostics are provided.
}

  \item{simulations}{
The number of artificial samples to generate for estimating the p-value of the goodness of fit test for logistic regression models.  These artificial samples are generated assuming the fitted logistic regression is correct.
}
  \item{n.cats}{
Number of (roughly) equal sized categories for the Hosmer-Lemeshow goodness of fit test for logistic regression models
}
  \item{seed}{
If specified, sets the random number seed before generation of artificial samples in the goodness of fit tests for logistic regression models.
}
  \item{prompt}{
For documentation only, if \code{FALSE}, skips prompting user for extra plots
}

}
\details{
This function provides standard visual and statistical diagnostics for regression models.  

For linear regression, tests of linearity, equal spread, and Normality are performed and residuals plots are generated.

The test for linearity (a goodness of fit test) is an F-test.  A simple linear regression model predicting y from x is fit and compared to a model treating each value of the predictor as some level of a categorical variable.  If this more sophisticated model does not offer a significant improvement in the sum of squared errors, the linearity assumption in that predictor is reasonable.  If the p-value is larger 0.05, then statistically we can consider the relationship to be linear.  If the p-value is smaller than 0.05, check the residuals plot and the predictor vs residuals plots for signs of obvious curvature (the test can be overly sensitive to inconsequential violations for larger sample sizes).  The test can only be run if are two or more individuals that have a common value of x.  A test of the model as a whole is run similarly if at least two individuals have identical combinations of all predictor variables.

Note:  if categorical variables, interactions, polynomial terms, etc., are present in the model, the test for linearity is conducted for each term even when it does not necessarily make sense to do so.

The test for equal spread is the Breusch-Pagan test.  If the p-value is larger 0.05, then statistically we can consider the residuals to have equal spread everywhere.  If the p-value is smaller than 0.05, check the residuals plot for obvious signs of unequal spread (the test can be overly sensitive to inconsequential violations for larger sample sizes).

The test for Normality is the Shapiro-Wilk test when the sample size is smaller than 5000, or the KS-test for larger sample sizes.  If the p-value is larger 0.05, then statistically we can consider the residuals to be Normally distributed. If the p-value is smaller than 0.05, check the histogram and QQ plot of residuals to look for obvious signs of non-Normality (e.g., skewness or outlier).  The test can be overly sensitive to inconsequential violations for larger sample sizes.

The first three plots displayed are the residuals plot (residuals vs. fitted values), histogram of residuals, and QQ plot of residuals.  The function gives the option of pressing Enter to display additional predictor vs. residual plots if \code{extra=TRUE}, or to terminate by typing 'q' in the console and pressing Enter.  If polynomial or interactions terms are present in the model, a plot is provided for each term.  If categorical predictors are present, plots are provided for each indicator variable.

For logistic regression, two goodness of fit tests are offered.

Method 1 is a crude test that assumes the fitted logistic regression is correct, then generates an artifical sample according the predicted probabilities.  A chi-squared test is conducted that compares the observed levels to the predicted levels.  The test is failed is the p-value is less than 0.05.  The test is not sensitive to departures from the logistic curve unless the sample size is very large or the logistic curve is a really bad model.

Method 2 is a Hosmer-Lemeshow type goodness of fit test.  The observations are put into 10 groups according to the probability predicted by the logistic regression model.  For example, if there were 200 observations, the first group would have the cases with the 20 smallest predicted probabilities, the second group would have the cases with the 20 next smallest probabilities, etc.  The number of cases with the level of interest is compared with the expected number given the fitted logistic regression model via a chi-squared test.  The test is failed is the p-value is less than 0.05. 

Note:  for both methods, the p-values of the chi-squared tests are estimate via Monte Carlo simulation instead of any asymptotic results.  


}

\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link{shapiro.test}}, \code{\link{ks.test}}, bptest (in package lmtest).  The goodness of fit test for logistic regression is further detailed and implemented in package 'rms' using the commands lrm and residuals. }

\examples{
  #Simple linear regression where everything looks good 
  data(FRIEND)
  M <- lm(FriendshipPotential~Attractiveness,data=FRIEND)
  check.regression(M)
  
  #Multiple linear regression (prompt is FALSE only for documentation)
  data(AUTO)
  M <- lm(FuelEfficiency~.,data=AUTO)
  check.regression(M,extra=TRUE,prompt=FALSE)
  
  
  #Multiple linear regression with a categorical predictors and an interaction
  data(TIPS)
  M <- lm(TipPercentage~Bill*PartySize*Weekday,data=TIPS)
  check.regression(M)
  
  #Multiple linear regression with polynomial term (prompt is FALSE only for documentation)
  #Note:  in this example only plots are provided
  data(BULLDOZER)
  M <- lm(SalePrice~.-YearMade+poly(YearMade,2),data=BULLDOZER)
  check.regression(M,extra=TRUE,tests=FALSE,prompt=FALSE)

  #Simple logistic regression.  Use 8 categories since only 8 unique values of Dose
  data(POISON)
	M <- glm(Outcome~Dose,data=POISON,family=binomial)
	check.regression(M,n.cats=8,seed=892)

  #Multiple logistic regression
  data(WINE)
  M <- glm(Quality~.,data=WINE,family=binomial)
	check.regression(M,seed=2010)

  
	 }

