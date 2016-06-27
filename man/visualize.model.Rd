\name{visualize.model}
\alias{visualize.model}

\title{
Visualizations of one or two variable linear or logistic regressions or of partitions models
}

\description{
Provides useful plots to illustrate the inner-workings of regression models with one or two predictors or a partition model with not too many branches.
}
\usage{
visualize.model(M,loc="topleft",level=0.95,cex.leg=0.7,...)
}

\arguments{
  \item{M}{
A linear or logistic regression model with one or two predictors (not all categorical) or a partition model
}
  \item{loc}{
The location for the legend, if one is to be displayed.  Can also be "top", "topright", "left", "center", "right", "bottomleft", "bottom", or "bottomright".
  }
  \item{level}{
  The level of confidence for confidence and prediction intervals for the case of simple linear regression.
  }
  \item{cex.leg}{
  Magnification factor for text in legends.  Smaller numbers indicate smaller text.  Default is 0.7.
  }
  \item{...}{
  Additional arguments to \code{plot}.  This is typically only used for logistic regression models where \code{xlim} is to be specified to see the entirety of the curve instead of using the default range.
  }
}

\details{
If \code{M} is a simple linear regression model, this provides a scatter plot, fitted line, and confidence/prediction intervals.

If \code{M} is a simple logistic regression model, this provides the fitted logistic curve.

If \code{M} is a regression with two quantitative predictors, this provides the implicit regression lines when one of the variables equals its 5th (small), 50th (median), and 95th (large) percentiles.  The model may have interaction terms.  In this case, the p-value of the interaction is output.  The definition of small and large can be changed with the \code{level} argument.

If \code{M} is a regression with a quantitative predictor and a categorical predictor (with or without interactions), this provides the implicit regression lines for each level of the categorical predictor.  The p-value of the effect test is displayed if an interaction is in the model.

If \code{M} is a partition model from \code{rpart}, this shows the tree.
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\seealso{ \code{\link{rpart}}, \code{\link{lm}}, \code{\link{glm}}}
\examples{
  data(SALARY)
  #Simple linear regression with 90% confidence and prediction intervals
  M <- lm(Salary~Education,data=SALARY)
  visualize.model(M,level=0.90,loc="bottomright")
  
  #Multiple linear regression with two quantitative predictors (no interaction)
  M <- lm(Salary~Education+Experience,data=SALARY)
  visualize.model(M)

  #Multiple linear regression with two quantitative predictors (with interaction)
  #Take small and large to be the 25th and 75th percentiles
  M <- lm(Salary~Education*Experience,data=SALARY)
  visualize.model(M,level=0.75)
  
  #Multiple linear regression with one categorical and one quantitative predictor
  M <- lm(Salary~Education*Gender,data=SALARY)
  visualize.model(M)

  data(WINE)
  #Simple logistic regression with expanded x limits
  M <- glm(Quality~alcohol,data=WINE,family=binomial)
  visualize.model(M,xlim=c(0,20))

  #Multiple logistic regression with two quantitative predictors
  M <- glm(Quality~alcohol*sulphates,data=WINE,family=binomial)
  visualize.model(M,loc="left")

  data(TIPS)
  #Multiple logistic regression with one categorical and one quantitative predictor
  #expanded x-limits to see more of the curve
  M <- glm(Smoker~PartySize*Weekday,data=TIPS,family=binomial)
  visualize.model(M,loc="topright",xlim=c(-5,15))
  
  #Partition model predicting a quantitative response
  TREE <- rpart(Salary~.,data=SALARY)
  visualize.model(TREE)
  
  #Partition model predicting a categorical response
  TREE <- rpart(Quality~.,data=WINE)
  visualize.model(TREE)
}

