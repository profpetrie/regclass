\name{build.model}
\alias{build.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variable selection for descriptive or predictive linear and logistic regression models
}
\description{
This function uses \code{\link{bestglm}} to consider an extensive array of models and makes recommendations on what set of variables is appropriate for the final model.  Model hierarchy is not preserved.  Interactions and multi-level categorical variables are allowed.
}
\usage{
build.model(form,data,type="predictive",Kfold=5,repeats=10,
prompt=TRUE,seed=NA,holdout=NA,...) }

\arguments{
  \item{form}{
A model formula giving the most complex model to consider (often predicting y from all variables \code{y~.} or all variables including two-way interactions \code{y~.^2})
}
  \item{data}{
Name of the data frame that contain all variables specifed by \code{form}
}
  \item{type}{
Either "predictive" or "descriptive".  If \code{predictive}, the procedure estimates the generalization error of candidate models via repeated K-fold cross-validation.  If \code{descriptive}, the procedure calculates the AICs of models.
}
  \item{Kfold}{
The number of folds for repeated K-fold cross-validation for predictive model building
}
  \item{repeats}{
The number of repeats for repeated K-fold cross-validation for predictive model building
}
\item{seed}{
If specified, the random number seed used to initialize the repeated K-fold cross-validation procedure so that results can be reproduced.
}
\item{prompt}{
If \code{FALSE}, the procedure will not output a warning to the user if fitting the candidate set will take "long".  Usually only run with \code{FALSE} for documentation purposes.  
}
\item{holdout}{
A optional dataframe to serve as a holdout sample.  The generalization error on the holdout sample will be calculated and displayed for the best model at each number of predictors.
}
\item{...}{
Additional arguments to \code{\link{bestglm}}.  This allows the procedure to do a search rather than exhaustive enumeration or allows tweaking of the number of reported models or maximum number of independent variables (\code{nvmax}), etc.  See \code{\link{bestglm}} and \code{\link{regsubsets}}.
}

}
\details{
This procedure takes the formula specified by \code{form} and the original dataframe and simply converts it into a form that \code{\link{bestglm}} (which normally cannot do cross-validation when categorical variables are involved) can use by adding in columns to represent interactions and categorical variables.

One the dataframe has been generated, a warning is given to the user if the procedure may take too long (many rows or many potential predictors), and then \code{\link{bestglm}} is run.  A plot and table of models' performances is given, as well as a recommendation for a final set of variables (model with the lowest AIC/estimated generalization error, or a simpler model that is more or less equivalent). 

The command returns a list with \code{bestformula} (the formula of the model with the lowest AIC or the model chosen by the one standard deviation rule), \code{bestmodel} (the fitted model that had the lowest AIC or the one chosen by the one standard deviation rule), \code{predictors} (a list giving the predictors that appeared in the best model with 1 predictor, with 2 predictors, etc).   

If a descriptive model is sought, the last component of the returned list is \code{AICtable} (a data frame containing the number of predictors and the AIC of the best model with that number of predictors; a * denotes the model with the lowest AIC while a + denotes the simplest model whose AIC is within 2 of the lowest).  

If a predictive model is sought, the last component of the returned list is \code{CVtable} (a data frame containing the number of predictors and the estimated generalization error of the best model with that number of predictors along with the SD from repeated K-fold cross validation; a * denotes the model with the lowest error while the + denotes the model selected with the one standard deviation rule).  Note that the generalization error in the second column of this table is the squared error if the response is quantitative and is another measure of error (not the misclassification rate) if the response is categorical.  Additional columns are provided to give the root mean squared error or misclassification rate.

Note:  \code{bestmodel} is the one selected by the one standard deviation rule or the simplest one whose AIC is no more than 2 above the model with the lowest AIC.  Because the procedure does not respect model hierarchy and can include interactions, the formula returned may not be immediately useable if it involves a categorical variable since the variable returned is how R names indicator variables.  You may have to manually fit the model based on the selected predictors.

If \code{HOLDOUT} is given a plot of the error on the holdout sample versus the number of predictors (for the best model at that number of predictors) is provided along with the estimated generalization error from the training set.  This can be used to see if the models generalize well, but is in general not used to tune which model is selected.
}

\references{
Introduction to Regression and Modeling with R
}

\author{
Adam Petrie
}

\seealso{
\code{\link{bestglm}}, \code{\link{regsubsets}}, \code{\link{see.models}}, \code{\link{generalization.error}}.
}

\examples{

  #Descriptive model.  Note: Tip and Bill should not be used simultaneously as 
  #predictors of TipPercentage, so leave Tip out since it's not known ahead of time
  data(TIPS)
  MODELS <- build.model(TipPercentage~.-Tip,data=TIPS,type="descriptive")
  MODELS$AICtable
  MODELS$predictors[[1]] #Variable in best model with a single predictors
  MODELS$predictors[[2]] #Variables in best model with two predictors
  summary(MODELS$bestmodel) #Summary of best model, in this case with two predictors

  #Another descriptive model (large dataset so changing prompt=FALSE for documentation)
  data(PURCHASE)
  set.seed(320)
  #Take a subset of full dataframe for quick illustration
  SUBSET <- PURCHASE[sample(nrow(PURCHASE),500),]
  MODELS <- build.model(Purchase~.,data=SUBSET,type="descriptive",prompt=FALSE)
  MODELS$AICtable  #Model with 1 or 2 variables look pretty good
  #Predict whether a purchase is made by # of previous visits and distance to store
	MODELS$predictors[[2]]  

  #Predictive model.  
  data(SALARY)
  set.seed(2010)
  train.rows <- sample(nrow(SALARY),0.7*nrow(SALARY),replace=TRUE)
  TRAIN <- SALARY[train.rows,]
  HOLDOUT <- SALARY[-train.rows,]
  MODELS <- build.model(Salary~.^2,data=TRAIN,holdout=HOLDOUT)
  summary(MODELS$bestmodel)
  M <- lm(Salary~Gender+Education:Months,data=TRAIN)
  generalization.error(M,HOLDOUT)
  
  #Predictive model for WINE data, takes a while.  Misclassification rate on holdout sample is 18\%.
  data(WINE)
  set.seed(2010)
  train.rows <- sample(nrow(WINE),0.7*nrow(WINE),replace=TRUE)
  TRAIN <- WINE[train.rows,]
  HOLDOUT <- WINE[-train.rows,]
  \dontrun{MODELS <- build.model(Quality~.,data=TRAIN,seed=1919,holdout=HOLDOUT)}
  \dontrun{MODELS$CVtable}
	 }

