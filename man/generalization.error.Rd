\name{generalization.error}
\alias{generalization.error}

\title{
Calculating the generalization error of a model on a set of data
}

\description{
This function takes a linear regression from \code{lm}, logistic regression from \code{glm}, partition model from \code{rpart}, or random forest from \code{randomForest} and calculates the generalization error on a dataframe.  }

\usage{
generalization.error(MODEL,HOLDOUT,Kfold=FALSE,K=5,R=10,seed=NA)
}

\arguments{
  \item{MODEL}{
A linear regression model created using \code{\link{lm}}, a logistic regression model created using \code{\link{glm}}, a partition model created using \code{\link{rpart}}, or a random forest created using \code{\link{randomForest}}.}
  \item{HOLDOUT}{
  A dataset for which the generalization error will be calculated.  If not given, the error on the data used to build the model (\code{MODEL}) is used.
  }
  \item{Kfold}{
  If \code{TRUE}, function will estimate the generalization error of \code{MODEL} using repeated K-fold cross validation (regression models only)
  }
  \item{K}{
  The number of folds used in repeated K-fold cross-validation for the estimation of the generalization error for the model \code{MODEL}.  It is useful to compare this number to the actual generalization error on \code{HOLDOUT}.
  }
  \item{R}{
  The number of repeats used in repeated K-fold cross-validation.
  }
  \item{seed}{an optional argument priming the random number seed for estimating the generalization error}

  

}

\details{
This function calculates the error on \code{MODEL}, its estimated generalization error from repeated K-fold cross-validation (for regression models only), and the actual generalization error on \code{HOLDOUT}.  If the response is quantitative, the RMSE is reported.  If the response is categorical, the confusion matrices and misclassification rates are returned.

}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\examples{

  #Education analytics
  data(STUDENT)
  set.seed(1010)
  train.rows <- sample(1:nrow(STUDENT),0.7*nrow(STUDENT))
  TRAIN <- STUDENT[train.rows,]
  HOLDOUT <- STUDENT[-train.rows,]
  M <- lm(CollegeGPA~.,data=TRAIN)
  #Also estimate the generalization error of the model
  generalization.error(M,HOLDOUT,Kfold=TRUE,seed=5020)
  #Try partition and randomforest, though they do not perform as well as regression here
  TREE <- rpart(CollegeGPA~.,data=TRAIN)
  FOREST <- randomForest(CollegeGPA~.,data=TRAIN)
  generalization.error(TREE,HOLDOUT)
  generalization.error(FOREST,HOLDOUT) 

  #Wine
  data(WINE)
  set.seed(2020)
  train.rows <- sample(1:nrow(WINE),0.7*nrow(WINE))
  TRAIN <- WINE[train.rows,]
  HOLDOUT <- WINE[-train.rows,]
  M <- glm(Quality~.^2,data=TRAIN,family=binomial)
  generalization.error(M,HOLDOUT)
  #Random forest predicts best on the holdout sample
  TREE <- rpart(Quality~.,data=TRAIN)
  FOREST <- randomForest(Quality~.,data=TRAIN)
  generalization.error(TREE,HOLDOUT)
  generalization.error(FOREST,HOLDOUT) 

}

