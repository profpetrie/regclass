\name{confusion.matrix}
\alias{confusion.matrix}

\title{
Confusion matrix for logistic regression models
}

\description{
This function takes the output of a logistic regression created with \code{glm} and returns the confusion matrix. 
}
\usage{
confusion.matrix(M,DATA=NA) 
}

\arguments{
  \item{M}{
A logistic regression model created with \code{\link{glm}}
}
  \item{DATA}{
A data frame on which the confusion matrix will be made.  If omitted, the confusion matrix is on the data used in \code{M}.  If specified, the data frame must have the same column names as the data used to build the model in \code{M}.
}

}

\details{
This function makes classifications on the data used to build a logistic regression model by predicting the "level of interest" (last alphabetically) when the predicted probability exceeds 50\%.  
}


\author{
Adam Petrie
}


\seealso{ \code{\link{glm}}}
\examples{

  #On WINE data as a whole
  data(WINE)
  M <- glm(Quality~.,data=WINE,family=binomial)
  confusion.matrix(M)
  
  #Calculate generalization error using training/holdout
  set.seed(1010)
  train.rows <- sample(nrow(WINE),0.7*nrow(WINE),replace=TRUE)
  TRAIN <- WINE[train.rows,]
  HOLDOUT <- WINE[-train.rows,]
  M <- glm(Quality~.,data=TRAIN,family=binomial)
	confusion.matrix(M,HOLDOUT)
	
	
	#Predicting donation
	#Model predicting from recent average gift amount is significant, but its
	#classifications are the same as the naive model (majority rules)
	data(DONOR)
	M.naive <- glm(Donate~1,data=DONOR,family=binomial)
	confusion.matrix(M.naive)
	M <- glm(Donate~RECENT_AVG_GIFT_AMT,data=DONOR,family=binomial)
	confusion.matrix(M)
	
	 }

