\name{getcp}
\alias{getcp}

\title{
Complexity Parameter table for partition models
}

\description{
A simple function to take the output of a partition model created with \code{rpart} and return information abouthe complexity parameter and performance of varies models. }

\usage{
getcp(TREE)
}

\arguments{
  \item{TREE}{
An object of class \code{rpart}.  This is created by making a partition model using \code{rpart}.    
}

}

\details{
This function prints out a table of the complexity parameter, number of splits, relative error, cross validation error, and standard deviation of cross validation error for a partition model.  It adds helpful advice for what the value of CP is for the tree that had the lowest cross validation error and also the value of CP for the simplest tree with a cross validation error at most 1 standard deviation above the lowest. 

Further, a plot is made of the estimated generalization error (\code{xerror}) versus the number of splits to illustrate when the tree stops improving.  Vertical lines are draw at the number of splits corresponding to the lowest estimated generalization error to the tree selected by the one standard deviation rule.
}



\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\seealso{ \code{\link{rpart}}}
\examples{
  data(JUNK)
	TREE <- rpart(Junk~.,data=JUNK,control=rpart.control(cp=0,xval=10,minbucket=5))
	getcp(TREE)
	 }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
