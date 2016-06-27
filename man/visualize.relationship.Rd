\name{visualize.relationship}
\alias{visualize.relationship}

\title{
Visualizing the relationship between y and x in a partition model
}

\description{
Attempts to show how the relationship between y and x is being modeled in a partition or random forest model
}
\usage{
visualize.relationship(TREE,interest,on,smooth=TRUE,marginal=TRUE,nplots=5,
  seed=NA,pos="topright")
}

\arguments{
  \item{TREE}{
A partition or random forest model (though it works with many regression models as well)
}
  \item{interest}{
The name of the predictor variable for which the plot of y vs. x is to be made.
  }

  \item{on}{
  A dataframe giving the values of the other predictor variables for which the relationship is to be visualized.  Typically this is the dataframe on which the partition model was built.
  }
  \item{smooth}{
  If \code{TRUE}, the relationship is plotted using a \code{loess} to smooth out the relationship
  }
  \item{marginal}{
  If \code{TRUE}, the modeled value of y at a particular value of x is the average of the predicted values of y over all rows which have that common value of x.  If \code{FALSE}, then \code{nplots} rows from \code{on} will be selected and all other predictors will be fixed, showing the relationship between y and x for that particular set of characteristics.
  }

    \item{nplots}{
  The number of rows of \code{on} for which the relationship is plotted (if \code{marginal} is set to \code{FALSE})
  }
  \item{seed}{the seed for the random number seed if reproducibility is required}
  \item{pos}{the location of the legend}
}

\details{
The function shows a scatterplot of y vs. x in the \code{on} dataframe, then shows how \code{TREE} is modeling the relationship between y and x with predicted values of y for each row in the data and also a curve illustrating the relationship.  It is useful for seeing what the relationship between y and x as modeled by \code{TREE} "looks like", both as a whole and for particular combinations of other variables.  If \code{marginal} is \code{FALSE}, then differences  in the curves indicate the presence of some interaction between x and another variable.
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\seealso{ \code{\link{loess}}, \code{\link{lm}}, \code{\link{glm}}}
\examples{
  data(SALARY)
  FOREST <- randomForest(Salary~.,data=SALARY)
  visualize.relationship(FOREST,interest="Experience",on=SALARY)
  visualize.relationship(FOREST,interest="Months",marginal=FALSE,nplots=7,on=SALARY)

  data(WINE)
  TREE <- rpart(Quality~.,data=WINE)
  visualize.relationship(TREE,interest="alcohol",on=WINE,smooth=FALSE)
  visualize.relationship(TREE,interest="alcohol",on=WINE,marginal=FALSE,nplots=5,smooth=FALSE)
}

