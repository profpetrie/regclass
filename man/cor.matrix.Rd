\name{cor.matrix}
\alias{cor.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Correlation Matrix
}
\description{
This function produces the matrix of correlations between all quantitative variables in a dataframe. }
\usage{
cor.matrix(X,type="pearson")
}
\arguments{
  \item{X}{
A data frame
}
  \item{type}{
Either \code{pearson} or \code{spearman}.  If \code{pearson}, the Pearson correlations are returned.  If \code{spearman}, the Spearman's rank correlations are returned.
}

}

\details{
This function filters out any non-numerical variables and provides correlations only between quantitative variables.  Best for datasets with only a few variables.  The correlation matrix is returned (with class \code{matrix}).
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\seealso{ \code{\link{cor}} }
\examples{
  data(TIPS)
	cor.matrix(TIPS)
	data(AUTO)
	cor.matrix(AUTO,type="spearman")
	 }

