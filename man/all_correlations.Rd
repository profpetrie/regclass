\name{all_correlations}
\alias{all_correlations}
\alias{all.correlations}

\title{
Pairwise correlations between quantitative variables
}
\description{
This function gives a list of all pairwise correlations between quantitative variables in a dataframe.  Alternatively, it can provide all pairwise correlations with just a particular variable.}
\usage{
all_correlations(X,type="pearson",interest=NA,sorted="none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
A data frame
}
  \item{type}{
Either \code{pearson} or \code{spearman}.  If \code{pearson}, the Pearson correlations are returned.  If \code{spearman}, the Spearman's rank correlations are returned.
}
 \item{interest}{If specified, returns only pairwise correlations with this variable.  Argument should be in quotes and must give the exact name of the column of the variable of interest.}


\item{sorted}{Either \code{none}, \code{strength}, or \code{significance}.   If \code{strength}, sorts the list from most negative correlation to most positive (remember, correlations are stronger the farther they are from 0 (positive or negative).  If \code{significance}, sorts the list by p-value.  If \code{none}, no sorting takes place.}
}

\details{
This function filters out any non-numerical variables in the data frame and provides correlations only between quantitative variables.  It is useful for quickly glancing at the size of the correlations between many pairs of variables or all correlations with a particular variable.  Further analysis should be done on pairs of interest using \code{\link{associate}}.

Note:  if Spearmans' rank correlations are computed, warnings message result indicating that the exact p-value cannot be computed with ties.  Running \code{associate} will give you an approximate p-value using the permutation procedure.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}

\seealso{ \code{\link{cor}}, \code{\link{associate}} }

\examples{
	#all pairwise (Pearson) correlations between all quantitative variables
	data(STUDENT)
	all_correlations(STUDENT)  
	#Spearman correlations between all quantitative variables and CollegeGPA, sorted by pvalue. 
	#Gives warnings due to ties
	all_correlations(STUDENT,interest="CollegeGPA",type="spearman",sorted="significance")
	 }
