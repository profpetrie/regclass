\name{mosaic}
\alias{mosaic}

\title{
Mosaic plot
}

\description{
Provides a mosaic plot to visualize the association between two categorical variables }

\usage{
mosaic(formula,data,color=TRUE)
}

\arguments{

  \item{formula}{
A standard R formula written as y~x, where y is the name of the variable playing the role of y and x is the name of the variable playing the role of x.  
}
  \item{data}{
An optional argument giving the name of the data frame that contains x and y.  If not specified, the function will use existing definitions in the parent environment.
}

  \item{color}{
\code{TRUE} or \code{FALSE}.   If \code{FALSE}, plots are presented in greyscale.  If \code{TRUE}, an intelligent color scheme is chosen to shade the plot.  
}

}

\details{
This function shows a mosaic plot to visualize the conditional distributions of \code{y} for each level of \code{x}, along with the marginal distribution of \code{y} to the right of the plot.  The widths of the segmented bar charts are proportional to the frequency of each level of \code{x}.  These plots are the same that appear using \code{associate}.

}



\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\seealso{ \code{\link{associate}}}
\examples{
  data(ACCOUNT)
	mosaic(Area.Classification~Purchase,data=ACCOUNT,color=TRUE)
	 }
