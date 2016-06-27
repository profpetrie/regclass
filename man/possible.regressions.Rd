\name{possible.regressions}
\alias{possible.regressions}

\title{
Illustrating how a simple linear/logistic regression could have turned out via permutations
}

\description{
This function gives a demonstration of what simple linear or logistic regression lines could have looked like "by chance" if x and y were unrelated.  A scatterplot and fitted regression line is displayed along with the regression lines produced when x and y are unrelated via the permutation procedure.  The sum of squared error reductions for all lines (for linear regressions) are also displayed for an informal assessement of significance.

}

\usage{
possible.regressions(M,permutations=100,sse=TRUE,reduction=TRUE)
}

\arguments{
  \item{M}{
A simple linear regression model from \code{lm}
}
  \item{permutations}{
The number of artificial samples generated with the permutation procedure to consider (each will have y and x be independent by design).
}
 \item{sse}{Optional argument to either show or hide the histogram of sum of squared errors of the regression lines.}
 
  \item{reduction}{Optional argument that, if \code{sse} is \code{TRUE}, shows the reduction in the sum of squared errors or the raw sum of squared errors of the regressions themselves.}

}

\details{
This function gives a scatterplot and fitted regression line for \code{M} in red for a linear regression, or the fitted logistic curve (in black) for logistic regression.  Then, via the permutation procedure, it generates \code{permutations}, artificial samples where the observed values of x and y are paired up at random, ensuring that no relationship exists between them.  A regression is fit on this permutation sample, and the regression line is drawn in grey to illustrate how it may look "by chance" when x and y are unrelated.

If requested, a histogram of the sum of squared error reductions of each of the regressions on the permutation datasets (and the original regression in red) is displayed to allow for an informal assessement of the statistical significance of the regression.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}


\examples{
  
  #A weak but statistically significant relationship
  data(TIPS)
  M <- lm(TipPercentage~Bill,data=TIPS)
  possible.regressions(M)
  
  #A very strong relationship
  data(SURVEY10)
  M <- lm(PercMoreIntelligentThan~PercMoreAttractiveThan,data=SURVEY10)
  possible.regressions(M,permutations=1000)

  #Show raw SSE instead of reductions
  M <- lm(TipPercentage~PartySize,data=TIPS)
  possible.regressions(M,reduction=FALSE)
	 }
