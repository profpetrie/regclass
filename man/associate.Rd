\name{associate}
\alias{associate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Association Analysis
}
\description{
This function takes two quantities and computes relevent numerical measures of association.  The p-values of the associations are estimated via permutation tests.  Plots for diagnostics are provided as well, with optional arguments that allow for classic tests. }
\usage{
associate(formula, data, permutations = 500, seed=NA, plot = TRUE, classic = FALSE, 
  cex.leg=0.7, n.levels=NA,prompt=TRUE,color=TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
A standard R formula written as y~x, where y is the name of the variable playing the role of y and x is the name of the variable playing the role of x.  
}
  \item{data}{
An optional argument giving the name of the data frame that contains x and y.  If not specified, the function will use existing definitions in the parent environment.
}
  \item{permutations}{
The number of permutations for Monte Carlo estimation of the p-value.  If 0, function defaults to reporting classic results.
}
  \item{seed}{
An optional argument specifying the random number seed for permutations. 
}
  \item{plot}{
\code{TRUE} or \code{FALSE}.  Indicates whether the relevent plots are displayed. 
}
  \item{classic}{
\code{TRUE} or \code{FALSE}.  Indicates whether p-values should (also) be found using classic approximations.
}
  \item{cex.leg}{
Scale factor for the size of legends in plots.  Larger values make legends bigger.
}
  \item{n.levels}{
An optional argument of interest only when y is categorical and x is quantitative.  It specifies the number of levels when converting x to a categorical variable during the analysis.  Each level will have the same number of cases.  If this does not work out evenly, some levels are randomly picked to have one more case than the others.  If unspecified, the default is to pick the number of levels so that there are 10 cases per level or a maximum of 6 levels (whichever is smaller).

}
  \item{prompt}{
\code{TRUE} or \code{FALSE}.  If \code{FALSE}, function proceeds without prompting user when the number of observations or number of permutation is large (5000 threshold for each for a prompt).  Usually only run with \code{FALSE} for documentation purposes.
}

  \item{color}{
\code{TRUE} or \code{FALSE}.  Mostly used for mosaic plots.  If \code{FALSE}, plots are presented in greyscale.  If \code{TRUE}, an intelligent color scheme is chosen to shade the plot.  
}

  \item{\dots}{
Additional arguments related to plotting, e.g., pch, lty, lwd
}
}
\details{
This function uses Monte Carlo simulation (permutation procedure) to approximate the p-value of an association.   Only complete cases are considered in the analysis.

Valid formulas may include functions of the variable, e.g. y^2, log10(x), or more complicated functions like I(x1/(x2+x3)).  In the latter case, I() must surround the function of interest to be computed correctly.

When both x and y are quantitative variables, an analysis of Pearson's correlation and Spearman's rank correlation is provided. Scatterplots and histograms of the variables are provided.  If \code{classic} is \code{TRUE}, the QQ-plots of the variables are provided along with tests of assumptions.

When x is categorical and y is quantitative, the averages (as well as mean ranks and medians) of y are compared between levels of x.  The "discrepancy" is the F statistic for averages, Kruskal-Wallis statistic for mean ranks, and the chi-squared statistic for the median test.  Side-by-side boxplots are also provided.   If \code{classic} is \code{TRUE}, the QQ-plots of the distribution of y for each level of x are provided.

When x is quantitative and y is categorical, x is converted to a categorical variable with \code{n.levels} levels with equal numbers of cases.  A chi-squared test is performed for the association.  The classic approach assumes a multinomial logistic regression to check significance.  A mosaic plot showing the distribution of y for each induced level of x is provided as well as a probability "curve".  If \code{classic} is \code{TRUE}, the multinomial logistic curves for each level are provided versus x..

When both x and y are categorical, a chi-squared test is performed.  The contingency table, table of expected counts, and conditional distributions are also reported along with a mosaic plot.

If the permutation procedure is used, the sampling distribution of the measure of association is displayed over the requested amount of permutations along with the observed value on the actual data (except when y is categorical with x quantitative).


If classic results are desired, then plots and tests to check assumptions are supplied.  \code{white.test} from package bstats (version 1.1-11-5) and  \code{mshapiro.test} from package mvnormtest (version 0.1-9) are built into the function to avoid directly referencing the libraries (which sometimes causes problems). 




}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lm}}, \code{\link{glm}}, \code{\link{anova}}, \code{\link{cor}}, \code{\link{chisq.test}}, \code{\link{vglm}} }
\examples{
  #Two quantitative variables
  data(SALARY)
	associate(Salary~Education,data=SALARY,permutations=1000)
	
	#y is quantitative while x is categorical
	data(SURVEY11)
	associate(X07.GPA~X40.FavAlcohol,data=SURVEY11,permutations=0,classic=TRUE)
	
	#y is categorical while x is quantitative
	data(WINE)
	associate(Quality~alcohol,data=WINE,classic=TRUE,n.levels=5) 

  #Two categorical variables (many cases, turns off prompt asking for user input)
  data(ACCOUNT)
  set.seed(320)
  #Work with a smaller subset
  SUBSET <- ACCOUNT[sample(nrow(ACCOUNT),1000),]
	associate(Purchase~Area.Classification,data=SUBSET,classic=TRUE,prompt=FALSE)
	
	 }


