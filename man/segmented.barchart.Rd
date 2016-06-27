\name{segmented.barchart}
\alias{segmented.barchart}

\title{
Segmented barchart
}
\description{
Produces a segmented barchart of the input variable, forcing it to be categorical if necessary}
\usage{
segmented.barchart(x)
}

\arguments{
  \item{x}{
A vector.  If numerical, it is treated as categorical variable in the form of a factor
}

}

\details{
Standard segmented barchart.  Shaded areas are labeled with the levels they represent, and the percentage of cases with that level is labeled on the axis to the right.
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\examples{
  data(STUDENT)
	segmented.barchart(STUDENT$Family)  #Categorical variable
	data(TIPS)
	segmented.barchart(TIPS$PartySize)  #Numerical variable treated as categorical

	 }


