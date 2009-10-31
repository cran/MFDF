\name{usrecession}
\alias{usrecession}
\docType{data}
\title{
Records of U.S.economic recessions from 1953 to 2009
}
\description{
 The National Bureau of Economic Research (NBER) provides the
most widely accepted definition of a recession (NBER 2008): \emph{A recession is a significant decline in economic activity spread across the economy, lasting more than a few months, normally visible in production, employment, real income, and other indicators. A recession
begins when the economy reaches a peak of activity and ends when the economy reaches its trough. Between trough and peak, the economy is in an expansion.} The latest dating result of U.S. recessions can be found at the official web site of NBER (http://www.nber.org/cycles.html)
}
\usage{data(usrecession)}
\format{
  A data frame with 682 observations on the following 3 variables.
    \code{time index} is a numeric vector. 
    \code{flag} is a numeric vector.
    \code{time} is a factor contains characters specifying the month and year.
}




\source{
Official web site of NBER (http://www.nber.org/cycles.html)
}
\references{
Dou, W., Pollard, D and Zhou, H.H. (2009) Functional Regression for General Exponential Families. \emph{manuscripts}.
}
\examples{
data(usrecession)

}
\keyword{datasets}
