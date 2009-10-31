\name{usyields}
\alias{usyields}
\docType{data}
\title{
 U.S. treasury yields from 1953 to 2009
}
\description{
 This data set contains U.S. treasury yields from 1953 to 2009. This data set combines a data set obtained from Wharton Research Data Services (http://wrds.wharton.upenn.edu/) and the official Federal Reserve web site (http://www.federalreserve.gov/econresdata/researchdata.html)
}
\usage{data(usyields)}
\format{
  A data frame with 673 observations on the following 34 variables.
  \describe{
    \item{\code{time|maturity}}{a numeric vector}
    \item{\code{0.08}}{a numeric vector}
    \item{\code{0.25}}{a numeric vector}
    \item{\code{0.5}}{a numeric vector}
    \item{\code{1}}{a numeric vector}
    \item{\code{2}}{a numeric vector}
    \item{\code{3}}{a numeric vector}
    \item{\code{4}}{a numeric vector}
    \item{\code{5}}{a numeric vector}
    \item{\code{6}}{a numeric vector}
    \item{\code{7}}{a numeric vector}
    \item{\code{8}}{a numeric vector}
    \item{\code{9}}{a numeric vector}
    \item{\code{10}}{a numeric vector}
    \item{\code{11}}{a numeric vector}
    \item{\code{12}}{a numeric vector}
    \item{\code{13}}{a numeric vector}
    \item{\code{14}}{a numeric vector}
    \item{\code{15}}{a numeric vector}
    \item{\code{16}}{a numeric vector}
    \item{\code{17}}{a numeric vector}
    \item{\code{18}}{a numeric vector}
    \item{\code{19}}{a numeric vector}
    \item{\code{20}}{a numeric vector}
    \item{\code{21}}{a numeric vector}
    \item{\code{22}}{a numeric vector}
    \item{\code{23}}{a numeric vector}
    \item{\code{24}}{a numeric vector}
    \item{\code{25}}{a numeric vector}
    \item{\code{26}}{a numeric vector}
    \item{\code{27}}{a numeric vector}
    \item{\code{28}}{a numeric vector}
    \item{\code{29}}{a numeric vector}
    \item{\code{30}}{a numeric vector}
  }
}
\details{
 \code{time index} is the time index, for example, April 1953 is indexed as 1953.25. 
        \code{yields} is the yields for various maturities.
        \code{time} is the month and year during which the yield curve is observed. 
}
\source{
 Official Fed web site (http://www.federalreserve.gov/econresdata/researchdata.html) and Wharton Research Data Services (http://wrds.wharton.upenn.edu
)
}
\references{
  Dou, W., Pollard, D and Zhou, H.H. (2009) Functional Regression for General Exponential Families. \emph{manuscripts}.
       G\''{U} RKAYNAK, R. S., SACK, B. and WRIGHT, J. H. (2006). The u.s. treasury yield curve: 1961 to the present. \emph{Tech. rep}.
}
\examples{
data(usyields)

}
\keyword{datasets}
