\name{fglm}
\alias{fglm}
\title{Estimation for Functional Generalized Linear Models}
\description{
The function implements the estimation procedure for the slope function described in Dou etal.(2009). The estimation procedure is based on functional principal components analysis. 
}
\usage{
fglm(y,x, family = gaussian, N,m = N)
}
\arguments{
  \item{y}{a numeric vector.}
  \item{x}{a vector of functional data objects.}
  \item{family}{a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. (See family for details of \code{\link{family}} functions.)}
  \item{N}{An integer. }
  \item{m}{An integer.}
}
\details{  
 y contains responses and x contains covariate functions. Integer N is the cut-off points corresponding to change of measure or asymptotic equivalence. Integer m is the cut-off points in order to minimize the error in bias-variance trade-off. 
 }
\value{

It is a LIST consists of 
\item{b}{A functional data object which is the estimated slope function. }
\item{intercept}{intercept term.}

}
\references{
Dou, W., Pollard, D. and Zhou, H.H. (2009). Functional Regression for General Exponential Families \emph{manuscripts}.
}
\note{
The estimation procedure was studied in details in Dou etal.(2009) and the authorts showed that the estimation is minimax optimal under certain mild assumptions. 
}
\author{Wei Dou
\email{wei.dou@yale.edu}
}


\examples{

### Simulate Gaussian processes ###

fr <- 31       #terms in the fourier expansion#

basisobj.bx <- create.fourier.basis(rangeval=c(0, 1), 2*fr-1)

samplesize <- 1000

xx <-matrix(rnorm(samplesize*fr), samplesize, fr)

xcoef <- matrix(0, samplesize, 2*fr-1)

alpha <- 1.5

for(j in 1:fr){

    xcoef[,2*j-1] <- xx[,j]*(j^(-alpha/2))

}

#- get functional object -#
         
x.fd <- fd(t(xcoef),basisobj.bx)


### True slope function ###

  
b.coef <- rep(0, 2*fr-1)

beta <- 3

for(i.fr in 1:fr){

     b.coef[2*i.fr-1] <- ((-1)^(i.fr))*((i.fr)^(-beta))
}   

b <- fd(b.coef, basisobj.bx)


### Generate Bernoulli responses according functional logistic model ###

y <- rep(0,samplesize)

int <- xcoef%*%b.coef

p <- exp(int)/(1+exp(int))

for(ii in 1: samplesize){

     y[ii] <- sample(c(0,1),1,prob=c(1-p[ii],p[ii]))

}

### Estimate the slope function ###

fg <- fglm(y,x.fd,family=binomial(link = "logit"), N=15,m=2)

b.hat <- fg$b

### plot the result ###

plot.fd(b.hat)
grids <- seq(0,1,by=0.01)
points(grids,eval.fd(grids,b), type="l")

}
\keyword{nonparametric}

