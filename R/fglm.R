fglm <-
function(y, x, family = gaussian, N, m = N )
{
    if(m > N) return("m has to be not greater than N !")
    temp.pcaa <- pca.fd(x,N)
    lgr <- glm(y~temp.pcaa$scores, family=family)
    intercept <- lgr$coeff[1]
    temp.bcoef <- (temp.pcaa$harmonics$coefs[,1:m])%*%lgr$coeff[2:(m+1)]
    temp.bbasis <- temp.pcaa$harmonics$basis
    b <- fd(temp.bcoef, basisobj=temp.bbasis)
    #summaries <- lgr
    z <- list(intercept, b)
    names(z) <- c("intercept","b")
    return(z)
}

