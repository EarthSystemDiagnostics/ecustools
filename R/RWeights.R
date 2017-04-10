##' @title  Draw random number between 1 and length(weights) with the distribution
##' @param n Number of random numbers to be drawn
##' @param weights vector of numbers; defines the distribution
##' @return vector of n random numbers between 1 and length(weights)
##' @author Thomas Laepple
##' @examples
##' weights <- c(rep(1,10),1:10,10:1)
##' hist(RWeights(100000,weights),breaks=501)
##' @export
RWeights<-function(n=1000,weights)
{

    dpv <- new("unuran.discr", pv=weights, lb=1)
    gen <- Runuran::unuran.new(dpv)
    return(Runuran::ur(gen,n))
}
