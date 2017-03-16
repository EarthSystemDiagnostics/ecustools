##' as used in Huybers&Laepple EPSL 2014
##' @title  simulate a stochastic climate following a given spectrum spec until the breakpoint frequency and a powerlaw for lower frequencies
##' @param spec 
##' @param beta.low  powerlaw slope on timescales >1/breakpoint 
##' @param NYear number of years to simulate
##' @param breakpoint frequency of breakpoint (1/yr)
##' @return timeseries
##' @author Thomas Laepple
##' 
SimulateClimateAnnual<-function(spec,beta.low=0,NYear=10000,breakpoint=1/50)
{
##get new timebase
newTime <- seq(from=0,to=NYear,by=1)

## why do we only fit until freq.end=2?
beta.high <- -1*SlopeFit(spec,breakpoint,freq.end=2,bDebug=FALSE)$slope
    
##Get the variance from the spectral estimate
index.decade<-closest.element(spec$freq,breakpoint)

freq.save<-spec$freq[index.decade]
var.observed<-sum(spec$spec[(-1)*(1:index.decade)])/length(spec$freq)

##simulate the background spectra
sim.raw<-ts(SimPowerlawCombinedObserved(spec,beta.low,beta.high,
length(newTime),breakpoint=breakpoint,freq=1),freq=1,start=0)
    
##spectral estimate...
spec.estimate<-SpecMTM(sim.raw)

##search the index for the variability which is used to match the
##variances
    index.decade<-closest.element(spec.estimate$freq,freq.save)
    
##Get the variance from the spectral estimate
var.powerlaw<-sum(spec.estimate$spec[(-1)*(1:index.decade)])/length(spec.estimate$freq)

##Rescale factor to match the observed variance
rescaleFactor<- 1/sqrt(var.powerlaw)*sqrt(var.observed)
sim.scaled<-sim.raw*rescaleFactor

return(sim.scaled)

}

SimulateClimateAnnual(SpecMTM(rnorm(100)))
