##' as used in Huybers&Laepple EPSL 2014
##' to get a robust estimate of the breakpoint, the intercept with a high freq. powerlaw is used
##' @title Simulate a timeseries with length N which has a spectra consisting of the sample spectrum
##' and a powerlaw for the lower frequencies
##' @param spec 
##' @param beta1 slope for frequencies lower than breakpoint
##' @param beta2 slope for frequencies higher than the breakpoint (only used to determine the breakpoint)
##' @param N number of years
##' @param breakpoint frequency in 1/yr of the breakpoint 
##' @return 
##' @author Thomas Laepple 
SimPowerlawCombinedObserved<-function(spec,beta1,beta2,N,breakpoint=1/50)
{

    Norg<-N
    N<-ceiling(N/2)*2
    df  = 1/(N);
    f=seq(from=df,to=1/(2),by=df)
    index.breakpoint<-closest.element(f,breakpoint)

    #Intercept to cross at breakpoint
    a<- (-1*beta1*log(f[index.breakpoint]))-(-1*beta2*log(f[index.breakpoint]))
    FilterLow <-  exp((-1)*log(f[1:index.breakpoint])*beta1-a)
    FilterHigh <- exp((-1)*log(f[-1*(1:index.breakpoint)])*beta2)
    a<-approx(spec$freq,spec$spec,f[-1*(1:index.breakpoint)],rule=2)$y
    FilterHigh<-a/sum(a)*sum(FilterHigh)

    Filter<-c(sqrt(FilterLow),sqrt(FilterHigh))
    Filter = c(max(Filter), Filter,rev(Filter))

    x   = scale(rnorm(N+1,1))
    fx  =fft(x)
    ffx =fx*Filter;
    result<-scale(Re(fft(ffx,inverse=TRUE)))
    return(result[1:Norg])
}


