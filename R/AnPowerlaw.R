
##' @title return power spectral density vector for a power law with variance 1
##' @param beta 
##' @param freq frequency vector
##' @return power spectral density
##' @author Thomas Laepple
##' @export
AnPowerlaw<-function(beta,freq)
{
  #power law PSD. variance 1, slope beta
       power=1/(freq^beta);
       powerScale<-sum(power)*mean(diff(freq))*2
       return(power/powerScale)
}
