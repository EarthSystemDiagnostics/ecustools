##' @title timelag with rolling over at 365 days
##' @param data vector of values at 365 days
##' @param ilag lag in days
##' @return lagged values
##' @author Thomas Laepple
TLag<-function(data,ilag)
{
	temp<-rep(data,3)
	return(temp[366:730-ilag])
}

# TLag2<-function(data, ilag)
# {
#   i <- 1 + (1:365 -1 - ilag) %% 365
#   return(data[i])
# }
#
# TLag(1:365, 10)
# TLag2(1:365, 10)
#
# microbenchmark::microbenchmark(TLag(1:365, 10),
#                                TLag2(1:365, 10),
#                                times = 10000L)

