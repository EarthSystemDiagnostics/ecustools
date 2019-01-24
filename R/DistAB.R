#' from clim.pact
#' @title Distance in km between two coordinates given by lat/lon
#' @param lon  Longitude Degree East (scalar)
#' @param lat  Latitude Degree North (scalar)
#' @param lons Longitude Degree East (scalar or vector)
#' @param lats Latitude Degree North (scalar or vector of same length than lons)
#' @param a radius of the er
#' @return distance in km
#' @author Rasmus E. Benestad (modified by Thomas Laepple)
#' @examples
#' #Distance in the tropics
#' DistAB(0,0,1,0)
#' DistAB(0,0,0,1)
#' #Distance in the polar regions around EDML core-site
#' DistAB(0,-75,1,-75)
#' DistAB(0,-75,0,-76)
#' DistAB(0,-75,c(0,1,2,3),c(-76,-75,-74,-73))
#' @export
DistAB <- function(lon,lat,lons,lats) {
  a=6.378e06
  good <- is.finite(lons) & is.finite(lats)
  lons <- lons[good]
  lats <- lats[good]
  if ( (length(lon) !=1) | (length(lat) !=1) |
       (length(lons)!=length(lats)) ) {
    print(paste("distAB [clim.pact]: length(lon)=",length(lon),
                "length(lat)=",length(lat),
                "length(lons)=",length(lons),"length(lats)=",length(lats)))
    print("length(lons) must equal length(lats) and lon and lat must have length=1")
    stop('Error in distAB - argument lengths do not match!')
  }
  theta <- pi*lon/180
  phi <- pi*lat/180
  dist <- rep(NA,length(lons))
  r1 <- c(cos(phi)*cos(theta),
          sin(phi),
          cos(phi)*sin(theta))
  dim(r1) <- c(3,length(lon))
  theta <- pi*lons/180
  phi <- pi*lats/180

  r2 <- cbind(cos(phi)*cos(theta),
              sin(phi),
              cos(phi)*sin(theta))

  angle <- acos( r2 %*% r1 )
  dist <- rep(NA,length(lons))
  dist[good] <- a* angle
  return(dist/1000)
}
