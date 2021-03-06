% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DistAB.R
\name{DistAB}
\alias{DistAB}
\title{Distance in km between two coordinates given by lat/lon}
\usage{
DistAB(lon, lat, lons, lats)
}
\arguments{
\item{lon}{Longitude Degree East (scalar)}

\item{lat}{Latitude Degree North (scalar)}

\item{lons}{Longitude Degree East (scalar or vector)}

\item{lats}{Latitude Degree North (scalar or vector of same length than lons)}

\item{a}{radius of the er}
}
\value{
distance in km
}
\description{
from clim.pact
}
\examples{
#Distance in the tropics
DistAB(0,0,1,0)
DistAB(0,0,0,1)
#Distance in the polar regions around EDML core-site
DistAB(0,-75,1,-75)
DistAB(0,-75,0,-76)
DistAB(0,-75,c(0,1,2,3),c(-76,-75,-74,-73))
}
\author{
Rasmus E. Benestad (modified by Thomas Laepple)
}
