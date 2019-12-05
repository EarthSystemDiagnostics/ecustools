#' Circle coordinates on a sphere
#'
#' Get the latitude and longitude coordinates of a circle with a given radius
#' drawn onto the surface of a sphere.
#'
#' @param lat0 geographical latitude [degree] of circle centre.
#' @param lon0 geographical longitude [degree] of circle centre.
#' @param radius.circle radius of the circle; in the same units as
#' \code{radius.sphere}.
#' @param radius.sphere radius of the sphere on whose surface the circle is
#' drawn; i.e. typically Earth's radius. Default value is Earth's polar radius
#' which gives slightly more accurate results for drawing circles close to
#' either one of the poles. Adjust to equatorial radius for drawing circles in
#' the mid-latitudes.
#' @param n number of polar angle increments to draw the circle.
#' @param return.pi.interval if \code{TRUE}, return the circle's longitudes in
#' the interval [-pi, pi]; else in [0, 2*pi].
#' @return data frame of circle latitude (\code{lat}) and longitudes
#' (\code{lon}) in geographical coordinates in degree for the number of
#' incremental steps specified by \code{n} (given as the polar coordinate
#' \code{phi}).
#' @author Thomas Münch
#' @export
CircleCoordinates <- function(lat0, lon0, radius.circle,
                              radius.sphere = 6357, n = 100,
                              return.pi.interval = FALSE) {

  # Put input longitude in [-pi, pi]
  if (lon0 >= 180) lon0 <- lon0 - 360

  # Convert angles from degree to radian
  lat0 <- deg2rad(lat0)
  lon0 <- deg2rad(lon0)
  
  # Distance from centre of sphere to any point on circle
  r <- sqrt(radius.sphere^2 + radius.circle^2)

  # Incremental angles at which to draw the circle
  phi <- seq(0, 2 * pi, 2 * pi / n)

  # Define auxiliary variables

  cos.lat <- cos(lat0)
  sin.lat <- sin(lat0)

  cos.lon <- cos(lon0)
  sin.lon <- sin(lon0)

  cos.phi <- cos(phi)
  sin.phi <- sin(phi)

  aux1 <- radius.sphere * cos.lat - radius.circle * cos.phi * sin.lat
  aux2 <- radius.circle * sin.phi
  aux3 <- radius.sphere * sin.lat
  aux4 <- radius.circle * cos.phi

  
  # Euclidean circle coordinates

  x <- cos.lon * aux1 - sin.lon * aux2
  y <- sin.lon * aux1 + cos.lon * aux2
  z <- aux3 + cos.lat * aux4

  # Geographic latitude and longitude of these points

  lat <- asin(z / r)
  lon <- atan2(y, x)

  # Convert to degrees, longitude in [0, 2*pi]?, and output

  phi <- deg2rad(phi, inverse = TRUE)
  lat <- deg2rad(lat, inverse = TRUE)
  lon <- deg2rad(lon, inverse = TRUE)

  if (!return.pi.interval) {
    lon[lon < 0] <- lon[lon < 0] + 360
  }

  res <- as.data.frame(list(phi = phi, lat = lat, lon = lon))

  return(res)

}
