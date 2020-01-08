#' Find distance between points on Earth
#'
#' This is a general function to calculate the distance on Earth between a
#' target location and all points in a field of coordinates. It provides a
#' straighforward interface to the function \code{\link[geosphere]{distGeo}}
#' from package \code{geosphere}.
#'
#' This is a wrapper function whose purpose it is to provide a straighforward
#' interface to the function \code{\link[geosphere]{distGeo}}; this includes:
#' \itemize{
#' \item direct input of latitude/longitude values which are then shaped into
#'       the appropriate structure for \code{\link[geosphere]{distGeo}};
#' \item longitude values are automatically confined to [-180, 180] as needed by
#'       \code{\link[geosphere]{distGeo}};
#' \item if required, one can select the point closest to the target location as
#'       the reference for the distance calculations.}
#' @param lat0 numeric vector of length 1 with the latitude (in degree) of the
#' target location relative to which distances are to be calculated.
#' @param lon0 numeric vector of length 1 with the longitude (in degree) of the
#' target location.
#' @param lat numeric vector of latitudes (in degree) of the positions for
#' which distances from the target position are to be calculated.
#' @param lon numeric vector of longitudes (in degree) of the positions for
#' which distances from the target position are to be calculated. Must have the
#' same length as \code{lat}.
#' @param get.nearest logical; specify whether the given 'lat0' - 'lon0' pair
#' shall be used _as is_ as the target location (\code{FALSE}), or instead the
#' nearest point of the positions specified by 'lat' and 'lon' (\code{TRUE}).
#' Selection of the nearest point is currently implemented with
#' \code{\link[pfields]{MinimizeSpherical}}, for which the \code{"pfields"}
#' package needs to be installed.
#' @param verbose logical; if \code{TRUE} an informative message regarding the
#' selected target location is printed.
#' @return Numeric vector of the same length as \code{lat} with the distances
#' in km from the target location.
#' @author Thomas Münch
#' @seealso \code{\link[pfields]{MinimizeSpherical}};
#' \code{\link[pfields]{GetDistanceField}};
#' \url{https://github.com/EarthSystemDiagnostics/pfields}
#' @examples
#' # some coordinates
#' lat0 <- -75
#' lon0 <- 0
#' lat <- seq(-74, -80, -2)
#' lon <- seq(-2, 7, 3)
#'
#' d <- GetDistance(lat0, lon0, lat, lon, verbose = TRUE)
#' range(d)
#'
#' d <- GetDistance(lat0, lon0, lat, lon, get.nearest = TRUE, verbose = TRUE)
#' range(d)
#' @export
GetDistance <- function(lat0, lon0, lat, lon,
                        get.nearest = FALSE, verbose = FALSE) {

  # Error checking

  if (length(lat0) != length(lon0))
    stop("'lat0' and 'lon0' must both have length 1.")

  if (length(lat) != length(lon))
    stop("'lat' and 'lon' must have equal length.")

  # Select target point based on input

  if (get.nearest) {

    if (!requireNamespace("pfields", quietly = TRUE)) {
      stop(paste("Please install package 'pfields' to use this functionality."),
           call. = FALSE)
    }

    # get lat, lon of nearest point relative to target
    point <- rev(pfields::MinimizeSpherical(lat0, lon0, lat, lon,
                                            return.coordinates = TRUE))
    
    if (verbose) {
      message(sprintf(
        "Nearest grid point used as target: lat0 = %f, lon0 = %f.",
        point[2], point[1]))
    }

  } else {

    if (verbose) message("Using supplied 'lat0' and 'lon0' value as target.")
    point <- c(lon0, lat0)
  }

  # Shape coordinates of field in appropriate structure for 'distGeo' function

  field.coord <- rbind(lon, lat)

  # Need to put longitudes in [-180, 180] for 'distGeo' function

  point[point >= 180] <- point[point >= 180] - 360
  field.coord[field.coord >= 180] <- field.coord[field.coord >= 180] - 360

  # Get distance field
  distance <- apply(field.coord, 2, geosphere::distGeo, p2 = point)

  # Convert to km
  distance <- distance / 10^3

  return(distance)
}
