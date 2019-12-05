#' Minimize difference between coordinates
#'
#' This function finds the position in a latitude/longitude field that is
#' closest to a given target position by minimizing the root mean square
#' deviation between their position vectors in spherical coordinates.
#' @param lat0 a length-1 vector with the latitude of the target position.
#' @param lon0 a length-1 vector with the longitude of the target position.
#' @param lat numeric vector of latitudes from a field of coordinates for which
#' the point closest to the target is to be determined.
#' @param lon numeric vector of longitudes from a field of coordinates for
#' which the point closest to the target is to be determined. Must have the
#' same length as \code{lat}.
#' @param return.coordinates logical; shall the actual coordinates of the
#' position closest to the target be returned? Defaults to \code{FALSE}, which
#' returns just the vector index of the closest position.
#' @return per default, the index position in \code{lat} (or \code{lon}) of the
#' position closest to the target; or the actual coordinates of this position
#' as a length-2 numeric vector (for \code{return.coordinates = TRUE}).
#' @author Thomas Münch
#' @examples
#' # some coordinates
#' lat0 <- -75
#' lon0 <- 0
#' lat <- seq(-74, -80, -2)
#' lon <- seq(-2, 7, 3)
#'
#' # get closest point to lat0/lon0 in degree
#' MinimizeSpherical(lat0, lon0, lat, lon,
#'                   return.coordinates = TRUE)
#' @export
MinimizeSpherical <- function(lat0, lon0, lat, lon,
                              return.coordinates = FALSE) {

    # Error checking

    if (length(lat0) != length(lon0))
        stop("'lat0' and 'lon0' must both have length 1.")

    if (length(lat) != length(lon))
        stop("'lat' and 'lon' must have equal length.")

    n <- length(lat)

    # Convert angles from degree to radian

    x <- deg2rad(lat0)
    y <- deg2rad(lon0)

    xx <- deg2rad(lat)
    yy <- deg2rad(lon)
    
    # Convert input to vectors in spherical coordinates

    target <- c(cos(x) * cos(y),
                cos(x) * sin(y),
                sin(x))

    field.vectors <- rbind(cos(xx) * cos(yy),
                           cos(xx) * sin(yy),
                           sin(xx))

    # Root mean square deviation between target and data position vectors

    dev <- rep(NA, n)
    for (i in 1 : n) {

        dev[i] <- rmsd(field.vectors[, i], target)
    }

    # Return index of the position vector with minimum deviation to target

    i <- which.min(dev)

    if (return.coordinates) {
        res <- c(lat = lat[i], lon = lon[i])
    } else {
        res <- i
    }

    return(res)
}
