#' ReadFraile
#'
#' Reads the biogeochemical dataset from Igaraza Faile, Monthly data varname
#' e.g. bulloides or ruber... to get the list, leave it empty
#' @param FILENAME character, path to netcdf file
#' @param varname character, the variable to be read
#'
#' @return a \code{\link{pField}} object
#' @export
#' @author Thomas Laepple
#' @examples
ReadFraile <- function(FILENAME, varname){

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("package 'ncdf4' is needed for this function to work. Please install it.",
         call. = FALSE)}

  temp.nc <- ncdf4::nc_open(FILENAME)

  #Read out the data
  temp.time <- ncdf4::ncvar_get(temp.nc, "Time")
  temp.data <- ncdf4::ncvar_get(temp.nc, varname)
  temp.lat <- ncdf4::ncvar_get(temp.nc, "Latitude")
  temp.lon <- ncdf4::ncvar_get(temp.nc, "Longitude")

  #Sort the latitudes
  tmp <- sort(temp.lat, index.return = TRUE)
  temp.lat <- temp.lat[tmp$ix]
  temp.data <- temp.data[, tmp$ix, ]

  tmp <- sort(temp.lon, index.return = TRUE)
  temp.lon <- temp.lon[tmp$ix]
  temp.data <- temp.data[tmp$ix, , ]

  nc_close(temp.nc)

  return(pField(temp.data, temp.time, lat = temp.lat, lon = temp.lon,
                name = varname, history = FILENAME))
}
