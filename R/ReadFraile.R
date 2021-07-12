#' ReadFraile
#'
#' Reads the biogeochemical dataset from Igaraza Faile, Monthly data varname
#' e.g. bulloides or ruber... to get the list, leave it empty
#' @param FILENAME character, path to netcdf file
#' @param varname character, the variable to be read
#'
#' @return a \code{\link[pfields]{pField}} object
#' @export read_fraile ReadFraile
#' @aliases read_fraile ReadFraile
#' @source copied from paleolibrary/src/single_functions/speclib/read.fraile.R
#' and updated to use \code{ncdf4}
#' @author Thomas Laepple
#' @seealso \url{https://github.com/EarthSystemDiagnostics/pfields}
#' @examples
#' \dontrun{
#' ReadFrail("path/to/file.nc",
#'                   varname = "var.name")
#' }
ReadFraile <- function(FILENAME, varname){
  stop("Package 'ecustools' and this function are deprecated!")
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("package 'ncdf4' is needed for this function to work. Please install it.
         Linux users may have to install the 3rd party libraries libnetcdf-dev
         and libnetcdff-dev before installing ncdf4",
         call. = FALSE)
  }
  if (!requireNamespace("pfields", quietly = TRUE)) {
    stop(paste("package 'pfields' is needed for this function to work.",
               "Please install it."), call. = FALSE)
  }

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

  ncdf4::nc_close(temp.nc)

  return(pfields::pField(temp.data, temp.time, lat = temp.lat, lon = temp.lon,
                         name = varname, history = FILENAME))
}
read_fraile <- function(...){
  warning("read_fraile is deprecated and replaced with ReadFraile
          to comply with ECUS R style guide")
  ReadFraile(...)
}
