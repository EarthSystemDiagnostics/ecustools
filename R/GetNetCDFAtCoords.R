#' Extract timeseries for nearest coordinates from a netcdf file
#'
#' @param filename
#' @param req.coords
#' @param req.var
#' @param time.var
#'
#' @return
#' @export
#'
#' @examples
GetNetCDFAtCoords <- function(filename, req.coords, req.var, time.var = "time"){

  nc1 <- ncdf4::nc_open(filename, readunlim = FALSE)
  print(nc1)

  lats <- ncdf4::ncvar_get(nc1, varid = "latitude")
  lons <- ncdf4::ncvar_get(nc1, varid = "longitude")
  lons <- ifelse(lons > 180, lons - 360, lons)

  nc.coords <- expand.grid(lons = lons, lats = lats)

  ind <- sapply(1:nrow(req.coords), function(x) {
    which.min(geosphere::distHaversine(req.coords[x, ], nc.coords))
  })

  print(ind)

  req.nc.coords <- nc.coords[ind,]

  lon.inds.to.get <- sapply(req.nc.coords[,1], function(x) which(lons == x))
  lat.inds.to.get <- sapply(req.nc.coords[,2], function(x) which(lats == x))

  coord.inds.to.get <- cbind(lon.inds.to.get, lat.inds.to.get)

  system.time(
    dat.out <- plyr::aaply(coord.inds.to.get[,], 1, function(x) {
      ncdf4::ncvar_get(
        nc1,
        varid = req.var,
        start = c(x[1], x[2], 1),
        count = c(1, 1, -1)
      )},
      .progress = "text")
  )

  dat.out <- tibble::as_tibble(t(dat.out))

  dates <- ncdf4::ncvar_get(nc1, varid = "time", start = c(1), count = c(-1))

  ncdf4::nc_close(nc1)

  dat.out$date <- dates

  req.coords$location <- as.character(1:nrow(req.coords))

  dat.out <- tidyr::gather(dat.out, location, UQ(req.var), -date)
  dat.out <- dplyr::left_join(req.coords, dat.out)

  return(dat.out)
}
