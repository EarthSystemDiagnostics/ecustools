#' Extract timeseries for nearest coordinates from a netcdf file
#'
#' @param filename Name of the netcdf file
#' @param req.coords matrix or dataframe of longitude and latitude coordinates
#' @param req.var name of the variable to extract
#' @param time.var name of the time variable
#' @param lon.var,lat.var names of the longitude and latitude variables in the 
#' netcdf file
#'
#' @return
#' @export
#' @import ncdf4
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{req.coors = data.frame(
#'   lon = c(-64.5417, -64.9888, -64.7),
#'   lat = c(30.0867, 30.6486, 32.47))
#' GetNetCDFAtCoords("path/to/file.nc",
#'                   req.coords = req.coords,
#'                   req.var = "nobs")
#' }
GetNetCDFAtCoords <- function(filename, req.coords, req.var, time.var = "time",
                              lon.var = "longitude", lat.var = "latitude"){
  
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("package 'ncdf4' is needed for this function to work. Please install it.
         Linux users may have to install the 3rd party libraries libnetcdf-dev
         and libnetcdff-dev before installing ncdf4",
         call. = FALSE)
  }
  
  #browser()
  nc1 <- nc_open(filename, readunlim = FALSE)
  print(nc1)
  
  lats <- ncdf4::ncvar_get(nc1, varid = lat.var)
  lons <- ncdf4::ncvar_get(nc1, varid = lon.var)
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
    dat.out <- coord.inds.to.get %>% 
      tibble::as_tibble() %>% 
      dplyr::mutate(row = 1:n()) %>% 
      dplyr::rowwise() %>% 
      dplyr::do({
        tmp <- ncdf4::ncvar_get(
          nc1,
          varid = req.var,
          start = c(.$lon.inds.to.get, .$lat.inds.to.get, 1),
          count = c(1, 1, -1)
        )
        data.frame(location = as.character(.$row), date = 1:length(tmp),
                   req.var = tmp)
      })
  )
  
  #dat.out <- tibble::as_tibble(t(dat.out))
  
  dates <- ncdf4::ncvar_get(nc1, varid = time.var, start = c(1), count = c(-1))
  
  ncdf4::nc_close(nc1)
  
  req.coords$location <- as.character(1:nrow(req.coords))
  
  #dat.out <- tidyr::gather(dat.out, location, UQ(req.var), -date)
  
  dat.out <- dplyr::left_join(req.coords, dat.out)
  
  # dat.out <- dat.out %>% 
  #   group_by(location) %>% 
  #   pivot_wider(names_from = location, values_from = req.var)
  
  dat.out$date <- rep(dates, nrow(req.coords))
  
  
  return(dat.out)
}
