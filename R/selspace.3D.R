##' @title first element of a vector
##' @param x vector
##' @return first element of X
##' @author Thomas Laepple
FirstElement <- function(x) {
  return(x[1])
}

##' @title last element of a vector
##' @param x vector
##' @return last element of X
##' @author Thomas Laepple
LastElement <- function(x) {
  return(x[length(x)])
}


#' SelSpace3D
#' @param data pField object
#' @param lat1 vector length 1
#' @param lon1 vector legnth 1
#' @param SBOX vector length 1, defaults to 5
#' @param tolLon vector length 1, defaults to 10
#' @param bNN logical, defaults to FALSE
#' @param timeindexNA at which timestep should the Next Neighbour algorithm
#'   search for missing values
#'
#' @description Function to interpolate a field to a given point specified by
#'   latitude and longitude. It uses the nearest neighbour if the adjancents
#'   points are missing, if not it uses bilinear interpolation
#'
#' @return \code{\link{pTs}} object
#' @export SelSpace3D selspace.3D
#' @aliases SelSpace3D selspace.3D
#' @author Thomas Laepple
#' @examples
SelSpace3D <- function(data, lat1, lon1, SBOX = 5, tolLon = 10, 
  bNN = FALSE, timeindexNA = 1) {
  choice.lat <- lat1
  choice.lon <- lon1
  temp <- attributes(data)
  
  if (prod(dim(data)) != length(temp$lon) * length(temp$lat) * 
    length(time(data))) 
    stop("N(data) != N(lat)*N(lon)*N(time)")
  
  # make a 2D array [lon,lat] containing the orginal indices
  pointer3d.raw <- array(1:dim(data)[2], c(1, length(temp$lon), 
    length(temp$lat)))
  
  # arrange to get a continous field
  nlon <- length(temp$lon)
  nTime <- length(time(data))
  d <- diff(temp$lon)
  
  if (max(d) > (min(d) + 0.01)) {
    edgelon <- which(d == max(d))
    pointer3d <- array(NA, c(1, length(temp$lon), length(temp$lat)))
    pointer3d[, (edgelon + 1):nlon, ] <- pointer3d.raw[, 
      (edgelon + 1):nlon, ]
    pointer3d[, 1:edgelon, ] <- data3d.raw[, 1:edgelon, ]
    temp$lon <- c(temp$lon[(edgelon + 1):nlon], temp$lon[1:edgelon] + 
      360)
  } else pointer3d <- pointer3d.raw
  
  # longitude jump by wrapping
  wrap.dLon <- FirstElement(temp$lon) - (LastElement(temp$lon) - 
    360)
  
  if ((wrap.dLon > 0) & (wrap.dLon < tolLon)) {
    ### Check if we the data is global on the longitudes, than Copy
    ### the data 3 times.... to avoid breaks in the longitude
    pointer3d.3c <- array(NA, c(1, 3 * length(temp$lon), 
      length(temp$lat)))
    pointer3d.3c[, 1:nlon, ] <- pointer3d[, 1:nlon, ]
    pointer3d.3c[, (nlon + 1):(2 * nlon), ] <- pointer3d[, 
      1:nlon, ]
    pointer3d.3c[, (2 * nlon + 1):(3 * nlon), ] <- pointer3d[, 
      1:nlon, ]
    
    lon.3c <- c(temp$lon[1:nlon] - 360, temp$lon[1:nlon], 
      temp$lon[1:nlon] + 360)
  } else {
    # Do wrap the longitudes at the longitudes could not be
    # connected
    lon.3c <- temp$lon
    pointer3d.3c <- pointer3d
  }
  
  if (temp$lat[2] < temp$lat[1]) 
    # if the latitudes are from + to -, reverse them
  {
    temp$lat <- rev(temp$lat)
    pointer3d.3c <- pointer3d.3c[, , rev(seq(temp$lat))]
  }
  
  
  # attention... midpoints are given...
  if ((lat1 > LastElement(temp$lat)) | (lat1 < FirstElement(temp$lat))) {
    warning("Latitude outside field")
    return(NULL)
  }
  if ((lon1 > LastElement(lon.3c)) | (lon1 < FirstElement(lon.3c))) {
    warning("Longitude outside field")
    return(NULL)
  }
  
  indexLat <- rev(which(temp$lat <= lat1))[1]:which(temp$lat >= 
    lat1)[1]
  indexLon <- rev(which(lon.3c <= lon1))[1]:which(lon.3c >= 
    lon1)[1]
  
  # First check if we have any missing data
  pointer.select <- pointer3d.3c[, indexLon, indexLat]
  
  
  if (is.null(dim(pointer.select))) {
    dim(pointer.select) <- c(1, length(pointer.select))
  } else {
    dim(pointer.select) <- c(1, dim(pointer.select))
    }
     
  
  # data.select<-(as.matrix(data)[,pointer.select]) if
  # (is.null(dim(data.select)) nData<-sum(!is.na(data.select))
  # else nData<-colSums(!is.na(data.select))
  
  # if (min(colSums(!is.na(data)[,pointer.select]==0) {
  # warning('One neighbour point only contains missing values;
  # therefore Next Neighbour is used') bNN=TRUE }
  
  if (bNN) {
    # Create an area around the point
    tempindexLat <- ((indexLat[1] - SBOX):(indexLat[1] + 
      SBOX))
    tempindexLon <- ((indexLon[1] - SBOX):(indexLon[1] + 
      SBOX))
    
    # remove areas outside the boundaries
    tempindexLat <- tempindexLat[tempindexLat > 0]
    tempindexLon <- tempindexLon[tempindexLon > 0]
    tempindexLat <- tempindexLat[tempindexLat <= length(temp$lat)]
    tempindexLon <- tempindexLon[tempindexLon <= length(lon.3c)]
    
    # Get the nearest neighbours
    res <- expand.grid(tempindexLon, tempindexLat)
    
    # only retain the nearest nonmissing neighbours
    IndexRegion <- diag(pointer3d.3c[1, res[, 1], res[, 2]])
    indexV <- !is.na(data[timeindexNA, IndexRegion])
    
    if (sum(indexV) == 0) 
      return(NA)
    x <- res[indexV, 1]
    y <- res[indexV, 2]
    
    # Distances to the points in the area
    D2i <- (lon.3c[x] - lon1)^2 + (temp$lat[y] - lat1)^2
    
    # nearest neighbour is the one with the smallest distance
    neighbour <- order(D2i)[1]
    
    # Switch to real data
    intpoldata <- data[, pointer3d.3c[, x[neighbour], y[neighbour]]]
    choice.lat <- temp$lat[y[neighbour]]
    choice.lon <- lon.3c[x[neighbour]]
  } else {
    # here the interpolation starts
    
    if (length(indexLat) == 1) 
      ey = NA else ey <- (lat1 - temp$lat[indexLat[1]])/(temp$lat[indexLat[2]] - 
      temp$lat[indexLat[1]])
    if (length(indexLon) == 1) 
      ex = NA else ex <- (lon1 - lon.3c[indexLon[1]])/(lon.3c[indexLon[2]] - 
      lon.3c[indexLon[1]])
    
    
    if ((!is.finite(ex)) & (!is.finite(ey))) 
      intpoldata <- data[, pointer.select] else # we are on the point, no interpolation
    if (!is.finite(ex)) {
      intpoldata <- data[, pointer.select[, 1]] + (data[, 
        pointer.select[, 2]] - data[, pointer.select[, 
        1]]) * ey
      # only latitudonal interpolation
    } else {
      if (!is.finite(ey)) {
        intpoldata <- data[, pointer.select[, 1]] + (data[, 
          pointer.select[, 2]] - data[, pointer.select[, 
          1]]) * ex
        # only longitudonal interpolation #Switch to real data
      } else intpoldata <- (1 - ex) * (1 - ey) * data[, pointer.select[, 
        1, 1]] + (1 - ex) * (ey) * data[, pointer.select[, 
        1, 2]] + (ex) * (1 - ey) * data[, pointer.select[, 
        2, 1]] + (ex * ey) * data[, pointer.select[, 
        2, 2]]
    }
  }
  # create time series
  result <- pTs(intpoldata, time(data), choice.lat, choice.lon, 
    GetName(data), GetHistory(data), date = FALSE)
  
  hist <- paste("selspace: lat=", lat1, " lon=", lon1, sep = "")
  
  return(AddHistory(result, hist))
}

selspace.3D <- function(...) {
  warning("selspace.3D is deprecated and replaced with SelSpace3D
    to comply with ECUS R style guide")
  SelSpace3D(...)
}
