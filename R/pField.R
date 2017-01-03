#' pField
#'
#' @param data input data, usually a 3d array
#' @param time vector
#' @param lat vector of latitudes, defaults to 0
#' @param lon vector of longitudes, defaults to 0
#' @param name character string giving the name of the pField object to be generated
#' @inheritParams pTs
#'
#' @description adds attributes such as longitudes and latitudes to a gridded
#'   data array (which in most cases is three-dimensional and where the dimensions
#'   stand in general for longitude, latitude and time) and assigns the class
#'   "pField" to the resulting object
#'
#' @return a pField object
#' @export
#'
#' @author Thomas Laepple
#' @examples
pField <-
  function(data,
           time,
           lat = 0,
           lon = 0,
           name = " ",
           history = " ",
           date = TRUE)
  {
    #constants
    TOL = 1 / 400 #tolerance less than 1 day

    #check data
    if (length(data) <= 1)
      if (is.null(data[1]))
        data <- rep(NA, length(lat) * length(lon) * length(time))
      else
        data <- rep(data[1], length(lat) * length(lon) * length(time))

      if (length(data) != (length(lat) * length(lon) * length(time)))
      {
        stop(
          "nLat*nLon*nTime != N_Elements(data), if you want to create an empty field, supply data=NA"
        )
      }

      #shape data in 2D array
      dim(data) <- c(length(lat) * length(lon), length(time))

      if (length(time) > 1)
        #real time series
      {
        if (abs(max(diff(time)) - min(diff(time))) > TOL)
          stop("time steps are not equidistant")
        result <- ts(t(data),
                     start = time[1],
                     deltat = (time[2] - time[1]))
      }
      else
        result <- ts(t(data), start = time[1]) #or only one time step

      #put attributes and classes
      attr(result, 'lat') <- lat
      attr(result, 'lon') <- lon
      attr(result, 'name') <- name
      if (date)
        attr(result, 'history') <- paste(date(), history)
      else
        attr(result, 'history') <- paste(history)


      attr(result, 'oclass') <- class(result)
      attr(result, 'nclass') <- c('pField', 'ts')
      class(result) <- attr(result, 'nclass')

      invisible(result)
  }
