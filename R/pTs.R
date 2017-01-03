#' Create a pTs object
#'
#' @param data a vector, matrix or ts object
#' @param time vector
#' @param lat vector of length 1
#' @param lon vector of length 1
#' @param name character string, name of the pTs object to be generated
#' @param history character string to append to the history attribute
#' @param date logical, whether or not to append the current date to the history attribute
#'
#' @description pTs objects are enhanced time-series \code{\link[stats]{ts}} objects. \code{pTs()} adds attributes such as longitude and latitude to a time series
#'   vector/time series vectors (having the same time basis) and assigns the
#'   class "pTs" to the resulting object
#'
#' @return pTs object
#' @export
#'
#' @author Thomas Laepple
#' @examples
pTs <- function(data,
                time,
                lat = 0,
                lon = 0,
                name = "",
                history = "",
                date = TRUE)
{
  #constants
  TOL = 1 / 400 #tolerance less than 1 day

  if (length(data) <= 1)
    if (is.null(data[1]))
      data <- matrix(NA, length(time), length(name))
    else if (length(name) == 1)
      data <- rep(data[1], length(time))
    else
      data <- matrix(data[1], length(time), length(name))



    if (!is.null(ncol(data)) && (ncol(data) > 1))
      #multiple datasets
    {
      #check data
      if (nrow(data) != length(time))
        stop("nTime != N_Elements(data)")

    }
    #check data
    else if (length(data) != (length(time)))
      stop("nTime != N_Elements(data)")

    #shape data in 2D array

    if (length(time) > 1)
      #real time series
    {
      if (abs(max(diff(time)) - min(diff(time))) > TOL)
        stop("time steps are not equidistant")
      result <- ts(data, start = time[1], deltat = (time[2] - time[1]))
    }
    else
      result <- ts(data, start = time[1]) #or only one time step

    #put attributes and classes
    attr(result, 'lat') <- lat
    attr(result, 'lon') <- lon
    attr(result, 'name') <- name

    if (date)
      attr(result, 'history') <- paste(date(), history)
    else
      attr(result, 'history') <- paste(history)


    attr(result, 'oclass') <- class(result)
    attr(result, 'nclass') <- c('pTs', 'ts')
    class(result) <- attr(result, 'nclass')

    invisible(result)
}
