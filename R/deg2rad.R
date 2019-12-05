#' Convert between degree and radian
#'
#' Convert coordinates from degree to radian and vice versa.
#' @param x a numeric vector of coordinate values to be converted.
#' @param inverse logical; convert from degree to radian (the default) or from
#' radian to degree (\code{inverse = TRUE}).
#' @return numeric vector of the same length as \code{x} with the converted
#' coordinate values.
#' @author Thomas Münch
#' @examples
#' deg2rad(deg2rad(-75), inverse = TRUE)
#' @export
deg2rad <- function(x, inverse = FALSE) {
  f <- ifelse(inverse, 180 / pi, pi / 180)
  f * x
}
