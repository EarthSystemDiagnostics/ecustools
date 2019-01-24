#' @title Depth solution for Berger and Heath; e.g. page 65 in Officer and Lynch
#' @param z  depth
#' @param d mixing depth
#' @param z0 epth of the pulse without bioturbation
#' @return weights of the impulse response function
#' @author Thomas Laepple
#' @export
ImpulseResponse.BergerHeath <- function(z, d, z0 = 0) {
  x <- z0 + d - z
  epsilon <- x/d
  result <- 1/d * exp(-epsilon)
  result[z > (z0 + d)] <- 0
  return(result)
}

