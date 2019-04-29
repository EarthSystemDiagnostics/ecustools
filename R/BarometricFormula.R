##' Barometric height formula
##'
##' Calculate atmospheric surface pressure from elevation and temperature using
##' the barometric height formula for an isothermal atmosphere.
##' @param T surface temperature in degrees Celsius
##' @param h surface elevation in [m]
##' @param p0 reference surface pressure; defaults to 1013 mbar
##' @return surface pressure in [mbar] at elevation \code{h} and temperature
##' \code{T}
##' @author Thomas Münch
##' @examples
##' # Surface pressure at Kohnen Station:
##' BarometricFormula(-44.5, 2892)
##' @export
BarometricFormula <- function(T, h, p0 = 1013) {

    kR <- 8.314
    kg <- 9.807
    kM <- 0.02896

    T <- T + 273.15

    hs <- kR * T / (kg * kM)

    return(p0 * exp(-h / hs))

}

