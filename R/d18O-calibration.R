# d18O calibration functions from Anand et al (2003)


#' Calculate d18O of seawater from salinity
#'
#' @param Salinity Salinity in grams per litre or parts per thousand.
#' @description The equation from Duplessy et al. (1991) based on the
#'   salinity for water samples collected in the upper 250 m of the Atlantic
#'   Ocean during the GEOSECS expedition. As reported in Anand et al. 2003
#' @references
#'
#' Anand, P., Elderfield, H. and Conte, M. H.: Calibration of Mg/Ca thermometry
#' in planktonic foraminifera from a sediment trap time series,
#' Paleoceanography, 18(2), 1050, doi:10.1029/2002PA000846, 2003.
#'
#' Duplessy, J.-C., L. Labeyrie, A. Juillet-Leclerc, F. Maitre, J. Dupart, and
#' M. Sarnthein, Surface salinity reconstruction of the north Atlantic Ocean
#' during the last glacial maximum, Oceanol. Acta, 14, 311– 324, 1991.
#'
#' @return d18Ow as permille deviation from the SMOW scale
#' @seealso d18OcFromd18OwTemp CalcifTemp
#' @export
#'
#' @examples
#' d18OwFromSalinity(30)
d18OwFromSalinity <- function(Salinity){
  -19.264 + 0.558 * Salinity
}


#' Equilibrium d18O of calcification from d18Ow and Temperature (Paleotemperature Equation)
#'
#' @param d18Ow d18O of seawater
#' @param Temp Temperature in celcius
#'
#' @description The paleotemperature equation of O’Neil et al. (1969) and
#'   Shackleton (1974). The factor 0.27 was used to convert from water on the
#'   SMOW scale to calcite on the Peedee Belemnite (PDB) scale.
#'
#'   As described in Anand et al. 2003
#'
#' @references
#'
#' O’Neil, J. R., R. N. Clayton, and T. K. Mayeda, Oxygen isotope fractionation
#' in divalent metal carbonates, 51, 5547– 5558, 1969.
#'
#' Shackleton, N. J., Attainment of isotopic equilibrium between ocean water and
#' the benthonic foraminiferal genus Uvigerina: Isotopic changes in the ocean
#' during the last glacial, Cent. Natl. Rech. Sci. Colloq. Int., 219, 203–209,
#' 1974.
#'
#' @return d18O of calcite on PDB scale
#' @seealso d18OwFromSalinity CalcifTemp
#' @export
#'
#' @examples
#'d18OcFromd18OwTemp(d18Ow = 0.6, 1:26)
d18OcFromd18OwTemp <- function(d18Ow, Temp){
  (d18Ow - 0.27) + (4.38 - (4.38^2 - 4 * 0.1 * (16.9 - Temp))^0.5) / (2 * 0.1)
}



#' Calcification temperature from calcite d18O and d18Ow (inverse of the Paleotemperature Equation)
#'
#' @param d18Oc d18O of calcite on PDB scale
#' @param d18Ow d18O of seawater SMOW scale
#'
#' @return Temperature in celcius
#' @export
#'
#' @examples
#' CalcifTemp(d18OcFromd18OwTemp(d18Ow = 0.6, 1:26), 0.6)
CalcifTemp <- function(d18Oc, d18Ow){
  16.9 - 4.38 * (d18Oc - (d18Ow - 0.27)) + 0.1 *
    (d18Oc - (d18Ow - 0.27))^2
}
