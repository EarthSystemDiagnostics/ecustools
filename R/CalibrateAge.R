#' @title Calibrate 14C age with Bchron
#' @description Calibrates a set of 14C ages in a dataframe using the BChron package
#' @param df dataframe
#' @param age.14C Name of column with 14C ages, Default: 'age.14C'
#' @param age.14C.se Name of column with 1se 14C age uncertainty, Default: 'age.14C.se'
#' @param curve Calibration curve, Default: 'intcal20'
#' @param return.type Return the ammended dataframe or additionally the list of PDFs, Default: 'df'
#' @param offset Optional offset applied to all 14C ages, Default: 0
#' @return A dataframe or list
#' @details A wrapper for Bchron::Bchroncalibrate
#' @examples
#' # With defaults
#' dat <- data.frame(age.14C = c(2000, 20000),
#'                   age.14C.se = c(100, 200))
#' CalibrateAge(dat)
#'
#' # Change the calibration
#' CalibrateAge(dat, curve = "marine13")
#'
#' # Return the PDFs
#' cal.lst <- CalibrateAge(dat, curve = "marine13", return = "lst")
#' with(cal.lst[[2]][[1]][[1]], {plot(ageGrid, densities)})
#'
#' # Use different column names
#' dat <- data.frame(radiocarbon.age = c(2000, 20000),
#'                  se = c(100, 200))
#' CalibrateAge(dat, age.14C = "radiocarbon.age", age.14C.se = "se")
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[Bchron]{BchronCalibrate}}
#'  \code{\link[ecustools]{SummariseEmpiricalPDF}}
#' @rdname CalibrateAge
#' @export
#' @importFrom Bchron BchronCalibrate
CalibrateAge <- function(df, age.14C = "age.14C",
                         age.14C.se = "age.14C.se",
                         curve = "intcal20", 
                         return.type = "df", offset = NULL){
  function_deprecated("prxytools")
  return.type <- match.arg(return.type, choices = c("df", "lst"))
  curve <- match.arg(curve, choices = c("intcal13", "shcal13", "marine13",
                                        "intcal20", "marine20", "shcal20",
                                        "normal"))
  
  if (is.null(offset)){
    df$offset <- 0
  } else{
    df$offset <- df[[offset]]
  }
  

  cal.ages <- lapply(1:nrow(df), function(x) {
    tryCatch(Bchron::BchronCalibrate(
      ages = df[[age.14C]][x] + df[["offset"]][x],
      ageSds = df[[age.14C.se]][x],
      calCurves = curve,
      ids = x),
      error = function(i){
        cat(strsplit(as.character(i), " : ", fixed = TRUE)[[1]][2])
        #print(as.character(i))
        NA
      })
  })


  # Use mean and sd of empirical PDFs as point estimates of calendar ages
  df$age.14C.cal <- sapply(cal.ages, function(x){
    if (is.na(x) == FALSE)
    {SummariseEmpiricalPDF(x[[1]]$ageGrid, x[[1]]$densities)["mean"]} else {NA}
  })

  df$age.14C.cal.se <- sapply(cal.ages, function(x){
    if (is.na(x) == FALSE)
    {SummariseEmpiricalPDF(x[[1]]$ageGrid, x[[1]]$densities)["sd"]} else {NA}
  })

  if (return.type == "df"){
    out <- df
  }

  if (return.type == "lst"){
    out <- list(df = df, cal.ages = cal.ages)
  }

  return(out)
}
