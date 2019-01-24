#' GetName
#'
#' @param data a pTs or pField object
#'
#' @description returns the "name" attribute of a pTs/pField object
#'
#' @return character
#' @aliases getname GetName
#' @export getname GetName
#' @author Thomas Laepple
#' @examples
#' getname(c(a = 2))
#' GetName(c(a = 2))
GetName <- function(data){
  return(attr(data, "name"))
}
getname <- function(...){
  warning("getname is deprecated and replaced with GetName
          to comply with ECUS R style guide")
  GetName(...)
}

#' GetHistory
#'
#' returns the "history" attribute of a pTs/pField object
#'
#' @param data a pTs or pField object
#'
#' @family history
#' @aliases GetHistory gethistory
#'
#' @return character
#' @author Thomas Laepple
#' @examples
GetHistory <- function(data) {
  #print(match.call())
  return(attr(data, "history"))
}
gethistory <- function(...){
  warning("gethistory is deprecated and replaced with GetHistory
          to comply with ECUS R style guide")
  GetName(...)
}


#' AddHistory
#'
#' adds a history (e.g., a comment) to a pTs or a pField object
#'
#' @param x a pTs or pField object
#' @param newhist character string
#'
#' @family history
#' @aliases addhistory AddHistory
#'
#' @return a pTs or pField object
#' @export addhistory AddHistory
#' @author Thomas Laepple
#' @examples
AddHistory <- function(x, newhist){
  newhist <- paste(date(), newhist)
  attr(x, "history") <- c(attr(x, "history"), newhist)
  return(x)
}
addhistory <- function(...){
  warning("addhistory is deprecated and replaced with AddHistory
          to comply with ECUS R style guide")
  AddHistory(...)
}
