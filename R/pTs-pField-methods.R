#' getname
#'
#' @param data a pTs or pField object
#'
#' @description returns the "name" attribute of a pTs/pField object
#'
#' @return character
#' @export
#' @author Thomas Laepple
#' @examples
getname <- function(data){return(attr(data, "name"))}


#' gethistory
#'
#' returns the "history" attribute of a pTs/pField object
#'
#' @param data a pTs or pField object
#'
#' @family history
#'
#' @return character
#' @author Thomas Laepple
#' @examples
gethistory <- function(data) {
  return(attr(data, "history"))
}


#' addhistory
#'
#' adds a history (e.g., a comment) to a pTs or a pField object
#'
#' @param x a pTs or pField object
#' @param newhist character string
#'
#' @family history
#'
#' @return a pTs or pField object
#' @export
#' @author Thomas Laepple
#' @examples
addhistory <- function(x, newhist){
  newhist <- paste(date(), newhist)
  attr(x, "history") <- c(attr(x, "history"), newhist)
  return(x)
}
