#' Root-mean-square deviation
#'
#' Calculate the root-mean-square deviation (rmsd) of two numeric vectors.
#' @param v1 numeric vector for which to compute the rmsd with \code{v2}.
#' @param v2 numeric vector for which to compute the rmsd with \code{v1}; must
#' have the same length as \code{v1}.
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds. Defaults to \code{FALSE}.
#' @return The root-mean-square deviation of \code{v1} and \code{v2}, or
#' \code{NA} (for \code{na.rm = FALSE}) if any of their elements is \code{NA}.
#' @author Thomas Münch
#' @examples
#' v1 <- rnorm(1000)
#' v2 <- rnorm(1000)
#' rmsd(v1, v2)
#' @export
rmsd <- function(v1, v2, na.rm = FALSE) {
  function_deprecated("stattools")
  if (length(v1) != length(v2)) {
    stop("Arguments must have the same length.")
  }
  
  sqrt(mean((v1 - v2)^2, na.rm = na.rm))

}
