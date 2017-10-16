#' @title Standard deviation (error) of a standard deviation
#' @description Calculate the standard deviation (i.e. standard error) of a
#' standard deviation, given n, the number of samples from which the standard
#' deviation was estimated.
#' @param s standard deviation
#' @param n number of samples from which s was estimated
#' @return numeric
#' @details if n <= 0 an exact method is used, otherwise Stirling's
#'  approximation is used to avoid numerical errors calculating the gamma
#'  function for large values of n. Formulae taken from
#'  https://stats.stackexchange.com/q/28567
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname SDofSD
#' @export
SDofSD <- function(s, n) {
  if (any(n < 2))
    stop("n must be greater than or equal to 2")
  if (any(s < 0))
    stop("s must be >= 0")
  if(length(s) > 1 & length(n)==1)
    n <- rep(n, length(s))
  if(length(s) != length(n))
    stop("s and n should be vectors of the same length")

  ifelse(n <= 300, {
    message("Method: Exact")
    g.a <- gamma((n - 1) / 2)
    g.b <- gamma(n / 2)
    s * (g.a / g.b) * sqrt((n - 1) / 2 - (g.b / g.a) ^ 2)
  },  {
    message("Method: Stirling's approximation")
    s * sqrt(exp(1) * (1 - 1 / n) ^ (n - 1) - 1)
  })
}


