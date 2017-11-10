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
  if(length(n) > 1 & length(s)==1)
    s <- rep(s, length(n))
  if(length(s) != length(n))
    stop("s and n should be vectors of either the same length, or one of them should be length == 1")

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



#' Standard deviation of a numerical probability density function
#'
#' Calculates the standard deviation of a numerical probability density function,
#' i.e. a vector of values and corresponding vector of densities. It is important
#' that the range of x values cover a sufficient fraction of the total density.
#' For continuous distributions it should extend well into the tails. The
#' approximation will be better the finer the resolution of x.
#'
#' @param x vector of values
#' @param d vector of densities, do not need to sum to 1
#'
#' @return numeric
#' @export
#' @rdname SDofNumDist
#' @author Andrew Dolman <andrew.dolman@awi.de>
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Example 1
#'  x <- seq(1, 100, by = 0.01)
#'  d <- dnorm(x, 60, 5)
#'  plot(x, d, type = "l")
#'  SDofNumDist(x, d)
#'
#'  # Example 2
#'  a <- 45
#'  b <- 55
#'  d2 <- dunif(x, a, b)
#'  plot(x, d2, type = "l")
#'  SDofNumDist(x, d2)
#'
#'  # analytical solution for uniform distribution
#'  sqrt(1/12 * (b-a)^2)
#'  }
#' }
SDofNumDist <- function(x, d){
  d <- d / sum(d)
  sqrt(sum(d*x^2) - sum(d*x)^2)
}

