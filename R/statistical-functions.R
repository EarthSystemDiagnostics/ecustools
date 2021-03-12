#' Population standard deviation
#'
#' @param x 
#'
#' @return numeric, population standard deviation
#' @export
#'
#' @examples
#' 
#' # analytical solution to SD of uniform
#' sqrt(1/12 * 10^2)
#'  
#' sd(1:10)
#' 
#' PopSD(1:10)
PopSD <- function(x){
  n <- length(x)
  adj <- sqrt((n-1) / n)
  adj * sd(x)
}


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
#'  SDofSD(1, 12)
#'  library(ggplot2)
#' df <- expand.grid(SD = 1:10, n = seq(2, 10, length.out = 100))
#' df$SDofSD = SDofSD(df$SD, df$n)
#' ggplot(df, aes(x = n, y = SDofSD, group = SD, colour = SD)) +
#'   geom_line()
#'  }
#' }
#' @rdname SDofSD
#' @export
SDofSD <- function(s, n) {
  if (any(n < 2, na.rm = TRUE))
    warning("n must be greater than or equal to 2")
  if (any(s < 0, na.rm = TRUE))
    warning("s must be >= 0")
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



#' Standard deviation of a numerical probability density function.
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


#' Summarise an Empirical Probability Distribution Function.
#'
#' @param x A vector of values of empirical PDF
#' @param p A vector of probabilities
#' @details Calculation of the mode is naive. For a multimodal distribution only
#' the highest is returned, in the case of 2 or more modes with exactly the same
#'  probability, the first is returned.
#' @return Returns a named vector with the mean, median, mode, and standard
#' deviation of the empirical PDF
#' @export
#' @examples
#' df <- data.frame(x = 1:10)
#' df$p <- dnorm(df$x, 5, 2)
#' SummariseEmpiricalPDF(df$x, df$p)
SummariseEmpiricalPDF <- function(x, p){

  # Ensure x and p are sorted
  p <- p[order(x)]
  x <- sort(x)

  # Ensure p sum to 1
  p <- p / sum(p)

  # Mean
  w.mean <- sum(x * p)

  # SD
  M <- sum(p > 0)
  w.sd <- sqrt(sum(p * (x-w.mean)^2) / ((M-1)/M * sum(p)))

  # Median
  csum.p <- cumsum(p)
  med.ind <- which.min(abs(csum.p - 0.5))
  w.median <- x[med.ind]

  # Mode
  max.wt <- max(p)
  n.max <- sum(p == max.wt)
  if (n.max > 1) warning(paste0(n.max,
                                " x with equal maximum probability. Returning the first"))
  mode <- x[which.max(p)]

  return(c("mean" = w.mean, "median" = w.median, "mode" = mode, "sd" = w.sd))
}

#' Expected range
#'
#' Calculates the expected range of n observations drawn from a distibution with standard deviation sd
#' @param sd standard deviation
#' @param n number of observations
#'
#' @return numeric
#' @export
#'
#' @examples
#'
#' ExpectedRange(5, 15)
#'
#' \dontrun{
#' library(tidyverse)
#' f <- function(x, n) n * x * pnorm(x)^(n - 1) * dnorm(x)
#' ests <- expand.grid(mean = 0, sd = 2, n = 15, rep = 1:1000, i = 1:15) %>%
#'   mutate(rdv = rnorm(n(), mean, sd)) %>%
#'   group_by(mean, sd, n, rep) %>%
#'   summarise(sd_hat = sd(rdv),
#'             obs.range = diff(range(rdv))) %>%
#'   group_by(mean, sd, n, rep) %>%
#'   mutate(sd.range = 2*sd_hat*integrate(f,-Inf,Inf, n = n)$value,
#'          exp.range = ExpectedRange(sd, n))
#' ests %>%
#'   gather(estimate, value, -mean, -sd, -n, -rep, -sd_hat, -exp.range) %>%
#'   ggplot(aes(x = estimate, y = value)) +
#'   geom_boxplot() +
#'   geom_hline(aes(yintercept = exp.range)) +
#'   facet_wrap(~n)
#'   }
ExpectedRange <- function(sd, n) {

  f <- function(x, n) n * x * pnorm(x)^(n - 1) * dnorm(x)

  exp.range <- 2 * sd * integrate(f, -Inf, Inf, n = n)$value

  return(exp.range)
}


