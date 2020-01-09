## Optimal sampling
library(tidyverse)

# For a given setup returns the cost and sigma
CostSigma <- function(sigma.ind, sigma.meas,
                       n.ind, n.meas,
                       cost.ind, cost.meas){

  cost <- n.meas * n.ind * cost.ind + n.meas * cost.meas

  sigma <- sqrt(sigma.meas^2 / n.meas + sigma.ind^2 / (n.ind * n.meas))

  data.frame(sigma = sigma, cost = cost)

}


CostSigma(sigma.ind = 0.25, sigma.meas = 0.12,
          n.ind = 25, n.meas = 4,
          cost.ind = 1, cost.meas = 10)


####

# For a given total cost returns the possible n.ind, n.meas and sigmas

NMeas <- function(cost, n.ind,
                  cost.ind, cost.meas){
  n.meas <- cost / (cost.ind  * n.ind + cost.meas)
  n.meas[n.meas < 1] <- NA
  n.meas

}

NInd <- function(cost, n.meas,
                 cost.ind, cost.meas){
  n.ind <- -(cost.meas * n.meas - cost) / (cost.ind * n.meas)
  n.ind[n.ind < 1] <- NA
  n.ind
}


GetSetupSigma <- function(cost,
                          sigma.ind, sigma.meas,
                          cost.ind, cost.meas,
                          return = c("best", "all")){

  max.n.meas <- floor((cost - cost.ind) / cost.meas)
  max.n.ind <- floor((cost - cost.meas) / cost.ind)

  if ((max.n.ind >= 1 & max.n.meas >= 1) == FALSE){
    warning(paste0("cost = ", cost, " is not enough for a single measurement."))
    out <- data.frame(n.ind=NA, n.meas=NA, sigma = NA, cost = NA)
    return(out)
  }

  if (cost.meas >= cost.ind){
    n.meas <- 1:max.n.meas
    n.ind <- floor(-(cost.meas * n.meas - cost) / (cost.ind * n.meas))
    n.meas <- n.meas[n.ind >= 1]
    n.ind <- n.ind[n.ind >= 1]

  } else if (cost.meas < cost.ind){
    n.ind <- 1:max.n.ind
    n.meas <- floor(cost / (cost.ind  * n.ind + cost.meas))
    n.ind <- n.ind[n.meas >= 1]
    n.meas <- n.meas[n.meas >= 1]
  }

  n.ind <- n.ind[n.ind >= 1]
  n.meas <- n.meas[n.meas >= 1]

  cost <- n.meas * n.ind * cost.ind + n.meas * cost.meas

  sigma <- sqrt(sigma.meas^2 / n.meas + sigma.ind^2 / (n.ind * n.meas))

  df <- data.frame(n.ind=n.ind, n.meas=n.meas, sigma = sigma, cost = cost)

  return <- match.arg(return)

  if (return == "all"){
    out <- df
  } else if (return == "best"){
    out <- df[which.min(df$sigma),]
  }

  return(out)
}


df <- GetSetupSigma(cost = 100,
              sigma.ind = 0.22, sigma.meas = 0.12,
              cost.meas = 11, cost.ind = 0.5,
              return = "all")

GetSetupSigma(cost = 100,
              sigma.ind = 0.25, sigma.meas = 0.12,
              cost.meas = 11, cost.ind = 1,
              return = "best")

GetSetupSigma(cost = 10,
              sigma.ind = 2, sigma.meas = 0.5,
              cost.meas = 11, cost.ind = 0.5,
              return = "all")

df %>%
  ggplot(aes(x = n.meas, y = sigma)) +
  geom_point() +
  geom_line()

df %>%
  ggplot(aes(x = n.ind, y = sigma)) +
  geom_point() +
  geom_line()


####

# Cost and sigma

df <- tibble(max.cost = seq(100, 1000, 100)) %>%
  group_by(max.cost) %>%
  do({
    GetSetupSigma(cost = .$max.cost,
                  sigma.ind = 2, sigma.meas = 0.5,
                  cost.meas = 10, cost.ind = 1,
                  return = "best")
  })


df %>%
  ggplot(aes(x = cost, y = sigma)) +
  geom_point() +
  geom_line() +
  expand_limits(y = 0)


###

GetSetupCost <- function(sigma,
                          sigma.ind, sigma.meas,
                          cost.ind, cost.meas,
                          return = c("best", "all")){

  max.n.meas <- floor((sigma.meas^2 + sigma.ind^2) / sigma^2)

  n.meas <- 1:max.n.meas
  n.ind <- ceiling(-sigma.ind^2/(sigma.meas^2-n.meas*sigma^2))

  n.meas <- n.meas[n.ind >= 1]
  n.ind <- n.ind[n.ind >= 1]

  cost <- n.meas * n.ind * cost.ind + n.meas * cost.meas
  sigma <- sqrt(sigma.meas^2 / n.meas + sigma.ind^2 / (n.ind * n.meas))

  df <- data.frame(n.ind=n.ind, n.meas=n.meas, sigma = sigma, cost = cost)

  return <- match.arg(return)

  if (return == "all"){
    out <- df
  } else if (return == "best"){
    out <- df[which.min(df$cost),]
  }

  return(out)
}

GetSetupCost(sigma = 1,
             sigma.ind = 2, sigma.meas = 0.5,
             cost.meas = 10, cost.ind = 1,
             return = "best")

df <- GetSetupCost(sigma = 1,
              sigma.ind = 2, sigma.meas = 0.5,
              cost.meas = 10, cost.ind = 1,
              return = "all") %>%
  tbl_df()

df %>%
  #filter(n.meas > 30) %>%
  ggplot(aes(x = n.meas, y = cost)) +
  geom_line() #+
  #scale_y_log10() +
  #scale_x_log10()
