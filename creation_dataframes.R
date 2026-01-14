# script to generate the data

# global parameters

library(tidyverse)
set.seed(123)
n <- 10000 # 10000 observations in the population dataframe

# version 1: uniform distribution
dat_inc <- data.frame(row.names = 1:10000)
for (i in 1:nrow(dat_inc)) {
  dat_inc$uniform[i] <- pmax(i/2 + rnorm(1, 0, 5), 0)
}

plot_uniform <- dat_inc |> 
  ggplot(aes(x = uniform)) +
  geom_histogram()


# version 2: Gamma
dat_inc$gamma <- rgamma(n, shape = 0.5, rate = 1/5000)

plot_gamma <- dat_inc |> 
  ggplot(aes(x = gamma)) +
  geom_histogram()

#Version 3: Lognormal
m <- 2500
v <- 800^2

sdlog  <- sqrt(log(1 + v / m^2))
meanlog <- log(m) - 0.5 * sdlog^2

dat_inc$lognormal <- rlnorm(n, meanlog, sdlog)

plot_lognormal <- dat_inc |> 
  ggplot(aes(x = lognormal)) +
  geom_histogram()


# version 4: GB2 by Jenkins (2009)
rGB2 <- function(n, a, b, p, q) {
  u <- rbeta(n, shape1 = p, shape2 = q)
  y <- b * (u / (1 - u))^(1 / a)
  return(y)
}

# Parameters from the paper (approximate)
a <- 2.994
b <- 227.840
p <- 1.063
q <- 1.015

dat_inc$jenkins <- rGB2(n, a, b, p, q)

plot_jenkins <- dat_inc |> 
  ggplot(aes(x = jenkins)) +
  geom_histogram()

ggpubr::ggarrange(plot_uniform, plot_gamma, plot_lognormal, plot_jenkins, nrow = 2, ncol = 2)

domains <- c(rep("d1", 500),
             rep("d2", 1000),
             rep("d3", 2500),
             rep("d4", 6000))

# randomly shuffle and assign to dataframe
dat_inc$domain <- sample(domains)

save(dat_inc, file = "data/dataframes.RData")

summary(dat_inc)
