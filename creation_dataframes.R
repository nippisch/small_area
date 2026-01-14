# script to generate the data

# global parameters

library(tidyverse)
set.seed(123)
n <- 10000 # 10000 observations in the population dataframe

# version 1: uniform distribution
dat_inc <- data.frame(row.names = 1:10000)
L <- 2450
U <- 2550
dat_inc$uniform <- runif(n, min = L, max = U)

ineq::Gini(dat_inc$uniform)

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


# version 4: Dagum-distribution

# install.packages("VGAM")
library(VGAM)
## shape parameters from Bandourian et al. (2002); Germany 1994 (b there: 94075, rescaled to match mean = 2500 of other distributions)
a <- 4.413
p <- 0.337

## calculating b such that E(X) = 2500 (see Kleiber (2008))
b <- 2500 / (gamma(p + 1/a) * gamma(1 - 1/a) / gamma(p))

dat_inc$dagum <- rdagum(n, shape1 = a, shape2 = p, scale  = b)

plot_dagum <- dat_inc |> 
  ggplot(aes(x = dagum)) +
  geom_histogram()

# check distributions visually
ggpubr::ggarrange(plot_uniform, plot_gamma, plot_lognormal, plot_dagum, nrow = 2, ncol = 2)

# adding domains
domains <- c(rep("d1", 500),
             rep("d2", 1000),
             rep("d3", 2500),
             rep("d4", 6000))

# randomly shuffle and assign to dataframe and check
dat_inc$domain <- sample(domains)
table(dat_inc$domain)

summary(dat_inc)

save(dat_inc, file = "data/dataframes.RData")
