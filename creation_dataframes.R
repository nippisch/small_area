# script to generate the data

# global parameters

library(tidyverse)
set.seed(123)
n <- 10000 # 10000 observations in the population dataframe

# version 1: normal distribution

dat_inc <- rnorm(n, 2500, 800) |> 
  pmax(0) |> 
  data.frame()

names(dat_inc)[1] <- "normal"

plot_normal <- dat_inc |> 
  ggplot(aes(x = normal)) +
  geom_histogram()

# version 2: uniform distribution

for (i in 1:nrow(dat_inc)) {
  dat_inc$uniform[i] <- pmax(i + rnorm(1, 0, 5), 0)
}

plot_uniform <- dat_inc |> 
  ggplot(aes(x = uniform)) +
  geom_histogram()


# version 3: right-skewed distribution
dat_inc$skewed <- rlnorm(n, meanlog = 5, sdlog = 1)

plot_skewed <- dat_inc |> 
  ggplot(aes(x = skewed)) +
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

ggpubr::ggarrange(plot_normal, plot_uniform, plot_skewed, plot_jenkins, nrow = 2, ncol = 2)

domains <- c(rep("d1", 500),
             rep("d2", 1000),
             rep("d3", 2500),
             rep("d4", 6000))

# randomly shuffle and assign to dataframe
dat_inc$domain <- sample(domains)

save(dat_inc, file = "data/dataframes.RData")

