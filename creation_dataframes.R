# script to generate the data

# global parameters

library(tidyverse)
library(ineq)
set.seed(123)
n <- 10000 # 10000 observations in the population dataframe

# version 1: normal distribution

dat_inc <- rnorm(n, 2500, 800) |> 
  data.frame()

names(dat_inc)[1] <- "normal"

dat_inc |> 
  ggplot(aes(x = normal)) +
  geom_histogram()

# version2: uniform distribution

for (i in 1:nrow(dat_inc)) {
  dat_inc$uniform[i] <- i + rnorm(1, 0, 5)
}

dat_inc |> 
  ggplot(aes(x = uniform)) +
  geom_histogram()


# version 3: right-skewed distribution
dat_inc$skewed <- rlnorm(n, meanlog = 5, sdlog = 1)

dat_inc |> 
  ggplot(aes(x = skewed)) +
  geom_histogram()

save(dat_inc, file = "data/dataframes.RData")
