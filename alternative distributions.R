#script to generate alternative distributions

# global parameters

library(tidyverse)
set.seed(123)
n <- 10000 # 10000 observations in the population dataframe

# version 5/3: Lognormal (baseline)
m <- 2500
v <- 800^2

sdlog  <- sqrt(log(1 + v / m^2))
meanlog <- log(m) - 0.5 * sdlog^2

dat_inc <- rlnorm(n, meanlog, sdlog)|> 
  data.frame()

names(dat_inc)[1] <- "Lognormal"

plot_skewed <- dat_inc |> 
  ggplot(aes(x = skewed)) +
  geom_histogram()

# version 6: Gamma (controlled skewness)
shapes <- c(500,1000,1500,2000)
betas <- 2500 / shapes

for (i in seq_along(shapes)) {
  dat_inc[[paste0("Gamma", shapes[i])]] <-  rgamma(n, shape =  shapes[i], rate =
                                                     betas[i])
} 
#Gamma Verteilung produziert aktuell nicht erwarteten Mittelwert

# version 7: Pareto (tail stress test)
alphas <- c(1.5, 2.5, 2.0)
rpareto_mean <- function(n, alpha, mean) {
  xm <- mean * (alpha - 1) / alpha
  xm * runif(n)^(-1 / alpha)
}

for (i in seq_along(alphas)){
  dat_inc[[paste0("pareto_a", alphas[i])]] <-  rpareto_mean(n, alpha = alphas[i], mean = 2500)
} 

# version 8: Mixtures (body + heavy tail)
component <- rbinom(10000, 1, 0.9)
dat_inc$Mix <- ifelse(component == 1,
                     rlnorm(n, meanlog, sdlog),
                     rpareto_mean(n, alpha = 1.5, mean = 2500))

# check mean incomes
sapply(dat_inc, mean)
