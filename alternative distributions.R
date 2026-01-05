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

names(dat_inc)[1] <- "lognormal"

plot_lognormal <- dat_inc |> 
  ggplot(aes(x = lognormal)) +
  geom_histogram()

# version 6: Gamma (controlled skewness) without shapes 2 and 0.7
shapes <- c(5, 1,  0.5)
betas <- 2500 / shapes

for (i in seq_along(shapes)) {
  dat_inc[[paste0("gamma", shapes[i])]] <-  rgamma(n, shape =  shapes[i], rate =
                                                     1/betas[i])
} 

gamma_plots <- list()
for (i in seq_along(shapes)) {
  gamma_plots[[paste0("plot_gamma", shapes[i])]] <- ggplot(dat_inc, 
                                                               aes(x = .data[[paste0("gamma", shapes[i])]])) +
    geom_histogram()
}

# version 7: Pareto (tail stress test)
alphas <- c(1.5, 2.5, 2.0)
rpareto_mean <- function(n, alpha, mean) {
  xm <- mean * (alpha - 1) / alpha
  xm * runif(n)^(-1 / alpha)
}

for (i in seq_along(alphas)) {
  dat_inc[[paste0("pareto_a", alphas[i])]] <-  rpareto_mean(n, alpha = alphas[i], mean = 2500)
} 

pareto_plots <- list()
for (i in seq_along(alphas)) {
  pareto_plots[[paste0("plot_pareto_a", alphas[i])]] <- ggplot(dat_inc, 
                                               aes(x = .data[[paste0("pareto_a", alphas[i])]])) +
    geom_histogram()
} 


# version 8: Mixtures (body + heavy tail)
component <- rbinom(10000, 1, 0.9)
dat_inc$mix <- ifelse(component == 1,
                     rlnorm(n, meanlog, sdlog),
                     rpareto_mean(n, alpha = 1.5, mean = 2500))

plot_mix <- dat_inc |> 
  ggplot(aes(x = mix)) +
  geom_histogram()

# check mean, max and min incomes
sapply(dat_inc, mean)
sapply(dat_inc, max)
sapply(dat_inc, min)

#assign domains
domains <- c(rep("d1", 500),
             rep("d2", 1000),
             rep("d3", 2500),
             rep("d4", 6000))

# randomly shuffle and assign to dataframe
dat_inc$domain <- sample(domains)


# Plotting all distributions
ggpubr::ggarrange(
  plot_lognormal,
  plot_mix
)

ggpubr::ggarrange(
  plotlist = pareto_plots
)

ggpubr::ggarrange(
  plotlist = gamma_plots
)

# select and save data
#first four distributions
dat_inc <- dat_inc[,c(1:4,9)]
#remaining four distributions (rerun script)
dat_inc <- dat_inc[,c(5:8,9)]

save(dat_inc, file = "data/dataframes.RData")
