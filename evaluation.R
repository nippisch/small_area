#Evaluation
library(tidyverse)


plot_data <- bind_rows(
  dis1 = dat_var_1,
  dis2 = dat_var_2,
  dis3 = dat_var_3,
  dis4 = dat_var_4,
  .id = "distribution"
)

#plotting variances per iteration

boot_per_iteration <-
plot_data |>
  filter(domain == "d1") |>
  ggplot(aes(x = R, y = boot, colour = distribution)) +
  geom_line() +
  theme_minimal() +
  ylab("bootstrap variance") +
  xlab("iterations")

jack_per_iteration <-
  plot_data |>
  filter(domain == "d1") |>
  ggplot(aes(x = R, y = jack, colour = distribution)) +
  geom_line() +
  theme_minimal() +
  ylab("jackknife variance") +
  xlab("iterations")

#plotting bias for one chosen distribution and all four domain sizes

pivoted_data <- dat_fin |>
  pivot_longer(cols = c("bias_boot", "bias_jack"),
               names_to = "method",
               values_to = "bias")

plot_bias <- pivoted_data |> filter(df == 3) |> #choose distribution
  ggplot(aes(x=domain, y=bias, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ylab("bias compared to MC-benchmark")
plot_bias

#plotting bias for all distributions for one chosen domain

pivoted_data <- dat_fin |>
  pivot_longer(cols = c("bias_boot", "bias_jack"),
               names_to = "method",
               values_to = "bias")

#assign distribution names (adjust if other distributions are chosen)
pivoted_data$df <- recode(
  pivoted_data$df,
  `1` = "uniform",
  `2` = "gamma",
  `3` = "lognormal",
  `4` = "jenkins"
)

plot_bias <- pivoted_data |> filter(domain == 4) |> #choose domain
  ggplot(aes(x = df, y = bias, colour = method)) +
  geom_point(size = 4) +
  theme_minimal() +
  ylab("bias compared to MC-benchmark")
plot_bias
