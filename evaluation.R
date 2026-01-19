#Evaluation
library(tidyverse)


plot_data <- bind_rows(
  df1 = dat_var_1,
  df2 = dat_var_2,
  df3 = dat_var_3,
  df4 = dat_var_4,
  .id = "distribution"
)


#Calculations

# calculating relative biases
dat_fin$rel_bias_boot <- dat_fin$bias_boot / dat_fin$var_mc
dat_fin$rel_bias_jack <- dat_fin$bias_jack / dat_fin$var_mc

# calculating relative RSMEs
dat_fin$rel_RMSE_boot <- dat_fin$RSME_boot / dat_fin$var_mc
dat_fin$rel_RMSE_jack <- dat_fin$RSME_jack / dat_fin$var_mc


#Plots

#Plotting


#plotting bias for chosen distributions and all four domain sizes

pivoted_data <- dat_fin |>
  pivot_longer(
    cols = c("bias_boot", "bias_jack"),
    names_to = "method",
    values_to = "bias"
  )

plot_bias_df1 <- pivoted_data |> filter(df == 1) |>
  ggplot(aes(x = domain, y = bias, colour = method)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle(label = "Uniform") +
  ylab("bias compared to MC-benchmark")

plot_bias_df2 <- pivoted_data |> filter(df == 2) |>
  ggplot(aes(x = domain, y = bias, colour = method)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle(label = "Gamma") +
  ylab("bias compared to MC-benchmark")

plot_bias_df3 <- pivoted_data |> filter(df == 3) |>
  ggplot(aes(x = domain, y = bias, colour = method)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle(label = "Lognormal") +
  ylab("bias compared to MC-benchmark")

plot_bias_df4 <- pivoted_data |> filter(df == 4) |>
  ggplot(aes(x = domain, y = bias, colour = method)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  ggtitle(label = "Dagum") +
  ylab("bias compared to MC-benchmark")

ggpubr::ggarrange(
  plot_bias_df1,
  plot_bias_df2,
  plot_bias_df3,
  plot_bias_df4,
  nrow = 2,
  ncol = 2
)

#plotting relative bias for all four domain sizes

pivoted_data <- dat_fin |>
  pivot_longer(cols = c("rel_bias_boot", "rel_bias_jack"),
               names_to = "method",
               values_to = "rel_bias")

plot_rel_bias_df1 <- pivoted_data |> filter(df == 1) |> 
  ggplot(aes(x=domain, y=rel_bias, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Uniform") +
  ylab("relative bias compared to MC-benchmark")

plot_rel_bias_df2 <- pivoted_data |> filter(df == 2) |> 
  ggplot(aes(x=domain, y=rel_bias, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Gamma") +
  ylab("relative bias compared to MC-benchmark")

plot_rel_bias_df3 <- pivoted_data |> filter(df == 3) |> 
  ggplot(aes(x=domain, y=rel_bias, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Lognormal") +
  ylab("relative bias compared to MC-benchmark")

plot_rel_bias_df4 <- pivoted_data |> filter(df == 4) |> 
  ggplot(aes(x=domain, y=rel_bias, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Dagum") +
  ylab("relative bias compared to MC-benchmark")

ggpubr::ggarrange(
  plot_rel_bias_df1,
  plot_rel_bias_df2,
  plot_rel_bias_df3,
  plot_rel_bias_df4,
  nrow = 2,
  ncol = 2
)

#plotting relative RMSE for all four domain sizes

pivoted_data <- dat_fin |>
  pivot_longer(cols = c("rel_RMSE_boot", "rel_RMSE_jack"),
               names_to = "method",
               values_to = "rel_RMSE")

plot_rel_rmse_df1 <- pivoted_data |> filter(df == 1) |> 
  ggplot(aes(x=domain, y=rel_RMSE, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Uniform") +
  ylab("relative RMSE compared to MC-benchmark")

plot_rel_rmse_df2 <- pivoted_data |> filter(df == 2) |> 
  ggplot(aes(x=domain, y=rel_RMSE, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Gamma") +
  ylab("relative RMSE compared to MC-benchmark")

plot_rel_rmse_df3 <- pivoted_data |> filter(df == 3) |> 
  ggplot(aes(x=domain, y=rel_RMSE, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Lognormal") +
  ylab("relative RMSE compared to MC-benchmark")

plot_rel_rmse_df4 <- pivoted_data |> filter(df == 4) |> 
  ggplot(aes(x=domain, y=rel_RMSE, colour = method))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  ggtitle(label = "Dagum") +
  ylab("relative RMSE compared to MC-benchmark")

ggpubr::ggarrange(
  plot_rel_rmse_df1,
  plot_rel_rmse_df2,
  plot_rel_rmse_df3,
  plot_rel_rmse_df4,
  nrow = 2,
  ncol = 2
)

#Distribution of Variance estimates: Bootstrap
dis_var_boot_df4_d1 <- plot_data |> filter(domain == "d1") |> filter(distribution == "df4")|>
  ggplot(aes(boot))+
  geom_histogram()+
  ggtitle(label = "Uniform") +
  theme_minimal()

dis_var_boot_df4_d2 <- plot_data |> filter(domain == "d2") |> filter(distribution == "df4")|>
  ggplot(aes(boot))+
  geom_histogram()+
  ggtitle(label = "Gamma") +
  theme_minimal()

dis_var_boot_df4_d3 <- plot_data |> filter(domain == "d3") |> filter(distribution == "df4")|>
  ggplot(aes(boot))+
  geom_histogram()+
  ggtitle(label = "Lognormal") +
  theme_minimal()

dis_var_boot_df4_d4 <- plot_data |> filter(domain == "d4") |> filter(distribution == "df4")|>
ggplot(aes(boot))+
  geom_histogram()+
  ggtitle(label = "Dagum") +
  theme_minimal()

ggpubr::ggarrange(
  dis_var_boot_df4_d1,
  dis_var_boot_df4_d2,
  dis_var_boot_df4_d3,
  dis_var_boot_df4_d4,
  nrow = 2,
  ncol = 2
)
#Distribution of Variance estimates: Jackknife
dis_var_jack_df4_d1 <- plot_data |> filter(domain == "d1") |> filter(distribution == "df4")|>
  ggplot(aes(jack))+
  geom_histogram()+
  ggtitle(label = "Uniform") +
  theme_minimal()

dis_var_jack_df4_d2 <- plot_data |> filter(domain == "d2") |> filter(distribution == "df4")|>
  ggplot(aes(jack))+
  geom_histogram()+
  ggtitle(label = "Gamma") +
  theme_minimal()

dis_var_jack_df4_d3 <- plot_data |> filter(domain == "d3") |> filter(distribution == "df4")|>
  ggplot(aes(jack))+
  geom_histogram()+
  ggtitle(label = "Lognormal") +
  theme_minimal()

dis_var_jack_df4_d4 <- plot_data |> filter(domain == "d4") |> filter(distribution == "df4")|>
  ggplot(aes(jack))+
  geom_histogram()+
  ggtitle(label = "Dagum") +
  theme_minimal()

ggpubr::ggarrange(
  dis_var_jack_df4_d1,
  dis_var_jack_df4_d2,
  dis_var_jack_df4_d3,
  dis_var_jack_df4_d4,
  nrow = 2,
  ncol = 2
)