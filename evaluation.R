#Evaluation
library(tidyverse)
library(emdi)

load("Workspace Simulation.RData")
rm(d_j_domain, d_jack, d_var_jack, dat_d, dat_d_sampled, dat_pop, dat_var_df, g_mean, sample_d, tmp_df, tmp_df_d, var_boot, var_r, A, 
   base_seed, df, g_bar, g_d, g_jack, g_jack_mean, i, j, jack_d, k, l, m, n_d, name_df, name_df1, R, rows, var_mc_d, var_mc_ij, dat_des_1, 
   dat_des_2, dat_des_3, dat_des_4)

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

#Calculating true gini coefficients and coverage

gini_uniform <- emdi::direct(y = "uniform" ,
             smp_data = dat_inc,
             smp_domains = "domain")
gini_true_uniform <- data.frame(gini_true = gini_uniform$ind$Gini,
                              domain = c("d1", "d2", "d3", "d4"))

gini_gamma <- emdi::direct(y = "gamma" ,
                             smp_data = dat_inc,
                             smp_domains = "domain")
gini_true_gamma <- data.frame(gini_true = gini_gamma$ind$Gini,
                              domain = c("d1", "d2", "d3", "d4"))

gini_lognormal <- emdi::direct(y = "lognormal" ,
                             smp_data = dat_inc,
                             smp_domains = "domain")
gini_true_lognormal <- data.frame(gini_true = gini_lognormal$ind$Gini,
                              domain = c("d1", "d2", "d3", "d4"))

gini_dagum <- emdi::direct(y = "dagum" ,
                             smp_data = dat_inc,
                             smp_domains = "domain")

gini_true_dagum <- data.frame(gini_true = gini_dagum$ind$Gini,
                              domain = c("d1", "d2", "d3", "d4"))

#function for coverage
calc_coverage <- function(dat_var, gini_true, alpha = 0.05) {
  
  z <- qnorm(1 - alpha / 2)
  
  dat_var |>
    left_join(
      gini_true,
      by = "domain"
    ) |>
    mutate(
      # Bootstrap CI
      l_boot = gini - z * sqrt(boot),
      u_boot = gini + z * sqrt(boot),
      cover_boot = gini_true >= l_boot & gini_true <= u_boot,
      
      # Jackknife CI
      l_jack = gini - z * sqrt(jack),
      u_jack = gini + z * sqrt(jack),
      cover_jack = gini_true >= l_jack & gini_true <= u_jack
    ) |>
    group_by(domain) |>
    summarise(
      coverage_boot = mean(cover_boot),
      coverage_jack = mean(cover_jack),
      .groups = "drop"
    )
}
#coverages for chosen distributions
cov_uni <- calc_coverage(dat_var_1, gini_true_uniform)
cov_gamma <- calc_coverage(dat_var_2, gini_true_gamma)
cov_lognormal <- calc_coverage(dat_var_3, gini_true_lognormal)
cov_dagum <- calc_coverage(dat_var_4, gini_true_dagum)


cov_uni <- cov_uni |> mutate(domain = recode(
  domain,
  "d1" = 1,"d2" = 2,"d3" = 3,"d4" = 4
))
cov_gamma <- cov_gamma |> mutate(domain = recode(
  domain,
  "d1" = 1,"d2" = 2,"d3" = 3,"d4" = 4
))
cov_lognormal <- cov_lognormal |> mutate(domain = recode(
  domain,
  "d1" = 1,"d2" = 2,"d3" = 3,"d4" = 4
))
cov_dagum <- cov_dagum |> mutate(domain = recode(
  domain,
  "d1" = 1,"d2" = 2,"d3" = 3,"d4" = 4
))
cov_uni$df <- rep(1)
cov_gamma$df <- rep(2)
cov_lognormal$df <- rep(3)
cov_dagum$df <- rep(4)

dat_cov <- rbind(cov_uni, cov_gamma, cov_lognormal, cov_dagum)
dat_fin <- merge(dat_fin, dat_cov, by=c("df", "domain"))
#Plots

#Coverage Plot
pivoted_data <- dat_fin|>pivot_longer(
  cols = c("coverage_boot", "coverage_jack"),
  names_to = "method",
  values_to = "coverage"
)
pivoted_data <- pivoted_data |>
  dplyr::mutate(method = factor(method, levels = c("coverage_boot", "coverage_jack"),
                                labels = c("Bootstrap", "Jackknife")),
                df = factor(
                  df,
                  levels = c(1, 2, 3, 4),
                  labels = c("Uniform", "Gamma", "Lognormal", "Dagum")
                ))

pivoted_data$covdummy <- as.factor(ifelse(pivoted_data$coverage > 0.95, 1, 0))


plot_CI <- pivoted_data |>
  ggplot(aes(
    x = domain,
    y = coverage,
    fill = covdummy
  ))+ guides(fill = "none")+
  geom_col(width = 0.7) +
  geom_hline(
    yintercept = 0.95,
    linewidth = 0.4,
    colour = "#009E73", alpha=0.6
  ) +
  facet_grid(rows = vars(method), cols = vars(df)) +
  scale_fill_manual(values = c("0" = "firebrick", "1" = "#009E73")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))+
  theme(axis.title.y = element_blank())+
  labs(x="Domain")

ggsave(
  "Plot_CI.pdf",
  plot_CI,
  width = 7,
  height = 4
)
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

#plotting relative bias

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

#plotting relative RMSE
pivoted_data <- dat_fin |>
  pivot_longer(
    cols = c(rel_RMSE_boot, rel_RMSE_jack),
    names_to = "method",
    values_to = "rel_RMSE"
  ) |>
  mutate(df = factor(
    df,
    levels = 1:4,
    labels = c("Uniform", "Gamma", "Lognormal", "Dagum")
  ),
  method = factor(method, labels = c("Bootstrap", "Jackknife")))

plot_rel_rmse <- ggplot(pivoted_data,
                        aes(
                          x = domain,
                          y = rel_RMSE,
                          colour = method
                        )) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap( ~ df, nrow = 2) +
  scale_colour_manual(name = "Method",
                      values = c(
                        "Bootstrap"  = "#E69F00",
                        "Jackknife"  = "#56B4E9"  
                      )) +
  scale_y_continuous(limits = c(0, 0.7),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Domain", y = "Relative RMSE") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text   = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank()
  )

plot_rel_rmse
ggsave(
  "relative_RMSE.pdf",
  plot_rel_rmse,
  width = 7,
  height = 4
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
