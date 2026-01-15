# code for simulation study

load("data/dataframes.RData")
library(tidyverse)
library(emdi)
library(ineq)

df <- 4 # number of dataframes
R <- 1000 # number of monte carlo iterations
n_d <- c(d1 = 50, d2 = 100, d3 = 250, d4 = 600) # domain sample sizes
base_seed <- 12345 # base seed to adjust later for reproducibility

# level 1: four different dataframes
for (i in 1:df) {
  
  # creation of an empty dataframe to store monte-carlo results
  dat_var_df <- data.frame(R = rep(1:R, each = length(n_d)))
  
  # filtered dataframe just with income for column i and the domain information
  dat_pop <- dat_inc |> 
    select(all_of(c(i, 5)))
  
  # level 2: R monte carlo iterations
  for (j in 1:R) {
    
    # preparation of merged dataframe later
    names(dat_pop)[1] <- "inc"
    dat_d_sampled <- data.frame(inc = numeric(0), domain = character(0))
    
    # level 3: sampling for four domains
    for (k in 1:length(n_d)) {
      
      # adjusted seed for variation in sampling and reproducibility at the same time
      set.seed(base_seed + j*100 + k)
      
      # dataframe solely with domain k
      dat_d <- dat_pop |> 
        filter(domain == names(n_d)[k])
      
      # sample from domain d of size n_d
      sample_d <- dat_d[sample(nrow(dat_d), size = n_d[k], replace = F), ]
      
      # save of sample into one big dataframe
      dat_d_sampled <- rbind(dat_d_sampled, sample_d)
      
    } # end loop level 3
    
    # creation of an empty dataframe to store variance estimation results
    var_r <- data.frame(domain = names(n_d), boot = NA, jack = NA)
    
    # bootstrap variance estimation using emdi and storage in var_r
    var_boot <- emdi::direct(y = "inc",
                             smp_data = dat_d_sampled,
                             smp_domains = "domain",
                             var = TRUE,
                             B = 100)
    var_r$boot <- var_boot$MSE$Gini
    
    # jackknife variance estimation, manually
    # level 4: jackknife for each domain
    for (l in 1:length(n_d)) {
      
      # filtered dataframe just for domain l
      d_j_domain <- dat_d_sampled |> 
        filter(domain == names(n_d)[l])
      
      # extraction of earlier calculated gini per domain l
      g_d <- var_boot$ind$Gini[l]
      var_r$gini[l] <- g_d
      
      # preparation of empty dataframe for jackknife variance estimation
      d_var_jack <- data.frame(g_jack = numeric(nrow(d_j_domain)))
      
      # level 5: calculation of the jackknife variance estimator per domain
      for (m in 1:nrow(d_j_domain)) {
        
        # jackknife dataframe, excluding m-th observation
        d_jack <- d_j_domain[-m, ]
        
        # gini for jackknife dataframe
        g_jack <- ineq::Gini(d_jack$inc)
        
        d_var_jack$g_jack[m] <- g_jack
        
      } # end loop level 5
      
      # calculating mean of g_jack
      g_jack_mean <- mean(d_var_jack$g_jack)
      
      # calculating squared difference
      d_var_jack$diff_sq <- (d_var_jack$g_jack - g_jack_mean)^2
      
      # calculating jackknife variance estimator per domain
      A <- nrow(d_j_domain) - 1
      jack_d <- ((A - 1) / A) * sum(d_var_jack$diff_sq)
      
      # adding the estimator to the existing dataframe with the bootstrap estimators
      var_r$jack[l] <- jack_d
      
      
    } # end loop level 4
    
    dat_var_df[dat_var_df$R == j, names(var_r)] <-  var_r

    
  } # end loop level 2
  
  name_df <- paste0("dat_var_", i)
  
  assign(name_df, dat_var_df)
  
} # end loop level 1

# saving data (for final dataset, remove "test" at the end of the file name)
save(dat_var_1, dat_var_2, dat_var_3, dat_var_4, file = "data/simulation_data_test.RData")


######### end of simulation ########################################################################################
####################################################################################################################
######### start of performance evaluation ##########################################################################

# benchmarking: design variance

load(file = "data/simulation_data_test.RData")
df <- 4 # number of dataframes, must match df above
R <- 3 # number of monte carlo iterations, must match R above
n_d <- c(d1 = 50, d2 = 100, d3 = 250, d4 = 600) # domain sample sizes, must match n_d above

# level 1: iteration over dataframes
for (i in 1:df) {
  
  # creation of a temporary df for dataframe i
  name_df <- paste0("dat_var_", i)
  tmp_df <- get(name_df)
  
  # df for mean gini over all iterations R per domain
  g_mean <- tmp_df |> 
    group_by(domain) |> 
    summarise(mean = mean(gini))
  
  # level 2: iteration over domains
  for (j in 1:length(n_d)) {
    
    # extraction of mean gini per domain
    g_bar <- g_mean$mean[j]
    
    # extraction of all rows with domain j
    rows <- tmp_df$domain == paste0("d", j) 
    
    # calculation of squared difference for rows of domain j
    tmp_df$sq_diff[rows] <- (tmp_df$gini[rows] - g_bar)^2
    
  } # end loop level 2
  
  # extraction of df's
  name_df1 <- paste0("dat_des_", i)
  assign(name_df1, tmp_df)
} # end loop level 1

# creation of an empty dataframe for final overview
dat_fin <- data.frame(df = rep(1:4, each = 4), 
                      domain = seq(1, 4, 1))

# level 1: iteration over dataframes
for (i in 1:df) {
  
  # defining dataframe for dataframe i
  name_df <- paste0("dat_des_", i)
  tmp_df <- get(name_df)
  
  # level 2: iteration over domains
  for (j in 1:length(n_d)) {
    
    # temporary dataset just with domain j
    tmp_df_d <- tmp_df[tmp_df$domain == paste0("d", j), ]
    
    # calculation of design variance for domain j and dataframe i
    var_mc_d <- (1 / (R - 1)) * sum(tmp_df_d$sq_diff)
    
    # saving variance in dataframe
    dat_fin[dat_fin$df == i & dat_fin$domain == j, "var_mc"] <- var_mc_d
    
  }  # end loop level 2
  
} # end loop level 1

# calculation of bias and RSME

# level 1: iteration over df's
for (i in 1:df) {
  
  # temporary dataframe with dataframe i
  name_df <- paste0("dat_var_", i)
  tmp_df <- get(name_df)
  
  # level 2: iteration over domains
  for (j in 1:length(n_d)) {
    
    # extraction of monte carlo design variance for df i and domain j
    var_mc_ij <- dat_fin[dat_fin$df == i & dat_fin$domain == j, "var_mc"]
    
    # temporary dataframe with only domain j
    tmp_df_d <- tmp_df[tmp_df$domain == paste0("d", j), ]
    
    # calculation of difference in variance and bias incl. extraction to dat_fin for bootstrap
    tmp_df_d$diff_boot <- tmp_df_d$boot - var_mc_ij
    dat_fin[dat_fin$df == i & dat_fin$domain == j, "bias_boot"] <- (1/R) * sum(tmp_df_d$diff_boot)
    
    # calculation of difference in variance and bias incl. extraction to dat_fin for jackknife
    tmp_df_d$diff_jack <- tmp_df_d$jack - var_mc_ij
    dat_fin[dat_fin$df == i & dat_fin$domain == j, "bias_jack"] <- (1/R) * sum(tmp_df_d$diff_jack)
    
    # calculation of squared difference and RSME incl. extraction to dat_fin for bootstrap
    tmp_df_d$sq_diff_boot <- (tmp_df_d$boot - var_mc_ij) ^ 2
    dat_fin[dat_fin$df == i & dat_fin$domain == j, "RSME_boot"] <- sqrt((1/R) * sum(tmp_df_d$sq_diff_boot))
    
    # calculation of squared difference and RSME incl. extraction to dat_fin for jackknife
    tmp_df_d$sq_diff_jack <- (tmp_df_d$jack - var_mc_ij) ^ 2
    dat_fin[dat_fin$df == i & dat_fin$domain == j, "RSME_jack"] <- sqrt((1/R) * sum(tmp_df_d$sq_diff_jack))
    
  } # end loop level 2
  
} # end loop level 1

