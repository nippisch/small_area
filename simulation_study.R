# code for simulation study

load("data/dataframes.RData")
library(tidyverse)
library(emdi)
library(ineq)

df <- 4 # number of dataframes
R <- 100 # number of monte carlo iterations
n_d <- c(d1 = 50, d2 = 100, d3 = 250, d4 = 600) # domain sample sizes

# level 1: four different dataframes
for (i in 1:df) {
  
  # filtered dataframe just with income for column i and the domain information
  dat_pop <- dat_inc |> 
    select(i, 5)
  
  dat_var_df <- data.frame(R = rep(1:R, each = 4))
  
  # level 2: 100 monte carlo iterations
  for (j in 1:R) {
    
    # preparation of merged dataframe later
    names(dat_pop)[1] <- "inc"
    dat_d_sampled <- data.frame(inc = numeric(0), domain = character(0))
    # level 3: sampling for four domains
    for (k in 1:length(n_d)) {
      
      # dataframe solely with domain k
      dat_d <- dat_pop |> 
        filter(domain == names(n_d)[k])
      
      # sample from domain d of size n_d
      sample_d <- dat_d[sample(nrow(dat_d), size = n_d[k], replace = F), ]
      
      # save of sample into one big dataframe
      dat_d_sampled <- rbind(dat_d_sampled, sample_d)
      
    } # end loop level 3
    var_r <- data.frame(domain = names(n_d), boot = NA)
    # bootstrap variance estimation using emdi
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
      
      # population gini per domain
      g_d <- ineq::Gini(d_j_domain$inc)
      
      var_r$gini[l] <- g_d
      
      d_var_jack <- data.frame(g_d = rep(g_d, nrow(d_j_domain)))
      
      # level 5: calculation of the jackknife variance estimator per domain
      for (m in 1:nrow(d_j_domain)) {
        
        # jackknife dataframe, excluding m-th observation
        d_jack <- d_j_domain[-m, ]
        
        # gini for jackknife dataframe
        g_jack <- ineq::Gini(d_jack$inc)
        
        d_var_jack$g_jack[m] <- g_jack
        
      } # end loop level 5
      
      # calculating squared difference
      d_var_jack$diff_sq <- (g_jack - g_d)^2
      
      # calculating jackknife variance estimator per domain
      A <- nrow(d_j_domain) -1
      jack_d <- ((A - 1) / A) * sum(d_var_jack$diff_sq)
      
      # adding the estimator to the existing dataframe with the bootstrap estimators
      var_r$jack[l] <- jack_d
      
      
    } # end loop level 4
    
    dat_var_df[dat_var_df$R == j, names(var_r)] <-  var_r

    
  } # end loop level 2
  
  name_df <- paste0("dat_var_", i)
  
  assign(name_df, dat_var_df)
  
} # end loop level 1

