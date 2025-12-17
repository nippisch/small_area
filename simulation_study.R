# code for simulation study

load("data/dataframes.RData")

# Simulation round 1

R <- 10000

for (i in 1:R) {
  
  dat_v1 <- dat_inc[1]
  
  dat_mc <- dat_v1[sample(nrow(dat_v1), size = 1000, replace = FALSE), ]
  
  
}
