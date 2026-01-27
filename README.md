# Bootstrap vs. Jackknife Variance Estimation for the Gini Coefficient - A Monte Carlo Study

This repository belongs to a project in context of the course "Small Area Estimation" at the University of Bamberg.

In this simulation study, we investigate whether the resampling variance estimation methods Bootstrap and Jackknive behave differently when it comes to varying income distributions and domain sample sizes. Therefore, we apply the Bootstrap variance estimation of the package `emdi` and an own leave-one-out Jackknife variance estimation.

We find rather subtle differences between the two methods: Whereas Bootstrap performs better in terms of relative RMSE for small sample sizes, Jackknife's performance increases with increasing sample size.
