##test for generate_abc_sample to make sure it works##

library(testthat)

#original function
prior_distribution <- function() {
  return(runif(2, 0, 1))
}

calculate_summary_stats <- function(sim_data_vector, reference_matrix) {
  sim_counts <- table(
    factor(sim_data_vector, levels = rownames(reference_matrix)),
    factor(hh_sizes_global, levels = colnames(reference_matrix)) 
  )
  return(as.vector(sim_counts))
}

generate_abc_sample <- function(observed_data_vector,
                                summary_statistic_fn,
                                prior_distribution_fn,
                                data_generating_fn,
                                epsilon) {
  while(TRUE) {
    theta <- prior_distribution_fn() 
    qc_val <- theta[1]
    qh_val <- theta[2]
    y <- data_generating_fn(qc_val, qh_val)
    stat_sim <- summary_statistic_fn(y)
    stat_obs <- observed_data_vector
    dist <- sqrt(sum((stat_sim - stat_obs)^2))
    if(dist < epsilon) {
      return(theta) 
    }
  }
}



## test ##

prior_dist_test <- function() { #our test parameter values
  c(0.5, 0.5) #50% chance of escaping infection from household & community
}

data_gen_test <- function(qc, qh) { #simulating data
  rep(qc+qh, 5)
}

summary_stat_test <- function(y) { #defining a summary statistic
  sum(y)
}

stat_test <- 5 #defining "real data" that we are comparing our summary stat to 
  #(setting it to the same value as the summary stat)


test_that("generate_abc_sample works", { #running the abc
  epsilon <- 1 #acceptance threshold
  theta <- generate_abc_sample(
    observed_data_vector = stat_test,
    summary_statistic_fn = summary_stat_test,
    prior_distribution_fn = prior_dist_test,
    data_generating_fn = data_gen_test,
    epsilon = epsilon
  )
  sim_data <- data_gen_test(theta[1], theta[2]) #simulated data (1, 1, 1, 1, 1)
  sim_stat <- summary_stat_test(sim_data) #parameter from simulated data (5)
  dist <- sqrt(sum((sim_stat - stat_test)^2)) #computing the euclidean distance
  expect_true(dist < epsilon) #distance between the stat summaries should be less than 1 (0)
})