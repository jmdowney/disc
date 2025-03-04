# 1. setup ####
library(SimEngine)
library(DRDID)

# 2. create data & helper functions ####

## a. generate "population" of clusters ####
create_clusters <- function(n, sd, mean) {
  
  # if mean = 'correlated':
  if (mean == 'correlated') {
    
    # randomly assign half to control half to treatment
    control <- rep(0, n/2)
    treatment <- rep(1, n/2)
    intervention <- sample(c(control, treatment))
    assignment_df <- data.frame(intervention) %>% 
      mutate(cluster_effect = rnorm(n = n, mean = intervention + 1, sd = sd))
    
    # create df including cluster effect, intervention vs control assignment, and cluster ID
    dat <- assignment_df %>% 
      tibble::rowid_to_column("cluster_id")
    
    return(dat)
  
  # if mean = 'uncorrelated':
  } else if (mean == 'uncorrelated') {
    
    # randomly assign half to control half to treatment
    control <- rep(0, n/2)
    treatment <- rep(1, n/2)
    intervention <- sample(c(control, treatment))
    assignment_df <- data.frame(intervention) %>% 
      mutate(cluster_effect = rnorm(n = n, mean = 0, sd = sd))
    
    # create df including cluster effect, treatment vs control assignment, and cluster ID
    dat <- assignment_df %>% 
      tibble::rowid_to_column("cluster_id")
    
    return(dat)
    
  } else {
    
    stop("mean can only be 'correlated' (meaning cluster effect has mean of 1 in control and 2 in intervention areas) or 'uncorrelated' (meaning cluster effect has mean of 0)")
    
  }
}
  
## b. sample clusters at baseline and endline for each design ####
sample_clusters <- function(dat, n, design) {
  
  # separate designs for traditional RCS vs DISC
  if (design == 'Traditional RCS') {
    
    # traditional repeated cross sectional design, baseline
    sample_baseline <- dat %>% 
      # ensure half of clusters sampled are intervention and half control
      group_by(intervention) %>% 
      slice_sample(n = n/2) %>% 
      mutate(time = 0)
  
    # traditional repeated cross sectional design, endline
    sample_endline <- dat %>% 
      # ensure half of clusters sampled are intervention and half control
      group_by(intervention) %>% 
      slice_sample(n = n/2) %>% 
      mutate(time = 1)
    
    # combine into one df
    all_sampled_clusters <- bind_rows(
      sample_baseline,
      sample_endline
    ) %>% 
      # add another ID, to match individuals to clusters
      tibble::rowid_to_column("individual_matching_id")  %>% 
      mutate(design = 'Traditional RCS')
    
    return(all_sampled_clusters)
    
  } else if (design == 'DISC') {
    
    # DISC design, baseline
    sample_baseline <- dat %>% 
      # ensure half of clusters sampled are intervention and half control
      group_by(intervention) %>% 
      slice_sample(n = n/2) %>% 
      mutate(time = 0)
    
    # DISC design, endline - same as DISC baseline
    sample_endline <- sample_baseline %>% 
      mutate(time = 1) 
    
    # now bind into one df to facilitate individual sampling
    all_sampled_clusters <- bind_rows(
      sample_baseline,
      sample_endline
    ) %>% 
      # add another ID, to match individuals to clusters
      tibble::rowid_to_column("individual_matching_id") %>% 
      mutate(design = 'DISC')
    
    return(all_sampled_clusters)
    
  } else {
    
    stop("design can only be 'Traditional RCS' or 'DISC'")
    
  }
}

## c. sample individuals from each cluster ####
sample_individuals <- function(all_sampled_clusters, n) {
  
  # initialize empty list for individuals
  individual_samples <- list()
  
  # loop through all sampled clusters to "sample" individuals for each one
  for (individual_matching_id in all_sampled_clusters$individual_matching_id) {
    # generate individual samples
    samples <- rnorm(n = n, 0, 1)
    # create df with cluster ID, individual ID, and individual samples
    df <- tibble(individual_matching_id = individual_matching_id, individual_id = 1:n, individual_residual = samples)
    # append to list 
    individual_samples[[individual_matching_id]] <- df
  }
  
  # combine into one df
  all_sampled_individuals <- tibble(bind_rows(individual_samples))
  
  return(all_sampled_individuals)
  
}

## d. create final df ####
# set values for mean outcome at baseline and follow-up, treatment effect the same;
# starting with simplest design of mean outcome at baseline and follow-up 
# among untreated group = 0
create_final_data <- function(all_sampled_individuals, all_sampled_clusters, te = 1, binary_effect = 0, uniform_effect = 0) {
  
  # generate covariates
  binary_covariate <- rbinom(nrow(all_sampled_individuals), size = 1, prob = 0.5)
  uniform_covariate <- runif(nrow(all_sampled_individuals), min = 0, max = 1 )
  
  final_sample <- all_sampled_individuals %>% 
    # join individual and cluster level data
    left_join(all_sampled_clusters, by = 'individual_matching_id') %>% 
    select(-individual_matching_id) %>% 
    # calculate outcomes
    mutate(treatment_effect = te) %>% 
    mutate(outcome = 0 + cluster_effect + treatment_effect*intervention*time + 
             time + binary_effect*binary_covariate + 
             uniform_effect*uniform_covariate + individual_residual) %>% 
    # add covariates to final sample df
    mutate(binary_covariate = binary_covariate,
           uniform_covariate = uniform_covariate)
  
  return(final_sample)
  
}

## e. helper functions ####

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# get ICC from SD 
get_icc <- function(sd) {
  
  icc <- sd^2/(sd^2 + 1)
  return(icc)
  
}

# 3. create models ####

# linear
fit_model_lm <- function(final_sample) {
  
  # fit model 
  model <- lm(outcome ~ intervention*time, data = final_sample)
  # get estimate of intervention effect
  summary <- summary(model)
  return(summary$coefficients[4,1])
  
}

# DRDID
fit_model_drdid <- function(final_sample) {
  
  # fit model 
  invisible(model <- drdid_rc(y = final_sample$outcome,
                              post = final_sample$time,
                              D = final_sample$intervention,
                              covariates = final_sample[, c("binary_covariate", "uniform_covariate")]
  )
  )
  # get estimate of intervention effect
  return(model$ATT)
  
}

# 4. set simulation levels ####

# create new simulation #
sim <- new_sim()

# different values of ICC + different designs (traditional vs disc)
sim %<>% set_levels(
  icc = seq(from = 0, to = 0.2, by = 0.05),
  design = c("Traditional RCS", "DISC"),
  n_clusters = seq(from = 10, to = 100, by = 10)
)

# 5. create simulation script ####
sim %<>% set_script(function() {
  sd <- get_sd(icc = L$icc)
  dat <- create_clusters(1000, sd, mean = 'uncorrelated')
  all_sampled_clusters <- sample_clusters(dat, L$n_clusters, design = L$design)
  all_sampled_individuals <- sample_individuals(all_sampled_clusters, 25)
  final_data <- create_final_data(all_sampled_individuals, all_sampled_clusters)
  final_data_large_uniform <- create_final_data(all_sampled_individuals, all_sampled_clusters, uniform_effect = 10, binary_effect = 0.5)
  linear_estimate <- fit_model_lm(final_data)
  drdid_estimate <- fit_model_drdid(final_data)
  linear_estimate_large_uniform <- fit_model_lm(final_data_large_uniform)
  drdid_estimate_large_uniform <- fit_model_drdid(final_data_large_uniform)
  return (list("linear_estimate"=linear_estimate, 
               "drdid_estimate" = drdid_estimate, 
               "linear_estimate_large_uniform" = linear_estimate_large_uniform,
               "drdid_estimate_large_uniform" = drdid_estimate_large_uniform,
               "sd"=sd))
})

sim %<>% set_config(
  num_sim = 1000,
  packages = c("tidyverse", "lme4", "stringr")
)

# 6. run simulation, summarize, save #### 
sim %<>% run()
save(sim, file = "simulation_results_linear_drdid.RData")

# linear
sim %>% SimEngine::summarize(
  list(stat = "sd", x = "linear_estimate")
)

# DRDID
sim %>% SimEngine::summarize(
  list(stat = "sd", x = "drdid_estimate")
)

# linear large uniform effect
sim %>% SimEngine::summarize(
  list(stat = "sd", x = "linear_estimate_large_uniform")
)

# DRDID large uniform effect
sim %>% SimEngine::summarize(
  list(stat = "sd", x = "drdid_estimate_large_uniform")
)