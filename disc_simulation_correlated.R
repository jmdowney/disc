# 1. setup ####
library(SimEngine)

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

## d. helper functions ####

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

# 3. create model ####

# set values for mean outcome at baseline and follow-up, treatment effect the same;
# starting with simplest design of mean outcome at baseline and follow-up 
# among untreated group = 0
fit_model <- function(all_sampled_individuals, all_sampled_clusters, te = 1) {
  
  final_sample <- all_sampled_individuals %>% 
    # join individual and cluster level data
    left_join(all_sampled_clusters, by = 'individual_matching_id') %>% 
    select(-individual_matching_id) %>% 
    # calculate outcomes
    mutate(treatment_effect = te) %>% 
    mutate(outcome = 0 + cluster_effect + treatment_effect*intervention + individual_residual)
  
  # fit model 
  model <- lm(outcome ~ intervention*time, data = final_sample)
  # get estimate of intervention effect
  summary <- summary(model)
  return(summary$coefficients[4,1])
  
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
  dat <- create_clusters(1000, sd, mean = 'correlated')
  all_sampled_clusters <- sample_clusters(dat, L$n_clusters, design = L$design)
  all_sampled_individuals <- sample_individuals(all_sampled_clusters, 25)
  estimate <- fit_model(all_sampled_individuals, all_sampled_clusters, te = 1)
  return (list("estimate"=estimate, "sd"=sd))
})

sim %<>% set_config(
  num_sim = 10,
  packages = c("tidyverse", "lme4", "stringr")
)

# 6. run simulation, summarize, save #### 
sim %<>% run()
sim %>% SimEngine::summarize(
  list(stat = "bias", estimate = "estimate", truth = 1)
)

save(sim, file = "simulation_results_correlated.RData")

