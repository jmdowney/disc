# 1. setup ####
library(SimEngine)
library(DRDID)
library(dplyr)

# 2. create data & helper functions ####

## a. generate "population" of level 1 clusters ####
create_level1_clusters <- function(n, sd, mean) {
  
  # if mean = 'correlated':
  if (mean == 'correlated') {
    
    # randomly assign half to control half to treatment
    control <- rep(0, n/2)
    treatment <- rep(1, n/2)
    intervention <- sample(c(control, treatment))
    assignment_df <- data.frame(intervention) %>% 
      mutate(level1_cluster_effect = rnorm(n = n, mean = intervention + 1, sd = sd))
    
    # create df including cluster effect, intervention vs control assignment, and cluster ID
    dat <- assignment_df %>% 
      tibble::rowid_to_column("level1_cluster_id")
    
    return(dat)
  
  # if mean = 'uncorrelated':
  } else if (mean == 'uncorrelated') {
    
    # randomly assign half to control half to treatment
    control <- rep(0, n/2)
    treatment <- rep(1, n/2)
    intervention <- sample(c(control, treatment))
    assignment_df <- data.frame(intervention) %>% 
      mutate(level1_cluster_effect = rnorm(n = n, mean = 0, sd = sd))
    
    # create df including cluster effect, treatment vs control assignment, and cluster ID
    dat <- assignment_df %>% 
      tibble::rowid_to_column("level1_cluster_id")
    
    return(dat)
    
  } else {
    
    stop("mean can only be 'correlated' (meaning cluster effect has mean of 1 in control and 2 in intervention areas) or 'uncorrelated' (meaning cluster effect has mean of 0)")
    
  }
}

## b. generate level 2 clusters within level 1 clusters ####
create_level2_clusters <- function(level1_clusters, n_level2_per_level1, sd_level2) {
  
  level2_clusters <- list()
  
  for (level1_id in level1_clusters$level1_cluster_id) {
    level1_row <- level1_clusters %>% filter(level1_cluster_id == level1_id)
    
    # Create level 2 clusters within this level 1 cluster
    level2_data <- data.frame(
      level1_cluster_id = rep(level1_id, n_level2_per_level1),
      intervention = rep(level1_row$intervention, n_level2_per_level1),
      level1_cluster_effect = rep(level1_row$level1_cluster_effect, n_level2_per_level1),
      level2_cluster_effect = rnorm(n = n_level2_per_level1, mean = 0, sd = sd_level2)
    ) %>% 
      tibble::rowid_to_column("level2_cluster_id") %>%
      mutate(level2_cluster_id = paste0(level1_id, "_", level2_cluster_id))
    
    level2_clusters[[level1_id]] <- level2_data
  }
  
  return(bind_rows(level2_clusters))
}

## c. sample level 1 clusters at baseline and endline for each design ####
sample_level1_clusters <- function(dat, n, design) {
  
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
    all_sampled_level1_clusters <- bind_rows(
      sample_baseline,
      sample_endline
    ) %>% 
      # create composite ID: cluster_time
      mutate(composite_id = paste0(level1_cluster_id, "_", time)) %>% 
      mutate(design = 'Traditional RCS')
    
    return(all_sampled_level1_clusters)
    
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
    all_sampled_level1_clusters <- bind_rows(
      sample_baseline,
      sample_endline
    ) %>% 
      # create composite ID: cluster_time
      mutate(composite_id = paste0(level1_cluster_id, "_", time)) %>% 
      mutate(design = 'DISC')
    
    return(all_sampled_level1_clusters)
    
  } else {
    
    stop("design can only be 'Traditional RCS' or 'DISC'")
    
  }
}

## d. sample level 2 clusters for three-level designs ####
sample_level2_clusters <- function(all_sampled_level1_clusters, level2_clusters, n_level2_per_level1, sampling_scenario, design) {
  
  if (sampling_scenario == "Two_Level") {
    # For two-level design, return NULL - no level 2 sampling
    return(NULL)
    
  } else if (sampling_scenario == "DDD") {
    # Different level 1, different level 2, different individuals (three-level RCS)
    # For Traditional RCS at level 1, we need to sample different level 2 at each timepoint
    
    level2_samples <- list()
    
    # Process each level 1 cluster_time combination separately (baseline and endline are different clusters for RCS)
    for (i in seq_len(nrow(all_sampled_level1_clusters))) {
      level1_row <- all_sampled_level1_clusters[i, ]
      level1_id <- level1_row$level1_cluster_id
      
      # Get all level 2 clusters for this level 1 cluster
      available_level2 <- level2_clusters %>% filter(level1_cluster_id == level1_id)
      
      # Sample level 2 clusters
      sampled_level2_ids <- available_level2 %>% 
        slice_sample(n = n_level2_per_level1) %>% 
        pull(level2_cluster_id)
      
      sample_per_timepoint <- available_level2 %>% 
        filter(level2_cluster_id %in% sampled_level2_ids) %>%
        mutate(time = level1_row$time,
               composite_id = paste0(level2_cluster_id, "_", level1_row$time))
      
      level2_samples[[i]] <- sample_per_timepoint
    }
    
    return(bind_rows(level2_samples))
    
  } else if (sampling_scenario == "SSD") {
    # Same level 1 clusters, same level 2 clusters, different individuals
    # Only valid for DISC design
    
    level2_samples <- list()
    
    # Get unique level 1 clusters (process each cluster once, not each cluster_time)
    unique_clusters <- all_sampled_level1_clusters %>% 
      filter(time == 0) %>%  # Since this is always DISC, just take baseline rows to get unique clusters
      select(level1_cluster_id, intervention, level1_cluster_effect)
    
    for (i in seq_len(nrow(unique_clusters))) {
      level1_id <- unique_clusters$level1_cluster_id[i]
      
      # Get all level 2 clusters for this level 1 cluster
      available_level2 <- level2_clusters %>% filter(level1_cluster_id == level1_id)
      
      # Sample level 2 clusters once for this level 1 cluster
      sampled_level2_ids <- available_level2 %>% 
        slice_sample(n = n_level2_per_level1) %>% 
        pull(level2_cluster_id)
      
      sampled_level2 <- available_level2 %>% 
        filter(level2_cluster_id %in% sampled_level2_ids)
      
      # Create baseline sample with composite ID
      baseline_sample <- sampled_level2 %>%
        mutate(time = 0,
               composite_id = paste0(level2_cluster_id, "_", 0))
      
      # Create endline sample (same level 2 clusters, different composite_id)
      endline_sample <- sampled_level2 %>% 
        mutate(time = 1,
               composite_id = paste0(level2_cluster_id, "_", 1))
      
      level2_samples[[i]] <- bind_rows(baseline_sample, endline_sample)
    }
    
    return(bind_rows(level2_samples))
    
  } else if (sampling_scenario == "SDD") {
    # Same level 1 clusters, different level 2 clusters, different individuals
    # Only valid for DISC design
    
    level2_samples <- list()
    
    # Get unique level 1 clusters
    unique_clusters <- all_sampled_level1_clusters %>% 
      filter(time == 0) %>%  # Since this is DISC, just take baseline rows to get unique clusters
      select(level1_cluster_id, intervention, level1_cluster_effect)
    
    for (i in seq_len(nrow(unique_clusters))) {
      level1_id <- unique_clusters$level1_cluster_id[i]
      
      # Get all level 2 clusters for this level 1 cluster
      available_level2 <- level2_clusters %>% filter(level1_cluster_id == level1_id)
      
      # Baseline: sample level 2 clusters
      sampled_level2_ids_baseline <- available_level2 %>% 
        slice_sample(n = n_level2_per_level1) %>% 
        pull(level2_cluster_id)
      
      baseline_sample <- available_level2 %>% 
        filter(level2_cluster_id %in% sampled_level2_ids_baseline) %>%
        mutate(time = 0,
               composite_id = paste0(level2_cluster_id, "_", 0))
      
      # Endline: sample different level 2 clusters
      sampled_level2_ids_endline <- available_level2 %>% 
        slice_sample(n = n_level2_per_level1) %>% 
        pull(level2_cluster_id)
      
      endline_sample <- available_level2 %>% 
        filter(level2_cluster_id %in% sampled_level2_ids_endline) %>%
        mutate(time = 1,
               composite_id = paste0(level2_cluster_id, "_", 1))
      
      level2_samples[[i]] <- bind_rows(baseline_sample, endline_sample)
    }
    
    return(bind_rows(level2_samples))
    
  } else {
    stop("sampling_scenario must be 'Two_Level', 'DDD', 'SSD', or 'SDD'")
  }
}

## e. sample individuals from each cluster ####
sample_individuals <- function(all_sampled_clusters, n, sampling_scenario, level2_samples = NULL) {
  
  if (sampling_scenario == "Two_Level") {
    # Two-level design: sample individuals directly from level 1 clusters
    
    # initialize empty list for individuals
    individual_samples <- list()
    
    # loop through all sampled cluster_time combos
    for (i in seq_len(nrow(all_sampled_clusters))) {
      cluster_time <- all_sampled_clusters$composite_id[i]
      # generate individual samples
      samples <- rnorm(n = n, 0, 1)
      # create df with composite ID, individual ID, and individual samples
      df <- tibble(composite_id = cluster_time, individual_id = 1:n, individual_residual = samples)
      # append to list 
      individual_samples[[i]] <- df
    }
    
    # combine into one df
    all_sampled_individuals <- tibble(bind_rows(individual_samples))
    
    return(all_sampled_individuals)
    
  } else {
    # Three-level design: sample individuals from level 2 clusters
    
    individual_samples <- list()
    
    # loop through all sampled level 2 cluster_time combos
    for (i in seq_len(nrow(level2_samples))) {
      cluster_time <- level2_samples$composite_id[i]
      # generate individual samples
      samples <- rnorm(n = n, 0, 1)
      # create df with composite ID, individual ID, and individual samples
      df <- tibble(composite_id = cluster_time, individual_id = 1:n, individual_residual = samples)
      # append to list 
      individual_samples[[i]] <- df
    }
    
    # combine into one df
    all_sampled_individuals <- tibble(bind_rows(individual_samples))
    
    return(all_sampled_individuals)
  }
}

## f. create final df ####
# set values for mean outcome at baseline and follow-up, treatment effect the same;
# starting with simplest design of mean outcome at baseline and follow-up 
# among untreated group = 0
create_final_data <- function(all_sampled_individuals, all_sampled_clusters, te = 1, binary_effect = 0, uniform_effect = 0, sampling_scenario = "Two_Level", level2_samples = NULL) {
  
  # generate covariates
  binary_covariate <- rbinom(nrow(all_sampled_individuals), size = 1, prob = 0.5)
  uniform_covariate <- runif(nrow(all_sampled_individuals), min = 0, max = 1 )
  
  if (sampling_scenario == "Two_Level") {
    # Two-level design: use level 1 cluster effects
    final_sample <- all_sampled_individuals %>% 
      # join individual and cluster level data
      left_join(all_sampled_clusters, by = "composite_id") %>% 
      # calculate outcomes
      mutate(treatment_effect = te) %>% 
      mutate(outcome = 0 + level1_cluster_effect + treatment_effect*intervention*time + 
               time + binary_effect*binary_covariate + 
               uniform_effect*uniform_covariate + individual_residual) %>% 
      # add covariates to final sample df
      mutate(binary_covariate = binary_covariate,
             uniform_covariate = uniform_covariate)
    
  } else {
    # Three-level design: use level 1 + level 2 cluster effects
    final_sample <- all_sampled_individuals %>% 
      # join individual and level 2 cluster data
      left_join(level2_samples, by = "composite_id") %>% 
      # calculate outcomes (level1_cluster_effect + level2_cluster_effect)
      mutate(treatment_effect = te) %>% 
      mutate(outcome = 0 + level1_cluster_effect + level2_cluster_effect + treatment_effect*intervention*time + 
               time + binary_effect*binary_covariate + 
               uniform_effect*uniform_covariate + individual_residual) %>% 
      # add covariates to final sample df
      mutate(binary_covariate = binary_covariate,
             uniform_covariate = uniform_covariate)
  }
  
  return(final_sample)
  
}

## g. helper functions ####

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

# TWO separate simulations:
# 1. Two-level designs: Traditional RCS vs DISC (both at two levels)
# 2. Three-level designs: DDD (RCS) vs SSD (DISC) vs SDD (DISC)

# create new simulations #
sim <- new_sim()

# 3-level
sim %<>% set_levels(
  icc = seq(from = 0, to = 0.2, by = 0.05),
  n_clusters = seq(from = 10, to = 100, by = 10),
  sampling_scenario = c("DDD", "SSD", "SDD")
)

# 2-level
# sim %<>% set_levels(
#   icc = seq(from = 0, to = 0.2, by = 0.05),
#   design = c("Traditional RCS", "DISC"),
#   n_clusters = seq(from = 10, to = 100, by = 10),
#   sampling_scenario = c("Two_Level")  # Two-level designs only
# )

# 5. create simulation scripts ####
sim %<>% set_script(function() {
  sd_level1 <- get_sd(icc = L$icc)
  sd_level2 <- get_sd(icc = L$icc)
  
  # Determine design based on sampling scenario
  # For three-level simulation:
  if (L$sampling_scenario == "DDD") {
    design <- "Traditional RCS"
  } else if (L$sampling_scenario %in% c("SSD", "SDD")) {
    design <- "DISC"
  } else if (L$sampling_scenario == "Two_Level") {
    # For two-level simulation, design comes from L$design
    design <- L$design
  }
  
  # Create level 1 clusters (population)
  level1_clusters <- create_level1_clusters(1000, sd_level1, mean = 'uncorrelated')
  
  # Create level 2 clusters (population: 20 per level 1 cluster)
  # Only needed for three-level designs
  if (L$sampling_scenario != "Two_Level") {
    level2_clusters <- create_level2_clusters(level1_clusters, n_level2_per_level1 = 20, sd_level2)
  } else {
    level2_clusters <- NULL
  }
  
  # Sample level 1 clusters
  all_sampled_level1_clusters <- sample_level1_clusters(level1_clusters, L$n_clusters, design = design)
  
  # Sample level 2 clusters (sample 5 from the 20 available per level 1 cluster)
  level2_samples <- sample_level2_clusters(all_sampled_level1_clusters, level2_clusters, 
                                           n_level2_per_level1 = 5, L$sampling_scenario, design)
  
  # Sample individuals
  all_sampled_individuals <- sample_individuals(all_sampled_level1_clusters, 25, L$sampling_scenario, level2_samples)
  
  # Create final data
  final_data <- create_final_data(all_sampled_individuals, all_sampled_level1_clusters, sampling_scenario = L$sampling_scenario, level2_samples = level2_samples)
  final_data_large_uniform <- create_final_data(all_sampled_individuals, all_sampled_level1_clusters, uniform_effect = 5, binary_effect = 0.5, sampling_scenario = L$sampling_scenario, level2_samples = level2_samples)
  
  # Fit models
  linear_estimate <- fit_model_lm(final_data)
  drdid_estimate <- fit_model_drdid(final_data)
  linear_estimate_large_uniform <- fit_model_lm(final_data_large_uniform)
  drdid_estimate_large_uniform <- fit_model_drdid(final_data_large_uniform)
  
  return (list("linear_estimate"=linear_estimate, 
               "drdid_estimate" = drdid_estimate, 
               "linear_estimate_large_uniform" = linear_estimate_large_uniform,
               "drdid_estimate_large_uniform" = drdid_estimate_large_uniform,
               "sd_level1"=sd_level1,
               "sd_level2"=sd_level2))
})

sim %<>% set_config(
  num_sim = 1000,
  packages = c("tidyverse", "lme4", "stringr"),
  progress_bar = TRUE  # Show progress bar during simulation
)

# 6. run simulation, summarize, save #### 
# Only run if this script is executed directly (not sourced)
if (sys.nframe() == 0) {
  
  sim %<>% run()
  # uncomment to overwrite current saved .Rdata files
  # save(sim, file = "simulation_results_linear_drdid.RData")
  save(sim, file = "simulation_results_linear_drdid_3level.RData")
  
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
  
  cat("Simulation complete!\n")
  
} else {
  cat("disc_simulation.R functions loaded for testing.\n")
}
