################################.
##### Main DISC simulation #####
################################.

if (cfg$sim_which=="main") {
  
  #' Run a single simulation
  #'
  #' @return A list with ...
  one_simulation <- function() {
    
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
    all_sampled_individuals <- sample_individuals(all_sampled_level1_clusters, L$ind_per_clust, L$sampling_scenario, level2_samples)
    
    # Create final data
    final_data <- create_final_data(all_sampled_individuals, all_sampled_level1_clusters, sampling_scenario = L$sampling_scenario, level2_samples = level2_samples)
    final_data_large_uniform <- create_final_data(all_sampled_individuals, all_sampled_level1_clusters, uniform_effect = 5, binary_effect = 0.5, sampling_scenario = L$sampling_scenario, level2_samples = level2_samples)
    
    # Fit models
    linear_estimate <- fit_model_lm(final_data)
    drdid_estimate <- fit_model_drdid(final_data)
    linear_estimate_large_uniform <- fit_model_lm(final_data_large_uniform)
    drdid_estimate_large_uniform <- fit_model_drdid(final_data_large_uniform)
    
    return (list(
      "linear_estimate" = linear_estimate$est,
      "linear_se" = linear_estimate$se,
      "drdid_estimate" = drdid_estimate$est,
      "linear_estimate_large_uniform" = linear_estimate_large_uniform$est,
      "drdid_estimate_large_uniform" = drdid_estimate_large_uniform$est,
      "sd_level1" = sd_level1,
      "sd_level2" = sd_level2
    ))
    
  }
  
}
