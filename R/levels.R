# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  level_sets[["main_2_levels"]] <- list(
    ind_per_clust = c(10, 25, 50, 100),
    icc = c(0.01, 0.05, 0.1, 0.2),
    design = c("RCS", "DISC"),
    ipc_sd = 0,
    n_clusters = seq(from = 10, to = 100, by = 10),
    sampling_scenario = c("Two_Level")
  )
  
  level_sets[["main_vary_cluster_size"]] <- list(
    ind_per_clust = 25,
    icc = c(0.01, 0.1),
    design = c("RCS", "DISC"),
    ipc_sd = c(0, 4, 8),
    n_clusters = seq(from = 10, to = 100, by = 10),
    sampling_scenario = c("Two_Level")
  )
  
  level_sets[["main_3_levels"]] <- list(
    ind_per_clust = 25,
    icc = seq(from = 0, to = 0.2, by = 0.05),
    ipc_sd = 0,
    n_clusters = seq(from = 10, to = 100, by = 10),
    sampling_scenario = c("DDD", "SSD", "SDD")
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
  # if (cfg$sim_level_set=="asdf") { cfg$keep = c(1:3,7:9,16:18,22:24) }
  
}
