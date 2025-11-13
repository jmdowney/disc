# NOTES:
#     TWO separate simulations:
#       1. Two-level designs: RCS vs DISC (both at two levels)
#       2. Three-level designs: DDD (RCS) vs SSD (DISC) vs SDD (DISC)

# Main config
cfg <- list(
  run_sims = T,
  run_process = F,
  sim_which = "main",
  sim_level_set = "main_vary_cluster_size", # "main_2_levels" "main_3_levels" "main_vary_cluster_size"
  sim_run_or_update = "run",
  sim_num = 1000,
  sim_parallel = F,
  sim_n_cores = 500,
  sim_stop_at_error = F,
  d = format(Sys.time(), "%Y-%m-%d")
)

# Secondary config
source("R/config.R", local=T)

# Load SimEngine + functions
{
  library(SimEngine)
  source("R/misc_functions.R", local=T)
  source("R/one_simulation.R", local=T)
}

# Set level sets
source("R/levels.R", local=T)

# Run simulation
if (cfg$run_sims) { source("R/run.R", local=T) }

# Tables and figures
if (cfg$run_process) {
  
  source("R/figure_disc.R", local=T) # Figure 1
  source("R/figure_analytical_variance.R", local=T) # Figure 2
  source("R/figure_analytical_vs_simulation.R", local=T) # Figure 3, Supp fig 1, Supp table 1
  source("R/figure_vary_cluster_size.R", local=T) # Supp fig 2
  
}
