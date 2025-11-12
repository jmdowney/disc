# NOTES:
#     TWO separate simulations:
#       1. Two-level designs: Traditional RCS vs DISC (both at two levels)
#       2. Three-level designs: DDD (RCS) vs SSD (DISC) vs SDD (DISC)

# Main config
cfg <- list(
  run_sims = F,
  run_process = T,
  sim_which = "main",
  sim_level_set = "main_3_levels", # "main_2_levels" "main_3_levels"
  sim_run_or_update = "run",
  sim_num = 1000,
  sim_parallel = F,
  sim_n_cores = 500,
  sim_stop_at_error = F
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
if (cfg$run_process) { source("R/process.R", local=T) }
