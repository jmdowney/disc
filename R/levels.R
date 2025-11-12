# # Set global constants
# C <- list(
#   alpha_1 = 0.5,
#   alpha_2 = 0.7,
#   t_0 = 200
# )

# Set simulation levels
if (cfg$run_sims && Sys.getenv("sim_run") %in% c("first", "")) {
  
  level_sets <- list()
  
  # Estimation: no edge mass
  # Figures: fig_1, fig_2
  level_sets[["estimation_1"]] <- list(
    n = c(10, 100, 1000),
    beta = c(2,3,4)
  )
  
  # Estimation: edge mass
  # Figures: fig_3, fig_4
  level_sets[["estimation_2"]] <- list(
    n = c(10, 100, 1000),
    beta = c(-2,-3,-4)
  )
  
  level_set <- level_sets[[cfg$sim_level_set]]
  
  # if (cfg$sim_level_set=="asdf") { cfg$keep = c(1:3,7:9,16:18,22:24) }
  
}
