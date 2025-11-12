
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
