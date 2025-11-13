
for (is_three_level in c(FALSE,TRUE)) {
  
  if (is_three_level) {
    load("Simulation Results/simulation_results_linear_drdid_3level.RData")
  } else {
    load("Simulation Results/simulation_results_linear_drdid.RData")
  }

  # get SD from ICC 
  get_sd <- function(icc) {
    sd <- sqrt(icc/(1-icc))
    return(sd)
  }
  
  # 2. wrangle simulation datasets ####
  
  if (is_three_level) {
    # Three-level design: use sampling_scenario instead of design
    simulation_lm <- sim %>% 
      SimEngine::summarize(
        list(stat = "sd", x = "linear_estimate_large_uniform")
      ) %>% 
      mutate(n = n_clusters*25*5,  # 25 individuals * 5 level 2 clusters
             var = sd_linear_estimate_large_uniform^2,
             method = 'Simulation',
             model = 'Linear',
             scenario = sampling_scenario) %>% 
      select(n, icc, var, method, scenario, model)
    
    simulation_drdid <- sim %>% 
      SimEngine::summarize(
        list(stat = "sd", x = "drdid_estimate_large_uniform")
      ) %>% 
      mutate(n = n_clusters*25*5,  # 25 individuals * 5 level 2 clusters
             var = sd_drdid_estimate_large_uniform^2,
             method = 'Simulation',
             model = 'DRDID',
             scenario = sampling_scenario) %>% 
      select(n, icc, var, method, scenario, model)
    
  } else {
    # Two-level design: use design
    simulation_lm <- sim %>% 
      SimEngine::summarize(
        list(stat = "sd", x = "linear_estimate_large_uniform")
      ) %>% 
      mutate(n = n_clusters*25,
             var = sd_linear_estimate_large_uniform^2,
             method = 'Simulation',
             model = 'Linear',
             scenario = design) %>% 
      select(n, icc, var, method, scenario, model)
    
    simulation_drdid <- sim %>% 
      SimEngine::summarize(
        list(stat = "sd", x = "drdid_estimate_large_uniform")
      ) %>% 
      mutate(n = n_clusters*25,
             var = sd_drdid_estimate_large_uniform^2,
             method = 'Simulation',
             model = 'DRDID',
             scenario = design) %>% 
      select(n, icc, var, method, scenario, model)
  }
  
  # 3. create figure to compare simulation results for each method for ICC = 0.1 ####
  
  # Combine data and create proper factor ordering
  combined_data <- simulation_lm %>% 
    rbind(simulation_drdid) %>% 
    filter(icc == 0.1) %>% 
    mutate(model_factor = factor(model, levels = c('Linear', 'DRDID')))
  
  if ("Traditional RCS" %in% unique(combined_data$scenario)) {
    combined_data %<>% mutate(
      scenario = ifelse(scenario=="Traditional RCS", "RCS", scenario)
    )
  }
  
  if (is_three_level) {
    # Three-level design: order as DDD, SDD, SSD
    combined_data <- combined_data %>%
      mutate(scenario_factor = factor(scenario, levels = c('DDD', 'SDD', 'SSD')))
    
    drdid_vs_linear_figure <- combined_data %>%
      ggplot(aes(n, var, linetype = model)) + 
      geom_line() +
      facet_grid(~scenario_factor) +
      xlab('Total individuals (n)') +
      ylab('Total variance') + 
      labs(linetype = 'Model')
    
  } else {
    
    # Two-level design: keep original format
    combined_data <- combined_data %>%
      mutate(scenario_factor = factor(scenario, levels = c('RCS', 'DISC')))
    
    drdid_vs_linear_figure <- combined_data %>%
      ggplot(aes(n, var, linetype = model)) + 
      geom_line() +
      facet_grid(~scenario_factor) +
      xlab('Total individuals (n)') +
      ylab('Total variance') + 
      labs(linetype = 'Model')
  }
  
  # Display the figure
  print(drdid_vs_linear_figure)
  
  # 4. save figure ####
  if (is_three_level) {
    ggsave(
      filename = paste0("Figures/", cfg$d, " drdid_vs_linear_figure_3level.pdf"),
      plot = drdid_vs_linear_figure,
      device = "pdf",
      width = 9,
      height = 4
    )
  } else {
    ggsave(
      filename = paste0("Figures/", cfg$d, " drdid_vs_linear_figure_2level.pdf"),
      plot = drdid_vs_linear_figure,
      device = "pdf",
      width = 9,
      height = 5
    )
  }
  
}

