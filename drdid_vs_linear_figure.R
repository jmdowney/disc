# 1. load simulation results and packages, create helper functions ####

library(SimEngine)

load("simulation_results_linear_drdid.RData")

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. wrangle simulation datasets ####

simulation_lm <- sim %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "linear_estimate_large_uniform")
  ) %>% 
  mutate(n = n_clusters*25,
         var = sd_linear_estimate_large_uniform^2,
         method = 'Simulation',
         model = 'Linear') %>% 
  select(n, icc, var, method, design, model)

simulation_drdid <- sim %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "drdid_estimate_large_uniform")
  ) %>% 
  mutate(n = n_clusters*25,
         var = sd_drdid_estimate_large_uniform^2,
         method = 'Simulation',
         model = 'DRDID') %>% 
  select(n, icc, var, method, design, model)

# 4. create figure to compare simulation results for each method for ICC = 0.2 ####
(drdid_vs_linear_figure <- simulation_lm %>% 
   rbind(simulation_drdid) %>% 
   filter(icc == 0.2) %>% 
   mutate(model_factor = factor(model, levels = c('Linear', 'DRDID'))) %>% 
   ggplot(aes(n, var, linetype = model)) + 
   geom_line() +
   facet_grid(~design) +
   xlab('Total individuals (n)') +
   ylab('Total variance') + 
   labs(linetype = 'Model') 
)

# 4. save figure ####
ggsave("drdid_vs_linear_figure.pdf", 
       plot = drdid_vs_linear_figure, 
       width = 9, 
       height = 5)
