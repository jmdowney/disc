# 1. load simulation results and packages, create helper functions ####

library(SimEngine)

load("simulation_results.RData")
load("simulation_drdid_results.RData")

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. create analytical dataset ####

# assuming m = 25
n_lm <- sim_lm$levels$n_clusters*25
icc_lm <- sim_lm$levels$icc
n_drdid <- sim_drdid$levels$n_clusters*25
icc_drdid <- sim_drdid$levels$icc

analytical_disc_lm <- expand.grid(n = n_lm, icc = icc_lm) %>% 
  mutate(var = 8/n,
         method = 'Analytical',
         design = 'DISC',
         model = 'Linear')

analytical_disc_drdid <- expand.grid(n = n_drdid, icc = icc_drdid) %>% 
  mutate(var = 8/n,
         method = 'Analytical',
         design = 'DISC',
         model = 'DRDID')

analytical_rcs_lm <- expand.grid(n = n_lm, icc = icc_lm) %>% 
  mutate(var = 8*( 25*((get_sd(icc))^2) + 1)/n,
         method = 'Analytical',
         design = 'Traditional RCS',
         model = 'Linear')

analytical_rcs_drdid <- expand.grid(n = n_drdid, icc = icc_drdid) %>% 
  mutate(var = 8*( 25*((get_sd(icc))^2) + 1)/n,
         method = 'Analytical',
         design = 'Traditional RCS',
         model = 'DRDID')

# 3. wrangle simulation datasets ####

simulation_lm <- sim_lm %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "estimate")
  ) %>% 
  mutate(n = n_clusters*25,
         var = sd_estimate^2,
         method = 'Simulation',
         model = 'Linear') %>% 
  select(n, icc, var, method, design, model)

simulation_drdid <- sim_drdid %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "estimate")
  ) %>% 
  mutate(n = n_clusters*25,
         var = sd_estimate^2,
         method = 'Simulation',
         model = 'DRDID') %>% 
  select(n, icc, var, method, design, model)

# 4. create figure to compare simulation results and analytical calculations for ICC = 0.2 ####
(analytical_vs_simulation_figure <- simulation_lm %>% 
    rbind(simulation_drdid) %>% 
    rbind(analytical_disc_lm) %>%
    rbind(analytical_disc_drdid) %>% 
    rbind(analytical_rcs_lm) %>% 
    rbind(analytical_rcs_drdid) %>%
    filter(icc == 0.2) %>% 
    mutate(model_factor = factor(model, levels = c('Linear', 'DRDID'))) %>% 
    ggplot(aes(n, var, linetype = method)) + 
    geom_line(position = position_jitter(w=0, h=0.003)) +
    facet_grid(model_factor~design) +
    xlab('Total individuals (n)') +
    ylab('Total variance') + 
    labs(title = str_wrap('Estimated total variance under DISC and traditional RCS designs, comparing empirical and theoretical variance, for a linear model and a DRDID model with covariates'),
         linetype = 'Method') 
)

# 4. save figure ####
ggsave("analytical_vs_simulation_figure.pdf", 
       plot = analytical_vs_simulation_figure, 
       width = 9, 
       height = 5)
