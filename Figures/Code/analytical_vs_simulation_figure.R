# 1. load simulation results and packages, create helper functions ####

library(tidyverse)
library(SimEngine)

load("simulation_results_linear_drdid.RData")

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. create analytical dataset ####

# assuming m = 25
n_lm <- sim$levels$n_clusters*25
icc_lm <- sim$levels$icc

analytical_disc_lm <- expand.grid(n = n_lm, icc = icc_lm) %>% 
  mutate(var = 8/n,
         method = 'Analytical',
         design = 'DISC',
         model = 'Linear')

analytical_rcs_lm <- expand.grid(n = n_lm, icc = icc_lm) %>% 
  mutate(var = 8*( 25*((get_sd(icc))^2) + 1)/n,
         method = 'Analytical',
         design = 'Traditional RCS',
         model = 'Linear')

# 3. wrangle simulation datasets ####

simulation_lm <- sim %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "linear_estimate")
  ) %>% 
  mutate(n = n_clusters*25,
         var = sd_linear_estimate^2,
         method = 'Simulation',
         model = 'Linear') %>% 
  select(n, icc, var, method, design, model)

# 4. create figure to compare simulation results and analytical calculations for ICC = 0.1 ####
(analytical_vs_simulation_figure_linear <- simulation_lm %>% 
    rbind(analytical_disc_lm) %>%
    rbind(analytical_rcs_lm) %>% 
    filter(icc == 0.1) %>% 
    ggplot(aes(n, var, linetype = method)) + 
    geom_line(position = position_jitter(w=0, h=0.002)) +
    facet_grid(~design) +
    xlab('Total individuals (n)') +
    ylab('Total variance') + 
    labs(linetype = 'Method') 
)

# 4. save figure ####
ggsave("analytical_vs_simulation_figure_linear.pdf", 
       plot = analytical_vs_simulation_figure_linear, 
       width = 9, 
       height = 5)
