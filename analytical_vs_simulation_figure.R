# 1. load simulation results and packages, create helper functions ####

library(SimEngine)

load("simulation_results.RData")

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. create analytical dataset ####

# assuming m = 25
n <- sim$levels$n_clusters*25
icc <- sim$levels$icc

analytical_disc <- expand.grid(n = n, icc = icc) %>% 
  mutate(var = 8/n,
         method = 'Analytical',
         design = 'DISC')

analytical_rcs <- expand.grid(n = n, icc = icc) %>% 
  mutate(var = 8*( 25*((get_sd(icc))^2) + 1)/n,
         method = 'Analytical',
         design = 'Traditional RCS')

# 3. create figure to compare simulation results and analytical calculations for ICC = 0.2 ####
(results <- sim %>% 
    SimEngine::summarize(
      list(stat = "sd", x = "estimate")
    ) %>% 
    mutate(n = n_clusters*25,
           var = sd_estimate^2,
           method = 'Simulation') %>% 
    select(n, icc, var, method, design) %>% 
    rbind(analytical_disc) %>%
    rbind(analytical_rcs) %>% 
    filter(icc == 0.2) %>% 
    ggplot(aes(n, var, linetype = method)) + 
    geom_line(position = position_jitter(w=0, h=0.003)) +
    facet_wrap(vars(design)) +
    xlab('Total individuals (n)') +
    ylab('Total variance') + 
    labs(title = str_wrap('Estimated total variance under DISC and traditional RCS designs, comparing empirical and theoretical variance', 60),
         linetype = 'Method') 
)

# 4. save figure ####
ggsave("analytical_vs_simulation_figure.pdf")
