# 1. load simulation results and packages, create helper functions ####

library(SimEngine)

load("simulation_results.RData")

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. create analytical dataset ####

# varying m, assuming 100 clusters
m <- c(10, 100)
icc <- sim$levels$icc

analytical_disc_varying_m <- expand.grid(m = m, icc = icc) %>% 
  mutate(var = 8/(m*100),
         method = 'Analytical',
         design = 'DISC')

analytical_rcs_varying_m <- expand.grid(m = m, icc = icc) %>% 
  mutate(var = 8*(m*(get_sd(icc)^2) + 1)/(m*100),
         method = 'Analytical',
         design = 'Traditional RCS')

# 3. create figure to compare designs for analytical calculations across different ICC values ####
custom_labels <- c("10" = "10 individuals\nper cluster", "100" = "100 individuals\nper cluster")
(analytical_varying_m <- analytical_disc_varying_m %>% 
    rbind(analytical_rcs_varying_m) %>% 
    ggplot(aes(icc, var, linetype = design)) +
    geom_line() +
    facet_wrap(~ m, labeller = as_labeller(custom_labels)) + 
    xlab('ICC') +
    ylab('Total variance') + 
    labs(title = str_wrap('Total analytical variance, comparing DISC and traditional RCS designs', 60),
         linetype = 'Design') 
)

# 4. save ####
ggsave("analytical_variance_figure.pdf")
