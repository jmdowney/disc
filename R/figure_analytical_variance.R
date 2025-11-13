# 1. load simulation results and packages, create helper functions ####

# load("simulation_results.RData")

# get SD from ICC 
get_sd <- function(icc, sigma) {
  
  sd <- sigma*sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. create analytical dataset ####

# varying m, assuming 100 clusters
m <- c(10, 25)
n <- 1000
icc <- seq(from = 0, to = 0.2, by = 0.05)
sigma <- sqrt(1000/8)

analytical_disc_varying_m <- expand.grid(m = m, icc = icc) %>% 
  mutate(var = (8*sigma^2)/n,
         method = 'Analytical',
         design = 'DISC')

analytical_rcs_varying_m <- expand.grid(m = m, icc = icc) %>% 
  mutate(var = 8*(m*(get_sd(icc,sigma)^2) + sigma^2)/n,
         method = 'Analytical',
         design = 'RCS')

# 3. create figure to compare designs for analytical calculations across different ICC values ####
custom_labels <- c("10" = "10 individuals\nper cluster", "25" = "25 individuals\nper cluster")
(analytical_variance_figure <- analytical_disc_varying_m %>% 
    rbind(analytical_rcs_varying_m) %>% 
    ggplot(aes(icc, var, linetype = design)) +
    geom_line() +
    facet_wrap(~ m, labeller = as_labeller(custom_labels)) + 
    xlab('ICC') +
    ylab('Total variance') + 
    scale_y_continuous(breaks=c(1:10)) +
    labs(linetype = 'Design') 
)

# 4. save ####
ggsave(
  filename = paste0("Figures/", cfg$d, " analytical_variance_figure.pdf"),
  plot = analytical_variance_figure,
  device = "pdf",
  width = 9,
  height = 5
)
