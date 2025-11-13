# 1. load simulation results and packages, create helper functions ####

sim <- readRDS("SimEngine.out/main_2_levels_20251112.rds")

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# 2. create analytical dataset ####

n_clust_lm <- sim$levels$n_clusters
icc_lm <- sim$levels$icc
ipc_lm <- sim$levels$ind_per_clust

analytical_disc_lm <- expand.grid(n_clusters=n_clust_lm, icc=icc_lm,
                                  ind_per_clust=ipc_lm) %>% 
  mutate(n = n_clust_lm*ind_per_clust,
         var = 8/n,
         method = 'Analytical',
         design = 'DISC',
         model = 'Linear')

analytical_rcs_lm <- expand.grid(n_clusters=n_clust_lm, icc=icc_lm,
                                 ind_per_clust=ipc_lm) %>% 
  mutate(n = n_clust_lm*ind_per_clust,
         var = 8*( ind_per_clust*((get_sd(icc))^2) + 1)/n,
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
  select(n_clusters, icc, ind_per_clust, n, var, method, design, model)
  
# 4. create figure to compare simulation results and analytical calculations for ICC = 0.1 ####
df_full <- simulation_lm %>% 
  rbind(analytical_disc_lm) %>%
  rbind(analytical_rcs_lm)
df_filtered <- df_full %>% filter(icc==0.1 & ind_per_clust==25)


(analytical_vs_simulation_figure_linear <- df_filtered %>%
    ggplot(aes(n, var, linetype = method)) + 
    geom_line(position = position_jitter(w=0, h=0.001)) +
    facet_grid(~design) +
    xlab('Total individuals (n)') +
    ylab('Total variance') + 
    labs(linetype = 'Method') 
)

# 4. save figure ####
ggsave(
  filename = paste0("Figures/", cfg$d, " analytical_vs_simulation_figure_linear.pdf"),
  plot = p_combined,
  device = "pdf",
  width = 9,
  height = 5
)

# Supplemental figures for appendix
(supp_fig_1 <- df_full %>%
    mutate(
      icc = factor(paste("ICC:",icc),
                   levels=paste("ICC:",icc_lm)),
      ind_per_clust = factor(paste("m:",ind_per_clust),
                             levels=paste("m:",ipc_lm))
    ) %>%
    ggplot(aes(x=n_clusters, y=var, linetype = method, color=design, shape=design)) + 
    geom_line(position = position_jitter(w=0, h=0.001)) +
    geom_point() +
    facet_grid(
      rows = dplyr::vars(icc),
      cols = dplyr::vars(ind_per_clust),
      scales = "free"
    ) +
    xlab('Number of clusters') +
    ylab('Total variance') + 
    labs(linetype = 'Method', shape="Design", color="Design") +
    theme(legend.position="bottom")
)
ggsave(
  filename = paste0("Figures/", cfg$d, " supp_fig_1.pdf"),
  plot = supp_fig_1,
  device = "pdf",
  width = 9,
  height = 7
)

# Table of simulation results
summ2 <- sim %>% SimEngine::summarize(
  list(stat="bias", estimate="linear_estimate", truth=1, name="Bias"),
  list(stat="var", x="linear_estimate", name="Variance"),
  list(stat="mse", estimate="linear_estimate", truth=1, name="MSE"),
  list(stat="coverage", estimate="linear_estimate", se="linear_se", truth=1, name="Coverage")
) %>%
  dplyr::filter(
    ind_per_clust %in% c(25,100) &
    icc %in% c(0.01, 0.1) &
    n_clusters %in% c(20,50)
  ) %>% 
  dplyr::arrange(design, icc, ind_per_clust, n_clusters) %>%
  dplyr::mutate(
    Bias = round(Bias, 3),
    Variance = round(Variance, 3),
    MSE = round(MSE, 3),
    Coverage = round(Coverage, 3)
  )

print(summ2)
