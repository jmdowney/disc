
sim <- readRDS("SimEngine.out/main_vary_cluster_size_20251113.rds")

get_sd <- function(icc) {
  sd <- sqrt(icc/(1-icc))
  return(sd)
}

simulation_lm <- sim %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "linear_estimate")
  ) %>% 
  mutate(n = n_clusters*25,
         var = sd_linear_estimate^2,
         model = 'Linear') %>% 
  select(n_clusters, icc, ind_per_clust, n, var, ipc_sd, design, model)


# Supplemental figures for appendix
(supp_fig_2 <- simulation_lm %>%
    mutate(
      icc = factor(paste("ICC:",icc),
                   levels=paste("ICC:",sim$levels$icc)),
      ipc_sd = factor(paste("Cluster size SD:",ipc_sd),
                             levels=paste("Cluster size SD:",sim$levels$ipc_sd))
    ) %>%
    ggplot(aes(x=n_clusters, y=var, linetype = design, color=design)) + 
    geom_line(position = position_jitter(w=0, h=0.001)) +
    # geom_point() +
    facet_grid(
      rows = dplyr::vars(icc),
      cols = dplyr::vars(ipc_sd),
      scales = "free"
    ) +
    scale_color_manual(values=c("#E69F00", "#009E73")) +
    xlab('Number of clusters') +
    ylab('Total variance') + 
    labs(linetype = 'Design', color="Design") + # shape="Design", 
    theme(legend.position="bottom")
)
ggsave(
  filename = paste0("Figures/", cfg$d, " supp_fig_2.pdf"),
  plot = supp_fig_2,
  device = "pdf",
  width = 8,
  height = 5
)
