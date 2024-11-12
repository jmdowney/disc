
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

set.seed(15)
cfg <- list(save=F)
df_pop <- data.frame(
  "x" = double(),
  "y" = double(),
  "time" = character(),
  "Sampled" = logical()
)

# Set cluster boundaries
df_rect <- data.frame(
  xmin = c(0.00, 0.35, 0.75, 0.00, 0.25, 0.60),
  xmax = c(0.30, 0.70, 1.00, 0.20, 0.55, 1.00),
  ymin = c(0.00, 0.00, 0.00, 0.50, 0.50, 0.50),
  ymax = c(0.45, 0.45, 0.45, 1.00, 1.00, 1.00),
  time = "Time 1"
)

# Sample individuals within boundaries
row <- 1
pad <- 0.03
for (i in c(1:nrow(df_rect))) {
  brd <- df_rect[i,]
  for (j in c(1:6)) {
    x <- runif(n=1, min=brd$xmin+pad, max=brd$xmax-pad)
    y <- runif(n=1, min=brd$ymin+pad, max=brd$ymax-pad)
    df_pop[row,] <- list(x, y, "Time 1", F)
    row <- row + 1
  }
}

df_pop2 <- df_pop
df_rect2 <- df_rect
df_pop2$time <- "Time 2"
df_rect2$time <- "Time 2"
df_pop <- rbind(df_pop, df_pop2)
df_rect <- rbind(df_rect, df_rect2)

for (design in c("RCS", "Cohort", "DISC")) {
  
  # Set sampled clusters
  if (design=="RCS") {
    df_rect$Sampled <- c(
      c(F,T,F,T,F,F),
      c(T,F,T,F,F,F)
    )
  } else {
    df_rect$Sampled <- rep(c(T,F,F,F,F,T), 2)
  }
  
  # Set sampled individuals
  # cl<-1; c(1,2,3)+6*(cl-1);
  df_pop$Sampled <- F
  if (design=="RCS") {
    inds_t1 <- c(7,8,9,19,20,21)
    inds_t2 <- c(37,38,39,49,50,51)
  } else if (design=="Cohort") {
    inds_t1 <- c(1,2,3,31,32,33)
    inds_t2 <- inds_t1 + 36
  } else if (design=="DISC") {
    inds_t1 <- c(1,2,3,31,32,33)
    inds_t2 <- inds_t1 + 38
  }
  df_pop[c(inds_t1,inds_t2),"Sampled"] <- T
  
  # Create plot
  plot <- ggplot(df_pop, aes(x=x, y=y, color=Sampled)) +
    geom_rect(
      aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Sampled,
          linetype=Sampled),
      data = df_rect,
      linewidth = 0.5,
      alpha = 0.3
    ) +
    geom_point(aes(shape=Sampled), size=2) +
    scale_linetype_manual(values=c("dotted", "solid")) +
    scale_shape_manual(values=c(1, 19)) +
    scale_fill_manual(values=c("salmon", "forestgreen")) +
    scale_color_manual(values=c("salmon", "forestgreen")) +
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      # plot.background = element_rect(color = "black"),
      plot.margin = unit(c(5,10,5,10), "pt"),
      legend.position = "bottom"
    ) +
    labs(x=NULL, y=NULL, title=design) +
    facet_grid(rows=dplyr::vars(time))
  
  assign(paste0("plot_", design), plot)
  
}

p_combined <- ggpubr::ggarrange(
  plot_RCS,
  plot_Cohort,
  plot_DISC,
  ncol = 3,
  common.legend = T,
  legend = "bottom"
)
print(p_combined)

if (cfg$save) {
  ggsave(
    filename = "design_plot.pdf",
    plot = p_combined,
    device = "pdf",
    width = 8,
    height = 5
  )
}
