
library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

set.seed(5)
cfg <- list(save=T)
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
    
    x_diff <- (brd$xmax-brd$xmin)/2
    y_diff <- (brd$ymax-brd$ymin)/3
    
    if (j %in% c(1,3,5)) {
      min_x <- brd$xmin
      max_x <- brd$xmin+x_diff
    } else {
      min_x <- brd$xmin+x_diff
      max_x <- brd$xmin+(x_diff*2)
    }
    if (j %in% c(1,2)) {
      min_y <- brd$ymin
      max_y <- brd$ymin+y_diff
    } else if (j %in% c(3,4)) {
      min_y <- brd$ymin+y_diff
      max_y <- brd$ymin+(y_diff*2)
    } else if (j %in% c(5,6)) {
      min_y <- brd$ymin+(y_diff*2)
      max_y <- brd$ymin+(y_diff*3)
    }
    min_x <- min_x+pad
    min_y <- min_y+pad
    max_x <- max_x-pad
    max_y <- max_y-pad
    
    x <- runif(n=1, min=min_x, max=max_x)
    y <- runif(n=1, min=min_y, max=max_y)
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
  df_pop$Sampled <- F
  if (design=="RCS") {
    inds_t1 <- c(7,10,12,20,21,23)
    inds_t2 <- c(37,38,42,50,51,54)
    title <- "(a) RCS sample"
  } else if (design=="Cohort") {
    inds_t1 <- c(1,4,5,31,33,36)
    inds_t2 <- c(37,40,41,67,69,72)
    title <- "(b) Cohort sample"
  } else if (design=="DISC") {
    inds_t1 <- c(1,4,5,31,33,36)
    inds_t2 <- c(38,40,42,68,69,71)
    title <- "(c) DISC sample"
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
    geom_point(aes(shape=Sampled), size=3) +
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
      plot.margin = unit(c(5,10,5,10), "pt"),
      legend.position = "bottom"
    ) +
    labs(x=NULL, y=NULL, title=title) +
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
    width = 9,
    height = 5
  )
}
