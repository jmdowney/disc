############################.
##### VIZ: Scatterplot #####
############################.

# Figures produced: fig_1, fig_2

sim <- readRDS("SimEngine.out/estimation_1_20231112.rds")

plot_data <- subset(sim$results, select=c("beta", "est", "se"))
plot <- ggplot(plot_data, aes(x=beta, y=est)) + geom_point(alpha=0.1)
ggsave(filename="fig_1.pdf", plot=plot, device="pdf", width=6, height=4)
