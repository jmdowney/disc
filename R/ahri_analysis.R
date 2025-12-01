# Setup ========================================================================
library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)

# Change working directory
setwd("/Users/jordandowney/Downloads")
loc_data <- read_dta("RD01-03 ACDIS BoundedStructures.dta") %>% 
  distinct(BSIntId, LocalArea, Isigodi)
hh_data <- read_csv("RD06-99 ACDIS HSE-H All/RD06-99 ACDIS HSE-H All.csv")
all_data <- hh_data %>% 
  left_join(loc_data, by = "BSIntId")
setwd("/Users/jordandowney/hybrid-cluster-sampling-simulation")

# EDA ==========================================================================

# Only 1 null observation using either Isigodi or LocalArea
loc_tab <- all_data %>% 
  group_by(LocalArea) %>% 
  #group_by(Isigodi) %>% 
  summarize(count = n())

# What years do we have?
years_tab <- all_data %>% 
  group_by(DSRound) %>% 
  mutate(year = max(year(VisitDate))) %>% 
  group_by(DSRound, year) %>% 
  summarize(count = n())

# How many households have more than 1 observation?
hh_tab <- all_data %>% 
  group_by(HHIntId) %>% 
  filter(n() >= 2)

# How many categories of binary variables?
(var_tab <- all_data %>%
  group_by(CTL) %>%
  summarize(count = n()))

# Create population ============================================================  

## Choose variables ####

# Baseline and endline year
baseline <- "2013"
endline <- "2017"

# Which variables have highest cluster-level variance at baseline?
  variance <- all_data %>% 
    filter(year(VisitDate) == baseline) %>% 
    select(LocalArea, BED:JWT)
  exclude_columns <- c("LocalArea")
  variables_to_analyze <- setdiff(names(variance), exclude_columns)
  
  # Initialize results df
  variance_results <- data.frame(
    variable = character(),
    cluster_variance = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Find cluster-level variances
  for (var in variables_to_analyze) {
    
    # Calculate proportion for each cluster (LocalArea)
    cluster_proportions <- variance %>%
      group_by(LocalArea) %>%
      summarise(
        cluster_prop = mean(.data[[var]], na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate variance across clusters
    cluster_var <- var(cluster_proportions$cluster_prop, na.rm = TRUE)
    
    # Store results
    variance_results <- rbind(
      variance_results,
      data.frame(
        variable = var,
        cluster_variance = cluster_var
      )
    )
  }

# Binary outcome variable
outcome <- "CTL"

## Select years and households ####
all_data <- hh_data %>%
  left_join(loc_data, by = "BSIntId") %>% 
  # take the two rounds with the highest HH intersection that are more than 3 years apart
  # 2013 & 2017, 17700 overlap
  filter(year(VisitDate) %in% c(baseline, endline)) %>% 
  group_by(HHIntId) %>% 
  filter(n() == 2) %>% 
  ungroup 

## Create population ####
population <- all_data %>% 
  mutate(year = year(VisitDate)) %>% 
  select(HHIntId, year, LocalArea, as.numeric(all_of(outcome))) %>% 
  # drop small clusters
  group_by(LocalArea) %>%
  filter(n() >= 100) %>% 
  ungroup

# Sample and collect results ===================================================

## Setup ####
set.seed(123)

# Choose sample sizes
n_clusters <- 10
n_hh <- 100
n_samples <- 500

# Initialize results df
results <- data.frame(
  iteration = 1:n_samples,
  baseline_prop = numeric(n_samples),
  endline_prop_rcs = numeric(n_samples),
  endline_prop_disc = numeric(n_samples),
  change_rcs = numeric(n_samples),
  change_disc = numeric(n_samples)
)

## Sample ####
for (i in 1:n_samples) {
  
  ### Baseline ####
  # Calculate cluster sizes (count of HHIntId per LocalArea)
  baseline_cluster_sizes <- population %>%
    filter(year == baseline) %>%
    group_by(LocalArea) %>% 
    summarise(cluster_size = n_distinct(HHIntId), .groups = 'drop')
  
  # Sample clusters using PPS
  baseline_clusters <- baseline_cluster_sizes %>%
    slice_sample(n = n_clusters, weight_by = cluster_size, replace = FALSE)
  
  # Sample households
  baseline_sample <- population %>%
    filter(year == baseline, LocalArea %in% baseline_clusters$LocalArea) %>%
    group_by(LocalArea) %>%
    slice_sample(n = n_hh, replace = FALSE) %>%
    ungroup()
  
  # Calculate baseline proportion of binary variable
  baseline_prop <- mean(baseline_sample[[outcome]], na.rm = TRUE)
  
  ## DISC endline ####
  # Sample the same clusters 
  endline_sample_disc <- population %>%
    filter(year == endline, LocalArea %in% baseline_clusters$LocalArea) %>%
    group_by(LocalArea) %>%
    slice_sample(n = n_hh, replace = FALSE) %>%
    ungroup()
  
  # Calculate endline proportion of binary variable
  endline_prop_disc <- mean(endline_sample_disc[[outcome]], na.rm = TRUE)
  
  ## RCS endline ####
  # Sample different clusters using PPS
  endline_cluster_sizes <- population %>%
    filter(year == endline) %>%
    group_by(LocalArea) %>%
    summarise(cluster_size = n_distinct(HHIntId), .groups = 'drop')
  endline_clusters_rcs <- endline_cluster_sizes %>%
    slice_sample(n = n_clusters, weight_by = cluster_size, replace = FALSE)
  endline_sample_rcs <- population %>%
    filter(year == endline, LocalArea %in% endline_clusters_rcs$LocalArea) %>%
    group_by(LocalArea) %>%
    slice_sample(n = n_hh, replace = FALSE) %>%
    ungroup()
  
  # Calculate endline proportion of binary variable
  endline_prop_rcs <- mean(endline_sample_rcs[[outcome]], na.rm = TRUE)
  
  # Calculate change in proportion
  change_disc <- endline_prop_disc - baseline_prop
  change_rcs <- endline_prop_rcs - baseline_prop
  
  # Store results
  results$baseline_prop[i] <- baseline_prop
  results$endline_prop_rcs[i] <- endline_prop_rcs
  results$endline_prop_disc[i] <- endline_prop_disc
  results$change_rcs[i] <- change_rcs
  results$change_disc[i] <- change_disc

}

# Visualize ====================================================================

# Calculate true value
true <- population %>% 
  group_by(year) %>% 
  summarise(true_prop = mean(.data[[outcome]], na.rm = TRUE), .groups = 'drop') %>% 
  arrange(year)
true_change <- true[2,2] - true[1,2]

# Reshape data from wide to long format
df_long <- results %>%
  select(change_rcs, change_disc) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Create scatterplot with vertical jitter
disc_rcs_ahri <- ggplot(df_long, aes(x = value, y = variable)) +
  geom_jitter(
    height = 0.1,        # Amount of vertical jitter
    width = 0,           # No horizontal jitter
    alpha = 0.3,         # Transparency to see overlapping points
    size = 2,            # Point size
    color = "gray28"  # Point color
  ) +
  # add true value
  geom_vline(xintercept = true_change[[1]], linetype = "dashed", size = 0.4) +
  annotate("text", x = -0.08, y = 2.5, label = "Population Mean Difference", hjust = -0.1) +
  scale_y_discrete(labels = c("change_disc" = "DISC", "change_rcs" = "RCS")) +
  labs(
    x = "Mean Difference",
    y = "Design"
  ) +
  theme_minimal()

ggsave(
  filename = paste0("Figures/", "2025-11-26 disc_rcs_ahri.pdf"),
  plot = disc_rcs_ahri,
  device = "pdf",
  width = 9,
  height = 5
)
