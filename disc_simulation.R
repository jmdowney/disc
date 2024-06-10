# 1. setup ####
library(SimEngine)
library(tidyverse)

# create new simulation #
sim <- new_sim()

# 2. create data ####

## a. generate "population" of clusters ####
create_clusters <- function(n, mean, sd) {
  
  # generate cluster random effects
  cluster_effects <- rnorm(n = n, mean = mean, sd = sd) 
  
  # randomly assign half to control half to intervention
  assignment <- rbinom(n = n, size = 1, prob = 0.5)
  
  # create df including cluster effect, intervention vs control assignment, and cluster ID
  dat <- bind_cols(
    tibble(cluster_effect = cluster_effects),
    tibble(intervention = assignment)
  ) %>% 
    tibble::rowid_to_column("cluster_id")
  
  return(dat)
  
}

# test
dat <- create_clusters(1000, 0, 1)

# visualize
hist(dat$cluster_effect)
  
## b. sample 100 clusters at baseline and endline for each design ####
sample_clusters <- function(n) {
  
  # traditional repeated cross sectional design, baseline
  sample_baseline_traditional <- dat %>% 
    # ensure half of clusters sampled are intervention and half control
    group_by(intervention) %>% 
    slice_sample(n = n/2) %>% 
    mutate(time = 1) %>% 
    mutate(design = 'traditional')
  
  # traditional repeated cross sectional design, endline
  sample_endline_traditional <- dat %>% 
    # ensure half of clusters sampled are intervention and half control
    group_by(intervention) %>% 
    slice_sample(n = n/2) %>% 
    mutate(time = 2) %>% 
    mutate(design = 'traditional')
  
  # DISC design, baseline
  sample_baseline_disc <- dat %>% 
    # ensure half of clusters sampled are intervention and half control
    group_by(intervention) %>% 
    slice_sample(n = n/2) %>% 
    mutate(time = 1) %>% 
    mutate(design = 'disc')
  
  # DISC design, endline - same as DISC baseline
  sample_endline_disc <- sample_baseline_disc %>% 
    mutate(time = 2) %>% 
    mutate(design = 'disc')
  
  # now bind into one df to facilitate individual sampling
  all_sampled_clusters <- bind_rows(
    sample_baseline_traditional,
    sample_endline_traditional,
    sample_baseline_disc,
    sample_endline_disc
  ) %>% 
    # add another ID, to match individuals to clusters
    tibble::rowid_to_column("individual_matching_id")
  
  return(all_sampled_clusters)
}

# test
all_sampled_clusters <- sample_clusters(100)

# visualize
hist(all_sampled_clusters$cluster_effect)

## c. sample individuals from each cluster ####
sample_individuals <- function(n, mean, sd) {
  
  # initialize empty list for individuals
  individual_samples <- list()
  
  # loop through all sampled clusters to "sample" individuals for each one
  for (individual_matching_id in all_sampled_clusters$individual_matching_id) {
    # generate individual samples
    samples <- rnorm(n = n, mean = mean, sd = sd)
    # create df with cluster ID, individual ID, and individual samples
    df <- tibble(individual_matching_id = individual_matching_id, individual_id = 1:n, individual_residual = samples)
    # append to list 
    individual_samples[[individual_matching_id]] <- df
  }
  
  # combine into one df
  all_sampled_individuals <- tibble(bind_rows(individual_samples))
  
  return(all_sampled_individuals)
  
}

# test
all_sampled_individuals <- sample_individuals(25, 0, 1)

# visualize
hist(all_sampled_individuals$residual)

## d. join cluster and individual dfs ####
final_sample <- all_sampled_individuals %>% 
  left_join(all_sampled_clusters, by = 'individual_matching_id') %>% 
  select(-individual_matching_id)

# 3. create model ####

# to do: write function to construct outcomes from final_sample, using
# mixed-effects model 

# 4. set simulation levels ####

# to do: these should be different values of cluster-level SD 
# (+ different values of individual-level SD?)

# 5. create simulation script ####

# to do

# 6. run simulation, view, summarize #### 
