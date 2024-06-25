# 1. setup ####
library(SimEngine)
library(tidyverse)
library(lme4)

# create new simulation #
sim <- new_sim()

# 2. create data ####

## a. generate "population" of clusters ####
create_clusters <- function(n, sd) {
  
  # generate cluster random effects
  cluster_effects <- rnorm(n = n, 0, sd = sd) 
  
  # randomly assign half to control half to intervention
  # maybe to do: change this to take a vector of n/2 0s and n/2 1s and permute randomly
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
dat <- create_clusters(1000, 1)

# visualize
hist(dat$cluster_effect)
  
## b. sample 100 clusters at baseline and endline for each design ####
sample_clusters <- function(n, design) {
  
  # to do: add conditional logic for design = traditional, design = disc
  if (design == 'traditional') {
    
    # traditional repeated cross sectional design, baseline
    sample_baseline <- dat %>% 
      # ensure half of clusters sampled are intervention and half control
      group_by(intervention) %>% 
      slice_sample(n = n/2) %>% 
      mutate(time = 0)
  
    # traditional repeated cross sectional design, endline
    sample_endline <- dat %>% 
      # ensure half of clusters sampled are intervention and half control
      group_by(intervention) %>% 
      slice_sample(n = n/2) %>% 
      mutate(time = 1)
    
    # combine into one df
    all_sampled_clusters <- bind_rows(
      sample_baseline,
      sample_endline
    ) %>% 
      # add another ID, to match individuals to clusters
      tibble::rowid_to_column("individual_matching_id")  %>% 
      mutate(design = 'traditional')
    
    return(all_sampled_clusters)
    
  } else if (design == 'disc') {
    
    # DISC design, baseline
    sample_baseline <- dat %>% 
      # ensure half of clusters sampled are intervention and half control
      group_by(intervention) %>% 
      slice_sample(n = n/2) %>% 
      mutate(time = 0)
    
    # DISC design, endline - same as DISC baseline
    sample_endline <- sample_baseline %>% 
      mutate(time = 1) 
    
    # now bind into one df to facilitate individual sampling
    all_sampled_clusters <- bind_rows(
      sample_baseline,
      sample_endline
    ) %>% 
      # add another ID, to match individuals to clusters
      tibble::rowid_to_column("individual_matching_id") %>% 
      mutate(design = 'disc')
    
    return(all_sampled_clusters)
    
  } else {
    
    stop("design can only be 'traditional' or 'disc'")
    
  }
}

# test
all_sampled_clusters <- sample_clusters(100, 'traditional')

# visualize
hist(all_sampled_clusters$cluster_effect)

## c. sample individuals from each cluster ####
sample_individuals <- function(n) {
  
  # initialize empty list for individuals
  individual_samples <- list()
  
  # loop through all sampled clusters to "sample" individuals for each one
  for (individual_matching_id in all_sampled_clusters$individual_matching_id) {
    # generate individual samples
    samples <- rnorm(n = n, 0, 1)
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
all_sampled_individuals <- sample_individuals(25)

# visualize
hist(all_sampled_individuals$individual_residual)

# 3. create model ####

# set values for mean outcome at baseline and follow-up, treatment effect the same;
# starting with simplest design of mean outcome at baseline and follow-up 
# among untreated group = 0
fit_model <- function(te) {
  
  final_sample <- all_sampled_individuals %>% 
    # join individual and cluster level data
    left_join(all_sampled_clusters, by = 'individual_matching_id') %>% 
    select(-individual_matching_id) %>% 
    # calculate outcomes
    mutate(treatment_effect = te) %>% 
    mutate(outcome = 0 + cluster_effect + treatment_effect*intervention + individual_residual)
  
  # fit random effects model 
  model <- lmer(outcome ~ intervention*time + (1 | cluster_id), data = final_sample)
  
  # get estimate of intervention effect
  summary <- summary(model)
  return(summary$coefficients[2,1])
  
}

# test
summary <- fit_model(1)

# 4. set simulation levels ####

# different values of cluster-level SD + different designs (traditional vs disc)
sim %<>% set_levels(
  sd = c(1, 2, 3),
  design = c("traditional","disc"),
  treatment_effect = c(1, 2, 3)
)

# 5. create simulation script ####
sim %<>% set_script(function() {
  dat <- create_clusters(1000, sd = L$sd)
  all_sampled_clusters <- sample_clusters(100, design = L$design)
  variance <- fit_model(te = L$te)
  return (list("variance"=variance))
})

# 6. run simulation, view, summarize #### 
sim %<>% run()
