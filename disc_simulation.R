# 1. setup ####
library(SimEngine)

# 2. create data & helper functions ####

## a. generate "population" of clusters ####
create_clusters <- function(n, sd, mean) {
  
  # if mean = 'correlated':
  if (mean == 'correlated') {
    
    # randomly assign half to control half to treatment
    control <- rep(0, n/2)
    treatment <- rep(1, n/2)
    intervention <- sample(c(control, treatment))
    assignment_df <- data.frame(intervention) %>% 
      mutate(cluster_effect = rnorm(n = n, mean = intervention + 1, sd = sd))
    
    # create df including cluster effect, intervention vs control assignment, and cluster ID
    dat <- assignment_df %>% 
      tibble::rowid_to_column("cluster_id")
    
    return(dat)
  
  # if mean = 'uncorrelated':
  } else if (mean == 'uncorrelated') {
    
    # randomly assign half to control half to treatment
    control <- rep(0, n/2)
    treatment <- rep(1, n/2)
    intervention <- sample(c(control, treatment))
    assignment_df <- data.frame(intervention) %>% 
      mutate(cluster_effect = rnorm(n = n, mean = 0, sd = sd))
    
    # create df including cluster effect, treatment vs control assignment, and cluster ID
    dat <- assignment_df %>% 
      tibble::rowid_to_column("cluster_id")
    
    return(dat)
    
  } else {
    
    stop("mean can only be 'correlated' (meaning cluster effect has mean of 1 in control and 2 in intervention areas) or 'uncorrelated' (meaning cluster effect has mean of 0)")
    
  }
}
  
## b. sample clusters at baseline and endline for each design ####
sample_clusters <- function(dat, n, design) {
  
  # separate designs for traditional RCS vs DISC
  if (design == 'Traditional RCS') {
    
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
      mutate(design = 'Traditional RCS')
    
    return(all_sampled_clusters)
    
  } else if (design == 'DISC') {
    
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
      mutate(design = 'DISC')
    
    return(all_sampled_clusters)
    
  } else {
    
    stop("design can only be 'Traditional RCS' or 'DISC'")
    
  }
}

## c. sample individuals from each cluster ####
sample_individuals <- function(all_sampled_clusters, n) {
  
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

## d. helper functions ####

# get SD from ICC 
get_sd <- function(icc) {
  
  sd <- sqrt(icc/(1-icc))
  return(sd)
  
}

# get ICC from SD 
get_icc <- function(sd) {
  
  icc <- sd^2/(sd^2 + 1)
  return(icc)
  
}

# 3. create model ####

# set values for mean outcome at baseline and follow-up, treatment effect the same;
# starting with simplest design of mean outcome at baseline and follow-up 
# among untreated group = 0
fit_model <- function(all_sampled_individuals, all_sampled_clusters, te = 1) {
  
  final_sample <- all_sampled_individuals %>% 
    # join individual and cluster level data
    left_join(all_sampled_clusters, by = 'individual_matching_id') %>% 
    select(-individual_matching_id) %>% 
    # calculate outcomes
    mutate(treatment_effect = te) %>% 
    mutate(outcome = 0 + cluster_effect + treatment_effect*intervention*time + time + individual_residual)
  
  # fit model 
  model <- lm(outcome ~ intervention*time, data = final_sample)
  # get estimate of intervention effect
  summary <- summary(model)
  return(summary$coefficients[4,1])
  
}

# 4. set simulation levels ####

# create new simulation #
sim <- new_sim()

# different values of ICC + different designs (traditional vs disc)
sim %<>% set_levels(
  icc = seq(from = 0, to = 0.2, by = 0.05),
  design = c("Traditional RCS", "DISC"),
  n_clusters = seq(from = 10, to = 100, by = 10)
)

# 5. create simulation script ####
sim %<>% set_script(function() {
  sd <- get_sd(icc = L$icc)
  dat <- create_clusters(1000, sd, mean = 'uncorrelated')
  all_sampled_clusters <- sample_clusters(dat, L$n_clusters, design = L$design)
  all_sampled_individuals <- sample_individuals(all_sampled_clusters, 25)
  estimate <- fit_model(all_sampled_individuals, all_sampled_clusters, te = 1)
  return (list("estimate"=estimate, "sd"=sd))
})

sim %<>% set_config(
  num_sim = 1000,
  packages = c("tidyverse", "lme4", "stringr")
)

# 6. run simulation, summarize, save #### 
sim %<>% run()
sim_lm <- sim
sim_lm %>% SimEngine::summarize(
  list(stat = "sd", x = "estimate")
)

save(sim_lm, file = "simulation_results.RData")

# 7. viz ####

# initial simulation results
(results <- sim %>% 
   SimEngine::summarize(
     list(stat = "sd", x = "estimate")
   ) 
 %>% 
  filter(n_clusters == 100) %>% 
  ggplot(aes(icc, sd_estimate, color = design)) + 
   geom_line() +
   xlab('Intraclass correlation coefficient') +
   ylab('Estimated total standard deviation') + 
   labs(title = str_wrap('Estimated total standard deviation under traditional RCS and DISC designs, for different values of intraclass correlation coefficient', 60)) +
   scale_fill_discrete(name = 'Design')
 )

# analytical, assuming m = 25
n <- sim$levels$n_clusters*25
icc <- sim$levels$icc

analytical_disc <- expand.grid(n = n, icc = icc) %>% 
  mutate(var = 8/n,
         method = 'Analytical',
         design = 'DISC')

analytical_rcs <- expand.grid(n = n, icc = icc) %>% 
  mutate(var = 8*( 25*((get_sd(icc))^2) + 1)/n,
         method = 'Analytical',
         design = 'Traditional RCS')

# compare simulation results and analytical calculations for ICC = 0.2
(results <- sim %>% 
  SimEngine::summarize(
    list(stat = "sd", x = "estimate")
    ) %>% 
  mutate(n = n_clusters*25,
         var = sd_estimate^2,
         method = 'Simulation') %>% 
  select(n, icc, var, method, design) %>% 
  rbind(analytical_disc) %>%
  rbind(analytical_rcs) %>% 
  filter(icc == 0.2) %>% 
    ggplot(aes(n, var, linetype = method)) + 
    geom_line(position = position_jitter(w=0, h=0.002)) +
    facet_wrap(vars(design)) +
    xlab('Total individuals (n)') +
    ylab('Total variance') + 
    labs(title = str_wrap('Estimated total variance under DISC and traditional RCS designs, comparing empirical and theoretical variance', 60),
         linetype = 'Method') 
)

# analytical, varying m, assuming 100 clusters
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

# compare designs for analytical calculations across different ICC values
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
  
