# Testing script to verify sampling design logic
# This script tests that each design scenario works as expected

source("disc_simulation.R")

# Helper function to test three-level designs
test_one_scenario <- function(icc = 0.1, sampling_scenario = "SSD", n_clusters = 20, browse_final_data = FALSE) {
  
  # Determine design based on sampling scenario
  if (sampling_scenario == "DDD") {
    design <- "Traditional RCS"
  } else if (sampling_scenario %in% c("SSD", "SDD")) {
    design <- "DISC"
  } else {
    stop("For Two_Level scenario, use test_two_level() function instead")
  }
  
  sd_level1 <- get_sd(icc = icc)
  sd_level2 <- get_sd(icc = icc * 0.5)
  level1_clusters <- create_level1_clusters(1000, sd_level1, mean = 'uncorrelated')
  level2_clusters <- create_level2_clusters(level1_clusters, n_level2_per_level1 = 20, sd_level2)
  all_sampled_level1_clusters <- sample_level1_clusters(level1_clusters, n_clusters, design = design)
  level2_samples <- sample_level2_clusters(all_sampled_level1_clusters, level2_clusters, 
                                           n_level2_per_level1 = 5, sampling_scenario, design)
  all_sampled_individuals <- sample_individuals(all_sampled_level1_clusters, 25, 
                                                sampling_scenario, level2_samples)
  final_data <- create_final_data(all_sampled_individuals, all_sampled_level1_clusters, 
                                  sampling_scenario = sampling_scenario, 
                                  level2_samples = level2_samples)
  
  if (browse_final_data) browser()
  
  return(list(
    final_data = final_data,
    level1_clusters = all_sampled_level1_clusters,
    level2_samples = level2_samples,
    individuals = all_sampled_individuals
  ))
}

# Helper function to test two-level designs
test_two_level <- function(icc = 0.1, design = "DISC", n_clusters = 20, browse_final_data = FALSE) {
  
  sd_level1 <- get_sd(icc = icc)
  level1_clusters <- create_level1_clusters(1000, sd_level1, mean = 'uncorrelated')
  all_sampled_level1_clusters <- sample_level1_clusters(level1_clusters, n_clusters, design = design)
  level2_samples <- NULL
  all_sampled_individuals <- sample_individuals(all_sampled_level1_clusters, 25, "Two_Level", level2_samples)
  final_data <- create_final_data(all_sampled_individuals, all_sampled_level1_clusters, 
                                  sampling_scenario = "Two_Level", level2_samples = NULL)
  
  if (browse_final_data) browser()
  
  return(list(
    final_data = final_data,
    level1_clusters = all_sampled_level1_clusters,
    individuals = all_sampled_individuals
  ))
}

# Main test function that runs verification for a design
test_design <- function(sampling_scenario, design = NULL, n_clusters = 20) {
  
  cat("\n", strrep("=", 80), "\n")
  cat("TESTING:", sampling_scenario, "\n")
  cat(strrep("=", 80), "\n\n")
  
  # Run the scenario
  if (sampling_scenario == "Two_Level") {
    if (is.null(design)) stop("Must specify design for Two_Level")
    result <- test_two_level(icc = 0.1, design = design, n_clusters = n_clusters)
  } else {
    result <- test_one_scenario(icc = 0.1, sampling_scenario = sampling_scenario, 
                                n_clusters = n_clusters, browse_final_data = FALSE)
  }
  
  final_data <- result$final_data
  
  # Count distinct clusters
  if (sampling_scenario != "Two_Level") {
    # Three-level design
    baseline <- final_data %>% filter(time == 0)
    endline <- final_data %>% filter(time == 1)
    
    # Get distinct level 1 and level 2 cluster IDs
    baseline_l1 <- unique(baseline$level1_cluster_id)
    endline_l1 <- unique(endline$level1_cluster_id)
    baseline_l2 <- unique(baseline$level2_cluster_id)
    endline_l2 <- unique(endline$level2_cluster_id)
    
    cat("LEVEL 1 CLUSTERS:\n")
    cat("  - Distinct at baseline:", length(baseline_l1), "\n")
    cat("  - Distinct at endline:", length(endline_l1), "\n")
    cat("  - Same clusters at both times?", setequal(baseline_l1, endline_l1), "\n")
    
    cat("\nLEVEL 2 CLUSTERS:\n")
    cat("  - Distinct at baseline:", length(baseline_l2), "\n")
    cat("  - Distinct at endline:", length(endline_l2), "\n")
    cat("  - Same clusters at both times?", setequal(baseline_l2, endline_l2), "\n")
    
    # Level 2 per level 1
    l2_per_l1 <- baseline %>%
      group_by(level1_cluster_id) %>%
      summarise(n_level2 = n_distinct(level2_cluster_id), .groups = "drop")
    cat("  - Level 2 per level 1: min =", min(l2_per_l1$n_level2), 
        ", max =", max(l2_per_l1$n_level2), "\n")
    
    # Individuals per level 2 cluster
    indiv_per_l2 <- baseline %>%
      group_by(level2_cluster_id) %>%
      summarise(n = n(), .groups = "drop")
    cat("\nINDIVIDUALS:\n")
    cat("  - Per level 2 cluster: min =", min(indiv_per_l2$n), 
        ", max =", max(indiv_per_l2$n), "\n")
    cat("  - Total at baseline:", nrow(baseline), "\n")
    cat("  - Total at endline:", nrow(endline), "\n")
    
  } else {
    # Two-level design
    baseline <- final_data %>% filter(time == 0)
    endline <- final_data %>% filter(time == 1)
    
    baseline_l1 <- unique(baseline$level1_cluster_id)
    endline_l1 <- unique(endline$level1_cluster_id)
    
    cat("LEVEL 1 CLUSTERS:\n")
    cat("  - Distinct at baseline:", length(baseline_l1), "\n")
    cat("  - Distinct at endline:", length(endline_l1), "\n")
    cat("  - Same clusters at both times?", setequal(baseline_l1, endline_l1), "\n")
    
    # Individuals per level 1 cluster
    indiv_per_l1 <- baseline %>%
      group_by(level1_cluster_id) %>%
      summarise(n = n(), .groups = "drop")
    cat("\nINDIVIDUALS:\n")
    cat("  - Per level 1 cluster: min =", min(indiv_per_l1$n), 
        ", max =", max(indiv_per_l1$n), "\n")
    cat("  - Total at baseline:", nrow(baseline), "\n")
    cat("  - Total at endline:", nrow(endline), "\n")
  }
  
  # Verify expected behavior
  cat("\nVERIFICATION:\n")
  
  if (sampling_scenario == "DDD") {
    l1_same <- setequal(baseline_l1, endline_l1)
    l2_same <- setequal(baseline_l2, endline_l2)
    cat("  Expected: Different L1, Different L2\n")
    if (!l1_same && !l2_same) {
      cat("  ✓ PASS\n")
    } else {
      cat("  ✗ FAIL\n")
    }
    
  } else if (sampling_scenario == "SSD") {
    l1_same <- setequal(baseline_l1, endline_l1)
    l2_same <- setequal(baseline_l2, endline_l2)
    cat("  Expected: Same L1, Same L2\n")
    if (l1_same && l2_same) {
      cat("  ✓ PASS\n")
    } else {
      cat("  ✗ FAIL\n")
    }
    
  } else if (sampling_scenario == "SDD") {
    l1_same <- setequal(baseline_l1, endline_l1)
    l2_same <- setequal(baseline_l2, endline_l2)
    cat("  Expected: Same L1, Different L2\n")
    if (l1_same && !l2_same) {
      cat("  ✓ PASS\n")
    } else {
      cat("  ✗ FAIL\n")
    }
    
  } else if (sampling_scenario == "Two_Level" && design == "DISC") {
    l1_same <- setequal(baseline_l1, endline_l1)
    cat("  Expected: Same L1\n")
    if (l1_same) {
      cat("  ✓ PASS\n")
    } else {
      cat("  ✗ FAIL\n")
    }
    
  } else if (sampling_scenario == "Two_Level" && design == "Traditional RCS") {
    l1_same <- setequal(baseline_l1, endline_l1)
    cat("  Expected: Different L1\n")
    if (!l1_same) {
      cat("  ✓ PASS\n")
    } else {
      cat("  ✗ FAIL\n")
    }
  }
  
  cat("\n")
  return(invisible(result))
}

# Run all tests

# Three-level designs
test_design("DDD", n_clusters = 10)
test_design("SDD", n_clusters = 10)
test_design("SSD", n_clusters = 10)

# Two-level designs
test_design("Two_Level", design = "DISC", n_clusters = 10)
test_design("Two_Level", design = "Traditional RCS", n_clusters = 10)
