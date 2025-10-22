# DISC

## Overview

The simulation supports **TWO separate comparisons**:

1. **Three-level designs**: DDD vs SSD vs SDD
2. **Two-level designs**: Traditional RCS vs DISC

These are run as separate simulations.

---

## Sampling Scenarios 

### **Three-Level Designs** (3 scenarios)

| Code | Name | Level 1 | Level 2 | Individuals | Design Used |
|------|------|---------|---------|-------------|-------------|
| **DDD** | Different-Different-Different | Different clusters at T0/T1 | Different subclusters at T0/T1 | Different individuals at T0/T1 | Traditional 3-level RCS |
| **SDD** | Same-Different-Different | Same clusters at T0/T1 | Different subclusters at T0/T1 | Different individuals at T0/T1 | DISC 3-level |
| **SSD** | Same-Same-Different | Same clusters at T0/T1 | Same subclusters at T0/T1 | Different individuals at T0/T1 | DISC 3-level |

### **Two-Level Designs** (2 scenarios)

| Code | Name | Level 1 | Individuals | Design Used |
|------|------|---------|-------------|-------------|
| **Two_Level** | Traditional RCS | Different clusters at T0/T1 | Different individuals at T0/T1 | Traditional 2-level RCS |
| **Two_Level** | DISC | Same clusters at T0/T1 | Different individuals at T0/T1 | DISC 2-level |

---

## Running Simulations

### **Option 1: Three-Level Simulation**

```r
# In disc_simulation.R, use this configuration (lines 406-410):
sim %<>% set_levels(
  icc = seq(from = 0, to = 0.2, by = 0.05),
  n_clusters = seq(from = 10, to = 100, by = 10),
  sampling_scenario = c("DDD", "SSD", "SDD")  # Three-level designs
)
```

This compares:
- **DDD**: Three-level RCS (different level 1 clusters, different level 2 clusters, different individuals)
- **SSD**: Three-level DISC with same level 2 clusters
- **SDD**: Three-level DISC with different level 2 clusters

**Total scenarios**: 5 (ICC) × 10 (n_clusters) × 3 (scenarios) = **150 scenarios**

Results saved to: `simulation_results_linear_drdid_3level.RData`

Note: This version takes a very long time to run! Only overwrite saved .Rdata if necessary.

---

### **Option 2: Two-Level Simulation**

```r
# In disc_simulation.R, comment out lines 406-410 and uncomment lines 413-418:
sim %<>% set_levels(
  icc = seq(from = 0, to = 0.2, by = 0.05),
  design = c("Traditional RCS", "DISC"),
  n_clusters = seq(from = 10, to = 100, by = 10),
  sampling_scenario = c("Two_Level")  # Two-level designs only
)
```

This compares:
- **Traditional RCS**: Different level 1 clusters at each time
- **DISC**: Same level 1 clusters at each time

**Total scenarios**: 5 (ICC) × 2 (designs) × 10 (n_clusters) = **100 scenarios**

Results saved to: `simulation_results_linear_drdid.RData`

---

## Testing All Designs

### **Run Comprehensive Tests**

Due to the length of time it takes to run simulations, there is also a testing script (`test_designs.R`) which verifies that all sampling scenarios work correctly:

```r
source("test_designs.R")
```

This will test all 5 designs and verify:
- **Cluster consistency**: Are clusters sampled correctly at each level?
- **Sample sizes**: How many clusters and individuals per design?
- **Expected behavior**: Does each design match its specification?

Output shows:
- ✓ PASS or ✗ FAIL for each design
- Detailed sample size information
- Verification that clusters are sampled as expected

### **Test Individual Scenarios (Optional)**

If you want to inspect a specific design interactively:

```r
source("disc_simulation.R")

# Three-level designs
result <- test_one_scenario(sampling_scenario = "DDD")  # or "SSD" or "SDD"
View(result$final_data)

# Two-level designs  
result <- test_two_level(design = "DISC")  # or "Traditional RCS"
View(result$final_data)

# Use browser() to pause and inspect
result <- test_one_scenario(sampling_scenario = "SSD", browse_final_data = TRUE)
```

---

## Understanding composite_id

### **Three-Level Designs**
Format: `"level1_level2_time"`

Example: `"5_3_0"`
- **5** = Level 1 cluster (village 5)
- **3** = Level 2 cluster (neighborhood 3 within village 5)
- **0** = Time (0=baseline, 1=endline)

### **Two-Level Designs**
Format: `"level1_time"`

Example: `"5_0"`
- **5** = Level 1 cluster (village 5)
- **0** = Time (0=baseline, 1=endline)

---

## Simulation Parameters

| Parameter | Value | Notes |
|-----------|-------|-------|
| Level 1 clusters (population) | 1000 | Large population to sample from |
| Level 2 clusters (population) | 20 per level 1 | Only for three-level designs |
| Level 2 clusters (sampled) | 5 per level 1 | 25% sampling rate |
| Individuals (sampled) | 25 per cluster | Per level 2 cluster (3-level) or level 1 cluster (2-level) |
| ICC Level 1 | 0, 0.05, 0.1, 0.15, 0.2 | Varies across simulation |
| ICC Level 2 | 0.5 × Level 1 ICC | Only for three-level designs |
| Treatment Effect | 1.0 | True effect size |

---

## Output Files

After running simulations, you'll have either:

1. **simulation_results_linear_drdid.RData** - Two-level results
2. **simulation_results_linear_drdid_3level.RData** - Three-level results

---

## Files in This Project

### **Main Simulation**
- **disc_simulation.R** - Main simulation code with all functions

### **Testing**
- **test_designs.R** - Comprehensive verification tests for all 5 designs

### **Results & Figures**
- Run simulations to generate `.RData` files
- Use figure scripts (`*_figure.R`) to visualize results
