# How to Build Your ACORN-like Demographic Dataset

## Quick Start

### Step 1: Install R and RStudio
Download and install:
- [R (version 4.3+)](https://cran.r-project.org/)
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/)

### Step 2: Get a Census API Key
1. Go to: https://api.census.gov/data/key_signup.html
2. Fill out the form (instant approval)
3. Check your email for the API key

### Step 3: Run the Script

Open RStudio and run:

```r
# Set your API key (only needed once - it saves to your .Renviron)
library(tidycensus)
census_api_key("YOUR_API_KEY_HERE", install = TRUE)

# Restart R (Session > Restart R), then run:
source("code/build_acorn_2023.R")
```

### Step 4: Wait for Data Download
- Downloads ~85,000 census tracts across all 50 states + DC + PR
- Takes approximately **15-30 minutes** depending on internet speed
- Progress bar shows download status

### Step 5: Find Your Output Files

After completion, check the `output/` folder:

| File | Description |
|------|-------------|
| `acs_2023_demographic_clusters.csv` | Main dataset with all tracts and cluster assignments |
| `cluster_profiles_2023.csv` | Mean values for each X10 group |
| `cluster_index_profiles_2023.csv` | Index scores (100 = national average) |
| `us_tract_clusters_2023.gpkg` | Spatial file for GIS software |
| `clustering_results_2023.RData` | R objects for further analysis |

### Step 6: Generate Visualizations (Optional)

```r
source("code/visualize_clusters.R")
```

This creates PDF charts in `output/`:
- Dendrograms
- Heatmaps by domain
- Population pyramids
- Silhouette analysis

---

## Using the Interactive GUI

For a visual interface with custom weighting and Excel export:

```r
# Install Shiny if needed
install.packages("shiny")

# Launch the GUI
shiny::runApp("app")
```

The GUI allows you to:
- Adjust weights for each variable domain
- Preview cluster assignments in real-time
- Export to Excel sorted by state/county
- Create custom cluster profiles

---

## Configuration Options

Edit these values at the top of `build_acorn_2023.R`:

```r
TARGET_YEAR <- 2023      # Change to 2024 after Jan 29, 2026
N_CLUSTERS <- 250        # Number of k-means clusters
N_RUNS <- 100            # K-means iterations (more = better, slower)
MAX_ITER <- 100000       # Max iterations per run
SEED <- 7777             # Random seed for reproducibility
```

### For Faster Testing
```r
N_RUNS <- 10             # Quick test (less optimal clusters)
MAX_ITER <- 1000         # Faster convergence
```

### For Production Quality
```r
N_RUNS <- 1000           # Better optimization
MAX_ITER <- 100000       # Full convergence
```

---

## Troubleshooting

### "API key not found"
```r
Sys.getenv("CENSUS_API_KEY")  # Should show your key
# If empty, re-run:
census_api_key("YOUR_KEY", install = TRUE)
# Then restart R
```

### "Error downloading state XX"
Some territories may fail. The script continues with available states.

### Memory issues
For systems with <16GB RAM:
```r
# Download one state at a time (edit the script)
states <- c("CA")  # Test with California first
```

### Package installation fails
```r
# Install manually:
install.packages(c("tidycensus", "tidyverse", "sf", "cluster", "units", "progress"))
```

---

## Understanding the Output

### Cluster Hierarchy
| Level | Clusters | Use Case |
|-------|----------|----------|
| X2 | 2 | Broadest segmentation |
| X10 | 10 | **Primary classification** |
| X31 | 31 | Detailed segments |
| X55 | 55 | Fine-grained types |
| cluster | 250 | Micro-clusters |

### X10 Group Labels
| Code | Name | Characteristics |
|------|------|-----------------|
| A | Hispanic and Kids | Young families, Hispanic population |
| B | Wealthy Nuclear Families | High income, married couples, suburban |
| C | Middle Income, Single Family | Moderate income, homeowners |
| D | Native American | Rural, Native American population |
| E | Wealthy Urbanites | High income, urban, educated |
| F | Low Income and Diverse | Mixed demographics, lower income |
| G | Old, Wealthy White | Older, affluent, predominantly white |
| H | Low Income Minority Mix | Diverse, lower income |
| I | Poor, African-American | Lower income, African-American |
| J | Residential Institutions | Group quarters, students, military |

### Index Scores
- **100** = National average
- **>100** = Above average (overrepresented)
- **<100** = Below average (underrepresented)

Example: Index of 150 for "Pct Bachelor Degree" means 50% more college graduates than national average.
