##########################################################
## Visualize ACORN-like Geodemographic Clusters
## Companion to build_acorn_2023.R
##########################################################

# =========================================================================
# SETUP
# =========================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  ggplot2,
  ggdendro,
  reshape2,
  RColorBrewer,
  scales,
  patchwork,
  here
)

# Load clustering results
load(here::here("output", "clustering_results_2023.RData"))

# Define color palette for 10 groups
group_colors <- c(
  "#69D2E7", "#E0E4CC", "#B5B5B5", "#79BD9A", "#F38630",
  "#EDC951", "#8C8590", "#CDB380", "#547980", "#4ECDC4"
)
names(group_colors) <- LETTERS[1:10]

group_labels <- c(
  "A" = "Hispanic and Kids",
  "B" = "Wealthy Nuclear Families",
  "C" = "Middle Income, Single Family",
  "D" = "Native American",
  "E" = "Wealthy Urbanites",
  "F" = "Low Income and Diverse",
  "G" = "Old, Wealthy White",
  "H" = "Low Income Minority Mix",
  "I" = "Poor, African-American",
  "J" = "Residential Institutions, Young"
)

# =========================================================================
# 1. DENDROGRAM VISUALIZATION
# =========================================================================

cat("Creating dendrogram visualization...\n")

# Convert hclust to dendrogram data
dend_data <- dendro_data(wards_hclust, type = "rectangle")

# Get cluster assignments for coloring
cluster_x10 <- ward_cuts$X10[order.dendrogram(as.dendrogram(wards_hclust))]

# Create segment data with colors
segments <- segment(dend_data)

# Plot dendrogram
p_dendro <- ggplot() +
  geom_segment(
    data = segments,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "gray50"
  ) +
  theme_minimal() +
  labs(
    title = "Hierarchical Clustering Dendrogram",
    subtitle = "Ward's Method on 250 K-Means Cluster Centroids",
    x = "Cluster",
    y = "Distance"
  ) +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  here::here("output", "dendrogram.pdf"),
  p_dendro,
  width = 12,
  height = 6
)

# =========================================================================
# 2. SILHOUETTE PLOT
# =========================================================================

cat("Creating silhouette plot...\n")

p_silhouette <- ggplot(silhouette_scores, aes(x = k, y = silhouette)) +
  geom_line(color = "steelblue", size = 0.8) +
  geom_point(
    data = silhouette_scores %>% filter(k %in% c(2, 10, 31, 55)),
    color = "red",
    size = 3
  ) +
  geom_vline(
    xintercept = c(2, 10, 31, 55),
    linetype = "dashed",
    color = "red",
    alpha = 0.5
  ) +
  annotate(
    "text",
    x = c(2, 10, 31, 55),
    y = max(silhouette_scores$silhouette, na.rm = TRUE) * 0.95,
    label = c("X2", "X10", "X31", "X55"),
    color = "red",
    size = 3,
    hjust = -0.2
  ) +
  scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100)) +
  theme_minimal() +
  labs(
    title = "Silhouette Analysis",
    subtitle = "Optimal number of clusters indicated by local maxima",
    x = "Number of Clusters (log scale)",
    y = "Average Silhouette Width"
  )

ggsave(
  here::here("output", "silhouette_analysis.pdf"),
  p_silhouette,
  width = 10,
  height = 6
)

# =========================================================================
# 3. HEATMAPS BY DOMAIN
# =========================================================================

cat("Creating domain heatmaps...\n")

# Define variable groups by domain
domain_vars <- list(
  Demography = c(
    "pct_male_under_5", "pct_male_25_29", "pct_male_65_66", "pct_male_85_plus",
    "pct_female_under_5", "pct_female_25_29", "pct_female_65_66", "pct_female_85_plus"
  ),
  Race = c(
    "pct_white", "pct_black", "pct_american_indian", "pct_asian",
    "pct_hispanic", "pct_not_citizen"
  ),
  Education = c(
    "pct_less_than_hs", "pct_hs_graduate", "pct_some_college",
    "pct_bachelor", "pct_graduate_prof"
  ),
  Housing = c(
    "pct_vacant", "pct_renter", "pct_1unit_detached", "pct_50plus_units",
    "pct_mobile_home", "pct_built_2020_later", "pct_built_1939_earlier"
  ),
  Wealth = c(
    "gini_index", "median_gross_rent", "median_home_value"
  ),
  Income = c(
    "pct_income_under_10k", "pct_income_50k_60k", "pct_income_200k_plus",
    "pct_public_assistance", "pct_retirement_income"
  ),
  Industry = c(
    "pct_agriculture_mining", "pct_construction", "pct_manufacturing",
    "pct_professional_scientific", "pct_education_health", "pct_public_admin"
  ),
  Digital_Access = c(
    "pct_internet", "pct_broadband", "pct_no_internet",
    "pct_has_computer", "pct_no_computer"
  )
)

# Function to create heatmap for a domain
create_domain_heatmap <- function(domain_name, vars) {
  # Check which variables exist
  available_vars <- vars[vars %in% names(df_final)]

  if (length(available_vars) == 0) {
    return(NULL)
  }

  # Calculate index scores by X10 group
  domain_data <- df_final %>%
    select(X10, all_of(available_vars)) %>%
    group_by(X10) %>%
    summarise(across(
      everything(),
      ~mean(., na.rm = TRUE) / mean(df_final[[cur_column()]], na.rm = TRUE) * 100
    )) %>%
    pivot_longer(
      cols = -X10,
      names_to = "variable",
      values_to = "index_score"
    )

  # Clean variable names for display
  domain_data <- domain_data %>%
    mutate(variable = str_replace_all(variable, "pct_", "% ") %>%
             str_replace_all("_", " ") %>%
             str_to_title())

  # Create heatmap
  p <- ggplot(domain_data, aes(x = variable, y = X10, fill = index_score)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(index_score)), size = 2.5) +
    scale_fill_gradientn(
      colours = c("#D7191C", "#FDAE61", "#F5F5F5", "#A6D96A", "#1A9641"),
      values = rescale(c(0, 50, 100, 150, 200)),
      limits = c(0, 200),
      oob = squish,
      name = "Index\nScore"
    ) +
    scale_y_discrete(limits = rev(LETTERS[1:10])) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 10),
      axis.title = element_blank(),
      legend.position = "right",
      panel.grid = element_blank()
    ) +
    labs(
      title = paste(domain_name, "Variables by Cluster Group"),
      subtitle = "Index Score: 100 = National Average"
    )

  return(p)
}

# Create and save heatmaps for each domain
for (domain in names(domain_vars)) {
  p <- create_domain_heatmap(domain, domain_vars[[domain]])
  if (!is.null(p)) {
    ggsave(
      here::here("output", paste0("heatmap_", tolower(domain), ".pdf")),
      p,
      width = 10,
      height = 6
    )
  }
}

# =========================================================================
# 4. GROUP DISTRIBUTION BAR CHART
# =========================================================================

cat("Creating group distribution chart...\n")

group_counts <- df_final %>%
  count(X10) %>%
  mutate(
    pct = n / sum(n) * 100,
    label = group_labels[as.character(X10)]
  )

p_distribution <- ggplot(group_counts, aes(x = X10, y = pct, fill = X10)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 3) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  labs(
    title = "Distribution of Census Tracts by Group",
    subtitle = "Percentage of all US Census Tracts",
    x = "Group",
    y = "Percentage of Tracts"
  )

ggsave(
  here::here("output", "group_distribution.pdf"),
  p_distribution,
  width = 10,
  height = 6
)

# =========================================================================
# 5. RADAR/SPIDER CHARTS FOR KEY VARIABLES
# =========================================================================

cat("Creating profile comparison charts...\n")

# Select key variables for radar chart
key_vars <- c(
  "pct_white", "pct_black", "pct_hispanic", "pct_asian",
  "pct_bachelor", "pct_income_200k_plus", "pct_income_under_10k",
  "pct_renter", "median_home_value", "pct_broadband"
)

# Check which exist
key_vars <- key_vars[key_vars %in% names(df_final)]

# Calculate normalized profiles
profiles_normalized <- df_final %>%
  select(X10, all_of(key_vars)) %>%
  group_by(X10) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = -X10, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  mutate(value_scaled = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup()

# Create faceted bar chart (alternative to radar)
p_profiles <- ggplot(profiles_normalized, aes(x = X10, y = value_scaled, fill = X10)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~variable, scales = "free_y", ncol = 5) +
  scale_fill_manual(values = group_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 8)
  ) +
  labs(
    title = "Cluster Profiles: Key Variables",
    subtitle = "Values scaled 0-1 within each variable",
    x = "Group",
    y = "Scaled Value"
  )

ggsave(
  here::here("output", "cluster_profiles_comparison.pdf"),
  p_profiles,
  width = 14,
  height = 8
)

# =========================================================================
# 6. POPULATION PYRAMIDS
# =========================================================================

cat("Creating population pyramids...\n")

# Get age variables
male_age_vars <- names(df_final)[grepl("pct_male_", names(df_final))]
female_age_vars <- names(df_final)[grepl("pct_female_", names(df_final))]

# Calculate means by group
age_data <- df_final %>%
  select(X10, all_of(c(male_age_vars, female_age_vars))) %>%
  group_by(X10) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(cols = -X10, names_to = "age_var", values_to = "pct") %>%
  mutate(
    gender = ifelse(grepl("male", age_var), "Male", "Female"),
    age_group = str_remove(age_var, "pct_(fe)?male_") %>%
      str_replace_all("_", " ") %>%
      str_replace("plus", "+"),
    pct = ifelse(gender == "Female", -pct, pct)  # Negative for female
  )

# Create pyramid plot for all groups
p_pyramid <- ggplot(age_data, aes(x = age_group, y = pct, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~X10, ncol = 5) +
  scale_fill_manual(values = c("Male" = "#1f77b4", "Female" = "#e377c2")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 8),
    legend.position = "bottom"
  ) +
  labs(
    title = "Age-Sex Structure by Cluster Group",
    subtitle = "Population pyramids showing demographic composition",
    x = "Age Group",
    y = "Percentage of Population",
    fill = "Gender"
  )

ggsave(
  here::here("output", "population_pyramids.pdf"),
  p_pyramid,
  width = 14,
  height = 10
)

# =========================================================================
# 7. SUMMARY STATISTICS TABLE
# =========================================================================

cat("Creating summary statistics...\n")

# Key summary statistics by group
summary_stats <- df_final %>%
  group_by(X10) %>%
  summarise(
    n_tracts = n(),
    avg_pop_density = mean(pop_density, na.rm = TRUE),
    avg_median_income = mean((pct_income_100k_125k + pct_income_125k_150k +
                                pct_income_150k_200k + pct_income_200k_plus) * 100,
                             na.rm = TRUE),
    avg_pct_bachelor_plus = mean(pct_bachelor + pct_graduate_prof, na.rm = TRUE),
    avg_pct_homeowner = mean(100 - pct_renter, na.rm = TRUE),
    avg_median_home_value = mean(median_home_value, na.rm = TRUE),
    avg_pct_broadband = mean(pct_broadband, na.rm = TRUE)
  ) %>%
  mutate(
    group_label = group_labels[as.character(X10)]
  ) %>%
  select(X10, group_label, everything())

write_csv(summary_stats, here::here("output", "group_summary_statistics.csv"))

# =========================================================================
# COMPLETE
# =========================================================================

cat("\n")
cat("=" , rep("=", 50), "=\n", sep = "")
cat("  Visualization Complete!\n")
cat("=" , rep("=", 50), "=\n", sep = "")
cat("\nOutput files created in: output/\n")
cat("  - dendrogram.pdf\n")
cat("  - silhouette_analysis.pdf\n")
cat("  - heatmap_*.pdf (by domain)\n")
cat("  - group_distribution.pdf\n")
cat("  - cluster_profiles_comparison.pdf\n")
cat("  - population_pyramids.pdf\n")
cat("  - group_summary_statistics.csv\n")
