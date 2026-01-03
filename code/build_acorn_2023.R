##########################################################
## Build ACORN-like Geodemographic Clusters
## Using 2019-2023 ACS 5-Year Estimates
## Based on methodology by Seth Spielman & Alex Singleton
## Paper: https://doi.org/10.1080/00045608.2015.1052335
##########################################################

# =========================================================================
# 1. SETUP & DEPENDENCIES
# =========================================================================

# Install required packages if missing
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidycensus,    # Census API access
  tidyverse,     # Data manipulation
  sf,            # Spatial features
  cluster,       # Clustering algorithms
  units,         # Unit conversions
  parallel,      # Parallel processing
  progress       # Progress bars
)

# Set your Census API key (get one at https://api.census.gov/data/key_signup.html)
# Uncomment and set your key:
# census_api_key("YOUR_CENSUS_API_KEY_HERE", install = TRUE)

# Set target year
# Use 2023 for 2019-2023 ACS 5-Year Estimates (released Dec 2024)
# Change to 2024 after Jan 29, 2026 for 2020-2024 estimates
TARGET_YEAR <- 2023

# Configuration
N_CLUSTERS <- 250           # Number of initial k-means clusters
N_RUNS <- 100               # Number of k-means runs (paper used 100,000)
MAX_ITER <- 100000          # Max iterations per k-means run (paper used 1,000,000)
SEED <- 7777                # Random seed for reproducibility

set.seed(SEED)

cat("=== Building ACORN-like Geodemographic Classification ===\n")
cat("Target Year:", TARGET_YEAR, "(ACS 5-Year Estimates)\n")
cat("Clusters:", N_CLUSTERS, "| Runs:", N_RUNS, "| Max Iterations:", MAX_ITER, "\n\n")

# =========================================================================
# 2. COMPLETE VARIABLE MAPPING (All 136 Original Variables + Modern Additions)
# =========================================================================

# The original study used 136 variables across 10 domains.
# Below is the complete mapping to current ACS variable IDs.

# --- DOMAIN 1: DEMOGRAPHY (Age Structure by Sex) ---
# Denominator: B01001_001 (Total Population)

age_vars <- c(
  # Male age cohorts
  "B01001_003",  # Male Under 5 Years
  "B01001_004",  # Male 5 To 9 Years
  "B01001_005",  # Male 10 To 14 Years
  "B01001_006",  # Male 15 To 17 Years
  "B01001_007",  # Male 18 And 19 Years
  "B01001_008",  # Male 20 Years
  "B01001_009",  # Male 21 Years
  "B01001_010",  # Male 22 To 24 Years
  "B01001_011",  # Male 25 To 29 Years
  "B01001_012",  # Male 30 To 34 Years
  "B01001_013",  # Male 35 To 39 Years
  "B01001_014",  # Male 40 To 44 Years
  "B01001_015",  # Male 45 To 49 Years
  "B01001_016",  # Male 50 To 54 Years
  "B01001_017",  # Male 55 To 59 Years
  "B01001_018",  # Male 60 And 61 Years
  "B01001_019",  # Male 62 To 64 Years
  "B01001_020",  # Male 65 And 66 Years
  "B01001_021",  # Male 67 To 69 Years
  "B01001_022",  # Male 70 To 74 Years
  "B01001_023",  # Male 75 To 79 Years
  "B01001_024",  # Male 80 To 84 Years
  "B01001_025",  # Male 85 Years And Over

  # Female age cohorts
  "B01001_027",  # Female Under 5 Years
  "B01001_028",  # Female 5 To 9 Years
  "B01001_029",  # Female 10 To 14 Years
  "B01001_030",  # Female 15 To 17 Years
  "B01001_031",  # Female 18 And 19 Years
  "B01001_032",  # Female 20 Years
  "B01001_033",  # Female 21 Years
  "B01001_034",  # Female 22 To 24 Years
  "B01001_035",  # Female 25 To 29 Years
  "B01001_036",  # Female 30 To 34 Years
  "B01001_037",  # Female 35 To 39 Years
  "B01001_038",  # Female 40 To 44 Years
  "B01001_039",  # Female 45 To 49 Years
  "B01001_040",  # Female 50 To 54 Years
  "B01001_041",  # Female 55 To 59 Years
  "B01001_042",  # Female 60 And 61 Years
  "B01001_043",  # Female 62 To 64 Years
  "B01001_044",  # Female 65 And 66 Years
  "B01001_045",  # Female 67 To 69 Years
  "B01001_046",  # Female 70 To 74 Years
  "B01001_047",  # Female 75 To 79 Years
  "B01001_048",  # Female 80 To 84 Years
  "B01001_049"   # Female 85 Years And Over
)

# --- DOMAIN 2: RACE & ETHNICITY ---
# Denominator: B01001_001 (Total Population)

race_vars <- c(
  "B02001_002",  # White Alone
  "B02001_003",  # Black or African American Alone
  "B02001_004",  # American Indian and Alaska Native Alone
  "B02001_005",  # Asian Alone
  "B03001_003",  # Hispanic or Latino
  "B05001_006"   # Not a U.S. Citizen
)

# --- DOMAIN 3: EDUCATION ---
# Denominator: B07009_001 (Population 25 Years and Over)

education_vars <- c(
  "B07009_002",  # Less Than High School Graduate
  "B07009_003",  # High School Graduate (includes equivalency)
  "B07009_004",  # Some College or Associate's Degree
  "B07009_005",  # Bachelor's Degree
  "B07009_006"   # Graduate or Professional Degree
)

# --- DOMAIN 4: MOBILITY ---
# Denominator: B07204_001 (Population 1 Year and Over)

mobility_vars <- c(
  "B07204_004",  # Different House 1 Year Ago, Same City or Town
  "B07204_007"   # Different House 1 Year Ago, Elsewhere (Different State)
)

# --- DOMAIN 5: TRANSPORTATION & COMMUTE ---
# Denominators: Various (workers 16+, commuters)

transportation_vars <- c(
  "B08014_002",  # No Vehicle Available
  "B08301_010",  # Public Transportation (Excluding Taxicab)
  "B08303_003",  # Travel Time: 5 to 9 Minutes
  "B08303_004",  # Travel Time: 10 to 14 Minutes
  "B08303_005",  # Travel Time: 15 to 19 Minutes
  "B08303_006",  # Travel Time: 20 to 24 Minutes
  "B08303_007",  # Travel Time: 25 to 29 Minutes
  "B08303_008",  # Travel Time: 30 to 34 Minutes
  "B08303_009",  # Travel Time: 35 to 39 Minutes
  "B08303_010",  # Travel Time: 40 to 44 Minutes
  "B08303_011",  # Travel Time: 45 to 59 Minutes
  "B08303_012",  # Travel Time: 60 to 89 Minutes
  "B08303_013"   # Travel Time: 90 or More Minutes
)

# --- DOMAIN 6: HOUSING STRUCTURE ---
# Denominator: B25002_001 (Total Housing Units)

housing_vars <- c(
  "B25002_003",  # Vacant Housing Units
  "B25003_003",  # Renter Occupied Housing Units
  "B25024_002",  # 1-Unit Detached Structures
  "B25024_003",  # 1-Unit Attached Structures
  "B25024_004",  # 2 Units in Structure
  "B25024_005",  # 3 or 4 Units in Structure
  "B25024_006",  # 5 to 9 Units in Structure
  "B25024_007",  # 10 to 19 Units in Structure
  "B25024_008",  # 20 to 49 Units in Structure
  "B25024_009",  # 50 or More Units in Structure
  "B25024_010",  # Mobile Home
  # Housing Vintage (note: buckets shift over time)
  "B25034_002",  # Built 2020 or Later
  "B25034_003",  # Built 2010 to 2019
  "B25034_010"   # Built 1939 or Earlier
)

# --- DOMAIN 7: HOUSING VALUE & WEALTH ---
# No denominator (median/index values)

wealth_vars <- c(
  "B19083_001",  # Gini Index of Income Inequality
  "B25064_001",  # Median Gross Rent
  "B25076_001",  # Lower Value Quartile (Owner-Occupied)
  "B25077_001",  # Median Value (Owner-Occupied)
  "B25078_001"   # Upper Value Quartile (Owner-Occupied)
)

# --- DOMAIN 8: INCOME DISTRIBUTION ---
# Denominator: B19001_001 (Total Households)

income_vars <- c(
  "B19001_002",  # Less than $10,000
  "B19001_003",  # $10,000 to $14,999
  "B19001_004",  # $15,000 to $19,999
  "B19001_005",  # $20,000 to $24,999
  "B19001_006",  # $25,000 to $29,999
  "B19001_007",  # $30,000 to $34,999
  "B19001_008",  # $35,000 to $39,999
  "B19001_009",  # $40,000 to $44,999
  "B19001_010",  # $45,000 to $49,999
  "B19001_011",  # $50,000 to $59,999
  "B19001_012",  # $60,000 to $74,999
  "B19001_013",  # $75,000 to $99,999
  "B19001_014",  # $100,000 to $124,999
  "B19001_015",  # $125,000 to $149,999
  "B19001_016",  # $150,000 to $199,999
  "B19001_017",  # $200,000 or More
  "B19058_002",  # With Cash Public Assistance or Food Stamps/SNAP
  "B19059_002"   # With Retirement Income
)

# --- DOMAIN 9: INDUSTRY & OCCUPATION ---
# Denominator: C24050_001 (Civilian Employed Population 16 Years and Over)

industry_vars <- c(
  "C24050_002",  # Agriculture, Forestry, Fishing and Hunting, and Mining
  "C24050_003",  # Construction
  "C24050_004",  # Manufacturing
  "C24050_005",  # Wholesale Trade
  "C24050_006",  # Retail Trade
  "C24050_007",  # Transportation and Warehousing, and Utilities
  "C24050_008",  # Information
  "C24050_009",  # Finance and Insurance, and Real Estate and Rental and Leasing
  "C24050_010",  # Professional, Scientific, and Management, and Administrative and Waste Management Services
  "C24050_011",  # Educational Services, and Health Care and Social Assistance
  "C24050_012",  # Arts, Entertainment, and Recreation, and Accommodation and Food Services
  "C24050_013",  # Other Services, Except Public Administration
  "C24050_014",  # Public Administration
  "C24050_015",  # Management, Business, Science, and Arts Occupations
  "C24050_029",  # Service Occupations
  "C24050_043",  # Sales and Office Occupations
  "C24050_057",  # Natural Resources, Construction, and Maintenance Occupations
  "C24050_071"   # Production, Transportation, and Material Moving Occupations
)

# --- DOMAIN 10: LANGUAGE ---
# Denominator: B16001_001 (Population 5 Years and Over)

language_vars <- c(
  "B16001_002",  # Speak Only English
  "B16001_003",  # Spanish
  "B16001_005"   # Spanish - Speak English Less Than "Very Well"
)

# --- DOMAIN 11: FAMILY STRUCTURE ---
# Various denominators

family_vars <- c(
  "B09005_005",  # Own Children Under 18 Years in Female Householder, No Spouse/Partner Present

  "B11001_003",  # Married-Couple Family Household
  "B11009_003",  # Male Householder and Male Partner
  "B11009_005"   # Female Householder and Female Partner
)

# --- DENOMINATORS (Required for calculations) ---

denominator_vars <- c(
  "B01001_001",  # Total Population
  "B01003_001",  # Total Population (for density)
  "B07009_001",  # Population 25 Years and Over (Education)
  "B07204_001",  # Population 1 Year and Over (Mobility)
  "B08014_001",  # Workers 16 Years and Over (Vehicles)
  "B08301_001",  # Workers 16 Years and Over (Transportation)
  "B08303_001",  # Workers 16 Years and Over Who Did Not Work From Home (Commute)
  "B16001_001",  # Population 5 Years and Over (Language)
  "B19001_001",  # Total Households (Income)
  "B25002_001",  # Total Housing Units
  "B25003_001",  # Occupied Housing Units (Tenure)
  "C24050_001",  # Civilian Employed Population 16 Years and Over (Industry)
  "B09005_001",  # Own Children Under 18 Years (Family)
  "B11001_001",  # Total Households (Family structure)
  "B11009_001",  # Coupled Households (Same-sex)
  "B26001_001"   # Group Quarters Population
)

# --- MODERN ADDITIONS (Digital Divide - Internet/Computer Access) ---
# These capture the contemporary digital divide, a key socioeconomic indicator

internet_vars <- c(
  "B28002_002",  # With an Internet Subscription
  "B28002_004",  # With a Broadband Internet Subscription
  "B28002_013",  # Without an Internet Subscription
  "B28001_002",  # Has One or More Types of Computing Devices
  "B28001_011"   # No Computer
)

internet_denom <- c(
  "B28002_001",  # Total Households (Internet)
  "B28001_001"   # Total Households (Computer)
)

# Combine all variables
all_vars <- unique(c(
  age_vars,
  race_vars,
  education_vars,
  mobility_vars,
  transportation_vars,
  housing_vars,
  wealth_vars,
  income_vars,
  industry_vars,
  language_vars,
  family_vars,
  denominator_vars,
  internet_vars,
  internet_denom
))

cat("Total variables to fetch:", length(all_vars), "\n")

# =========================================================================
# 3. VARIABLE METADATA (Human-readable names and domains)
# =========================================================================

var_metadata <- tribble(
  ~variable, ~description, ~domain, ~denominator,
  # Age - Male
  "B01001_003", "Pct Male Under 5 Years", "Demography", "B01001_001",
  "B01001_004", "Pct Male 5 To 9 Years", "Demography", "B01001_001",
  "B01001_005", "Pct Male 10 To 14 Years", "Demography", "B01001_001",
  "B01001_006", "Pct Male 15 To 17 Years", "Demography", "B01001_001",
  "B01001_007", "Pct Male 18 And 19 Years", "Demography", "B01001_001",
  "B01001_008", "Pct Male 20 Years", "Demography", "B01001_001",
  "B01001_009", "Pct Male 21 Years", "Demography", "B01001_001",
  "B01001_010", "Pct Male 22 To 24 Years", "Demography", "B01001_001",
  "B01001_011", "Pct Male 25 To 29 Years", "Demography", "B01001_001",
  "B01001_012", "Pct Male 30 To 34 Years", "Demography", "B01001_001",
  "B01001_013", "Pct Male 35 To 39 Years", "Demography", "B01001_001",
  "B01001_014", "Pct Male 40 To 44 Years", "Demography", "B01001_001",
  "B01001_015", "Pct Male 45 To 49 Years", "Demography", "B01001_001",
  "B01001_016", "Pct Male 50 To 54 Years", "Demography", "B01001_001",
  "B01001_017", "Pct Male 55 To 59 Years", "Demography", "B01001_001",
  "B01001_018", "Pct Male 60 And 61 Years", "Demography", "B01001_001",
  "B01001_019", "Pct Male 62 To 64 Years", "Demography", "B01001_001",
  "B01001_020", "Pct Male 65 And 66 Years", "Demography", "B01001_001",
  "B01001_021", "Pct Male 67 To 69 Years", "Demography", "B01001_001",
  "B01001_022", "Pct Male 70 To 74 Years", "Demography", "B01001_001",
  "B01001_023", "Pct Male 75 To 79 Years", "Demography", "B01001_001",
  "B01001_024", "Pct Male 80 To 84 Years", "Demography", "B01001_001",
  "B01001_025", "Pct Male 85 Years And Over", "Demography", "B01001_001",
  # Age - Female
  "B01001_027", "Pct Female Under 5 Years", "Demography", "B01001_001",
  "B01001_028", "Pct Female 5 To 9 Years", "Demography", "B01001_001",
  "B01001_029", "Pct Female 10 To 14 Years", "Demography", "B01001_001",
  "B01001_030", "Pct Female 15 To 17 Years", "Demography", "B01001_001",
  "B01001_031", "Pct Female 18 And 19 Years", "Demography", "B01001_001",
  "B01001_032", "Pct Female 20 Years", "Demography", "B01001_001",
  "B01001_033", "Pct Female 21 Years", "Demography", "B01001_001",
  "B01001_034", "Pct Female 22 To 24 Years", "Demography", "B01001_001",
  "B01001_035", "Pct Female 25 To 29 Years", "Demography", "B01001_001",
  "B01001_036", "Pct Female 30 To 34 Years", "Demography", "B01001_001",
  "B01001_037", "Pct Female 35 To 39 Years", "Demography", "B01001_001",
  "B01001_038", "Pct Female 40 To 44 Years", "Demography", "B01001_001",
  "B01001_039", "Pct Female 45 To 49 Years", "Demography", "B01001_001",
  "B01001_040", "Pct Female 50 To 54 Years", "Demography", "B01001_001",
  "B01001_041", "Pct Female 55 To 59 Years", "Demography", "B01001_001",
  "B01001_042", "Pct Female 60 And 61 Years", "Demography", "B01001_001",
  "B01001_043", "Pct Female 62 To 64 Years", "Demography", "B01001_001",
  "B01001_044", "Pct Female 65 And 66 Years", "Demography", "B01001_001",
  "B01001_045", "Pct Female 67 To 69 Years", "Demography", "B01001_001",
  "B01001_046", "Pct Female 70 To 74 Years", "Demography", "B01001_001",
  "B01001_047", "Pct Female 75 To 79 Years", "Demography", "B01001_001",
  "B01001_048", "Pct Female 80 To 84 Years", "Demography", "B01001_001",
  "B01001_049", "Pct Female 85 Years And Over", "Demography", "B01001_001",
  # Race
  "B02001_002", "Pct White Alone", "Race", "B01001_001",
  "B02001_003", "Pct Black Alone", "Race", "B01001_001",
  "B02001_004", "Pct American Indian Alone", "Race", "B01001_001",
  "B02001_005", "Pct Asian Alone", "Race", "B01001_001",
  "B03001_003", "Pct Hispanic Latino", "Race", "B01001_001",
  "B05001_006", "Pct Not Citizen", "Race", "B01001_001",
  # Education
  "B07009_002", "Pct Less Than High School", "Education", "B07009_001",
  "B07009_003", "Pct High School Graduate", "Education", "B07009_001",
  "B07009_004", "Pct Some College Associate", "Education", "B07009_001",
  "B07009_005", "Pct Bachelor Degree", "Education", "B07009_001",
  "B07009_006", "Pct Graduate Professional Degree", "Education", "B07009_001",
  # Mobility
  "B07204_004", "Pct Different House Same City", "Mobility", "B07204_001",
  "B07204_007", "Pct Different House Elsewhere", "Mobility", "B07204_001",
  # Transportation
  "B08014_002", "Pct No Car", "Transportation", "B08014_001",
  "B08301_010", "Pct Public Transportation", "Transportation", "B08301_001",
  "B08303_003", "Pct Commute 5-9 Min", "Transportation", "B08303_001",
  "B08303_004", "Pct Commute 10-14 Min", "Transportation", "B08303_001",
  "B08303_005", "Pct Commute 15-19 Min", "Transportation", "B08303_001",
  "B08303_006", "Pct Commute 20-24 Min", "Transportation", "B08303_001",
  "B08303_007", "Pct Commute 25-29 Min", "Transportation", "B08303_001",
  "B08303_008", "Pct Commute 30-34 Min", "Transportation", "B08303_001",
  "B08303_009", "Pct Commute 35-39 Min", "Transportation", "B08303_001",
  "B08303_010", "Pct Commute 40-44 Min", "Transportation", "B08303_001",
  "B08303_011", "Pct Commute 45-59 Min", "Transportation", "B08303_001",
  "B08303_012", "Pct Commute 60-89 Min", "Transportation", "B08303_001",
  "B08303_013", "Pct Commute 90+ Min", "Transportation", "B08303_001",
  # Housing
  "B25002_003", "Pct Vacant", "Housing", "B25002_001",
  "B25003_003", "Pct Renter Occupied", "Housing", "B25003_001",
  "B25024_002", "Pct 1 Unit Detached", "Housing", "B25002_001",
  "B25024_003", "Pct 1 Unit Attached", "Housing", "B25002_001",
  "B25024_004", "Pct 2 Units", "Housing", "B25002_001",
  "B25024_005", "Pct 3-4 Units", "Housing", "B25002_001",
  "B25024_006", "Pct 5-9 Units", "Housing", "B25002_001",
  "B25024_007", "Pct 10-19 Units", "Housing", "B25002_001",
  "B25024_008", "Pct 20-49 Units", "Housing", "B25002_001",
  "B25024_009", "Pct 50+ Units", "Housing", "B25002_001",
  "B25024_010", "Pct Mobile Home", "Housing", "B25002_001",
  "B25034_002", "Pct Built 2020 Or Later", "Housing", "B25002_001",
  "B25034_003", "Pct Built 2010-2019", "Housing", "B25002_001",
  "B25034_010", "Pct Built 1939 Or Earlier", "Housing", "B25002_001",
  # Wealth (no denominators - absolute values)
  "B19083_001", "Gini Index", "Wealth", NA_character_,
  "B25064_001", "Median Gross Rent", "Wealth", NA_character_,
  "B25076_001", "Lower Value Quartile", "Wealth", NA_character_,
  "B25077_001", "Median Home Value", "Wealth", NA_character_,
  "B25078_001", "Upper Value Quartile", "Wealth", NA_character_,
  # Income
  "B19001_002", "Pct Income Under 10k", "Income", "B19001_001",
  "B19001_003", "Pct Income 10k-15k", "Income", "B19001_001",
  "B19001_004", "Pct Income 15k-20k", "Income", "B19001_001",
  "B19001_005", "Pct Income 20k-25k", "Income", "B19001_001",
  "B19001_006", "Pct Income 25k-30k", "Income", "B19001_001",
  "B19001_007", "Pct Income 30k-35k", "Income", "B19001_001",
  "B19001_008", "Pct Income 35k-40k", "Income", "B19001_001",
  "B19001_009", "Pct Income 40k-45k", "Income", "B19001_001",
  "B19001_010", "Pct Income 45k-50k", "Income", "B19001_001",
  "B19001_011", "Pct Income 50k-60k", "Income", "B19001_001",
  "B19001_012", "Pct Income 60k-75k", "Income", "B19001_001",
  "B19001_013", "Pct Income 75k-100k", "Income", "B19001_001",
  "B19001_014", "Pct Income 100k-125k", "Income", "B19001_001",
  "B19001_015", "Pct Income 125k-150k", "Income", "B19001_001",
  "B19001_016", "Pct Income 150k-200k", "Income", "B19001_001",
  "B19001_017", "Pct Income 200k+", "Income", "B19001_001",
  "B19058_002", "Pct With Public Assistance SNAP", "Stability", "B19001_001",
  "B19059_002", "Pct With Retirement Income", "Stability", "B19001_001",
  # Industry
  "C24050_002", "Pct Agriculture Mining", "Industry", "C24050_001",
  "C24050_003", "Pct Construction", "Industry", "C24050_001",
  "C24050_004", "Pct Manufacturing", "Industry", "C24050_001",
  "C24050_005", "Pct Wholesale Trade", "Industry", "C24050_001",
  "C24050_006", "Pct Retail Trade", "Industry", "C24050_001",
  "C24050_007", "Pct Transportation Utilities", "Industry", "C24050_001",
  "C24050_008", "Pct Information", "Industry", "C24050_001",
  "C24050_009", "Pct Finance Real Estate", "Industry", "C24050_001",
  "C24050_010", "Pct Professional Scientific Mgmt", "Industry", "C24050_001",
  "C24050_011", "Pct Education Health Social", "Industry", "C24050_001",
  "C24050_012", "Pct Arts Entertainment Food", "Industry", "C24050_001",
  "C24050_013", "Pct Other Services", "Industry", "C24050_001",
  "C24050_014", "Pct Public Administration", "Industry", "C24050_001",
  "C24050_015", "Pct Mgmt Business Science Arts Occupations", "Occupation", "C24050_001",
  "C24050_029", "Pct Service Occupations", "Occupation", "C24050_001",
  "C24050_043", "Pct Sales Office Occupations", "Occupation", "C24050_001",
  "C24050_057", "Pct Natural Resources Construction Maintenance", "Occupation", "C24050_001",
  "C24050_071", "Pct Production Transportation Moving", "Occupation", "C24050_001",
  # Language
  "B16001_002", "Pct Only English", "Language", "B16001_001",
  "B16001_003", "Pct Spanish", "Language", "B16001_001",
  "B16001_005", "Pct Spanish Low English", "Language", "B16001_001",
  # Family Structure
  "B09005_005", "Pct Children Single Female HH", "Family Structure", "B09005_001",
  "B11001_003", "Pct Married Couple Family", "Family Structure", "B11001_001",
  "B11009_003", "Pct Male Male Household", "Family Structure", "B11009_001",
  "B11009_005", "Pct Female Female Household", "Family Structure", "B11009_001",
  # Density & Group Quarters (special - calculated differently)
  "B01003_001", "Population Density", "Demography", NA_character_,
  "B26001_001", "Pct Group Quarters", "Demography", "B01001_001",
  # Internet/Computer (Modern Additions)
  "B28002_002", "Pct With Internet Subscription", "Digital Access", "B28002_001",
  "B28002_004", "Pct With Broadband", "Digital Access", "B28002_001",
  "B28002_013", "Pct Without Internet", "Digital Access", "B28002_001",
  "B28001_002", "Pct Has Computing Device", "Digital Access", "B28001_001",
  "B28001_011", "Pct No Computer", "Digital Access", "B28001_001"
)

# =========================================================================
# 4. DATA DOWNLOAD
# =========================================================================

cat("\n=== Downloading ACS Data ===\n")
cat("This may take several minutes for full US tract-level data...\n")

# Function to download data for a single state with retry logic
download_state_data <- function(state_fips, vars, year) {
  tryCatch({
    get_acs(
      geography = "tract",
      variables = vars,
      state = state_fips,
      year = year,
      output = "wide",
      survey = "acs5",
      geometry = TRUE
    )
  }, error = function(e) {
    message(paste("Error downloading state", state_fips, ":", e$message))
    return(NULL)
  })
}

# Get list of states (including DC and PR)
states <- unique(fips_codes$state_code)
states <- states[!is.na(states) & states != ""]
# Remove territories that may not have tract-level data
states <- states[!states %in% c("AS", "GU", "MP", "VI")]

# Download data state by state with progress
cat("Downloading data for", length(states), "states/territories...\n")

acs_data_list <- list()
pb <- progress_bar$new(
  format = "  [:bar] :current/:total states (:percent) - ETA: :eta",
  total = length(states),
  clear = FALSE
)

for (state in states) {
  pb$tick()
  state_data <- download_state_data(state, all_vars, TARGET_YEAR)
  if (!is.null(state_data)) {
    acs_data_list[[state]] <- state_data
  }
  Sys.sleep(0.5)  # Rate limiting
}

# Combine all states
cat("\nCombining data from all states...\n")
acs_data <- bind_rows(acs_data_list)

cat("Downloaded", nrow(acs_data), "census tracts\n")

# =========================================================================
# 5. DATA PROCESSING & NORMALIZATION
# =========================================================================

cat("\n=== Processing Data ===\n")

# Calculate land area for density
cat("Calculating land area and population density...\n")
acs_data <- acs_data %>%
  mutate(
    area_sq_miles = as.numeric(set_units(st_area(geometry), mi^2)),
    # Avoid division by zero
    area_sq_miles = ifelse(area_sq_miles == 0, NA, area_sq_miles)
  )

# Create processed dataframe with percentage calculations
cat("Calculating percentages and ratios...\n")

# Function to safely calculate percentage
safe_pct <- function(num, denom) {
  result <- ifelse(denom > 0, (num / denom) * 100, NA)
  return(result)
}

# Build the processed dataset
df_processed <- acs_data %>%
  st_drop_geometry() %>%  # Drop geometry for processing
  transmute(
    GEOID = GEOID,
    NAME = NAME,

    # --- POPULATION DENSITY ---
    pop_density = B01003_001E / area_sq_miles,

    # --- AGE STRUCTURE (Male) ---
    pct_male_under_5 = safe_pct(B01001_003E, B01001_001E),
    pct_male_5_9 = safe_pct(B01001_004E, B01001_001E),
    pct_male_10_14 = safe_pct(B01001_005E, B01001_001E),
    pct_male_15_17 = safe_pct(B01001_006E, B01001_001E),
    pct_male_18_19 = safe_pct(B01001_007E, B01001_001E),
    pct_male_20 = safe_pct(B01001_008E, B01001_001E),
    pct_male_21 = safe_pct(B01001_009E, B01001_001E),
    pct_male_22_24 = safe_pct(B01001_010E, B01001_001E),
    pct_male_25_29 = safe_pct(B01001_011E, B01001_001E),
    pct_male_30_34 = safe_pct(B01001_012E, B01001_001E),
    pct_male_35_39 = safe_pct(B01001_013E, B01001_001E),
    pct_male_40_44 = safe_pct(B01001_014E, B01001_001E),
    pct_male_45_49 = safe_pct(B01001_015E, B01001_001E),
    pct_male_50_54 = safe_pct(B01001_016E, B01001_001E),
    pct_male_55_59 = safe_pct(B01001_017E, B01001_001E),
    pct_male_60_61 = safe_pct(B01001_018E, B01001_001E),
    pct_male_62_64 = safe_pct(B01001_019E, B01001_001E),
    pct_male_65_66 = safe_pct(B01001_020E, B01001_001E),
    pct_male_67_69 = safe_pct(B01001_021E, B01001_001E),
    pct_male_70_74 = safe_pct(B01001_022E, B01001_001E),
    pct_male_75_79 = safe_pct(B01001_023E, B01001_001E),
    pct_male_80_84 = safe_pct(B01001_024E, B01001_001E),
    pct_male_85_plus = safe_pct(B01001_025E, B01001_001E),

    # --- AGE STRUCTURE (Female) ---
    pct_female_under_5 = safe_pct(B01001_027E, B01001_001E),
    pct_female_5_9 = safe_pct(B01001_028E, B01001_001E),
    pct_female_10_14 = safe_pct(B01001_029E, B01001_001E),
    pct_female_15_17 = safe_pct(B01001_030E, B01001_001E),
    pct_female_18_19 = safe_pct(B01001_031E, B01001_001E),
    pct_female_20 = safe_pct(B01001_032E, B01001_001E),
    pct_female_21 = safe_pct(B01001_033E, B01001_001E),
    pct_female_22_24 = safe_pct(B01001_034E, B01001_001E),
    pct_female_25_29 = safe_pct(B01001_035E, B01001_001E),
    pct_female_30_34 = safe_pct(B01001_036E, B01001_001E),
    pct_female_35_39 = safe_pct(B01001_037E, B01001_001E),
    pct_female_40_44 = safe_pct(B01001_038E, B01001_001E),
    pct_female_45_49 = safe_pct(B01001_039E, B01001_001E),
    pct_female_50_54 = safe_pct(B01001_040E, B01001_001E),
    pct_female_55_59 = safe_pct(B01001_041E, B01001_001E),
    pct_female_60_61 = safe_pct(B01001_042E, B01001_001E),
    pct_female_62_64 = safe_pct(B01001_043E, B01001_001E),
    pct_female_65_66 = safe_pct(B01001_044E, B01001_001E),
    pct_female_67_69 = safe_pct(B01001_045E, B01001_001E),
    pct_female_70_74 = safe_pct(B01001_046E, B01001_001E),
    pct_female_75_79 = safe_pct(B01001_047E, B01001_001E),
    pct_female_80_84 = safe_pct(B01001_048E, B01001_001E),
    pct_female_85_plus = safe_pct(B01001_049E, B01001_001E),

    # --- RACE & ETHNICITY ---
    pct_white = safe_pct(B02001_002E, B01001_001E),
    pct_black = safe_pct(B02001_003E, B01001_001E),
    pct_american_indian = safe_pct(B02001_004E, B01001_001E),
    pct_asian = safe_pct(B02001_005E, B01001_001E),
    pct_hispanic = safe_pct(B03001_003E, B01001_001E),
    pct_not_citizen = safe_pct(B05001_006E, B01001_001E),

    # --- EDUCATION ---
    pct_less_than_hs = safe_pct(B07009_002E, B07009_001E),
    pct_hs_graduate = safe_pct(B07009_003E, B07009_001E),
    pct_some_college = safe_pct(B07009_004E, B07009_001E),
    pct_bachelor = safe_pct(B07009_005E, B07009_001E),
    pct_graduate_prof = safe_pct(B07009_006E, B07009_001E),

    # --- MOBILITY ---
    pct_diff_house_same_city = safe_pct(B07204_004E, B07204_001E),
    pct_diff_house_elsewhere = safe_pct(B07204_007E, B07204_001E),

    # --- TRANSPORTATION ---
    pct_no_car = safe_pct(B08014_002E, B08014_001E),
    pct_public_transit = safe_pct(B08301_010E, B08301_001E),
    pct_commute_5_9 = safe_pct(B08303_003E, B08303_001E),
    pct_commute_10_14 = safe_pct(B08303_004E, B08303_001E),
    pct_commute_15_19 = safe_pct(B08303_005E, B08303_001E),
    pct_commute_20_24 = safe_pct(B08303_006E, B08303_001E),
    pct_commute_25_29 = safe_pct(B08303_007E, B08303_001E),
    pct_commute_30_34 = safe_pct(B08303_008E, B08303_001E),
    pct_commute_35_39 = safe_pct(B08303_009E, B08303_001E),
    pct_commute_40_44 = safe_pct(B08303_010E, B08303_001E),
    pct_commute_45_59 = safe_pct(B08303_011E, B08303_001E),
    pct_commute_60_89 = safe_pct(B08303_012E, B08303_001E),
    pct_commute_90_plus = safe_pct(B08303_013E, B08303_001E),

    # --- HOUSING ---
    pct_vacant = safe_pct(B25002_003E, B25002_001E),
    pct_renter = safe_pct(B25003_003E, B25003_001E),
    pct_1unit_detached = safe_pct(B25024_002E, B25002_001E),
    pct_1unit_attached = safe_pct(B25024_003E, B25002_001E),
    pct_2units = safe_pct(B25024_004E, B25002_001E),
    pct_3_4units = safe_pct(B25024_005E, B25002_001E),
    pct_5_9units = safe_pct(B25024_006E, B25002_001E),
    pct_10_19units = safe_pct(B25024_007E, B25002_001E),
    pct_20_49units = safe_pct(B25024_008E, B25002_001E),
    pct_50plus_units = safe_pct(B25024_009E, B25002_001E),
    pct_mobile_home = safe_pct(B25024_010E, B25002_001E),
    pct_built_2020_later = safe_pct(B25034_002E, B25002_001E),
    pct_built_2010_2019 = safe_pct(B25034_003E, B25002_001E),
    pct_built_1939_earlier = safe_pct(B25034_010E, B25002_001E),

    # --- WEALTH (No normalization - absolute values) ---
    gini_index = B19083_001E,
    median_gross_rent = B25064_001E,
    lower_quartile_value = B25076_001E,
    median_home_value = B25077_001E,
    upper_quartile_value = B25078_001E,

    # --- INCOME ---
    pct_income_under_10k = safe_pct(B19001_002E, B19001_001E),
    pct_income_10k_15k = safe_pct(B19001_003E, B19001_001E),
    pct_income_15k_20k = safe_pct(B19001_004E, B19001_001E),
    pct_income_20k_25k = safe_pct(B19001_005E, B19001_001E),
    pct_income_25k_30k = safe_pct(B19001_006E, B19001_001E),
    pct_income_30k_35k = safe_pct(B19001_007E, B19001_001E),
    pct_income_35k_40k = safe_pct(B19001_008E, B19001_001E),
    pct_income_40k_45k = safe_pct(B19001_009E, B19001_001E),
    pct_income_45k_50k = safe_pct(B19001_010E, B19001_001E),
    pct_income_50k_60k = safe_pct(B19001_011E, B19001_001E),
    pct_income_60k_75k = safe_pct(B19001_012E, B19001_001E),
    pct_income_75k_100k = safe_pct(B19001_013E, B19001_001E),
    pct_income_100k_125k = safe_pct(B19001_014E, B19001_001E),
    pct_income_125k_150k = safe_pct(B19001_015E, B19001_001E),
    pct_income_150k_200k = safe_pct(B19001_016E, B19001_001E),
    pct_income_200k_plus = safe_pct(B19001_017E, B19001_001E),
    pct_public_assistance = safe_pct(B19058_002E, B19001_001E),
    pct_retirement_income = safe_pct(B19059_002E, B19001_001E),

    # --- INDUSTRY ---
    pct_agriculture_mining = safe_pct(C24050_002E, C24050_001E),
    pct_construction = safe_pct(C24050_003E, C24050_001E),
    pct_manufacturing = safe_pct(C24050_004E, C24050_001E),
    pct_wholesale = safe_pct(C24050_005E, C24050_001E),
    pct_retail = safe_pct(C24050_006E, C24050_001E),
    pct_transport_utilities = safe_pct(C24050_007E, C24050_001E),
    pct_information = safe_pct(C24050_008E, C24050_001E),
    pct_finance_real_estate = safe_pct(C24050_009E, C24050_001E),
    pct_professional_scientific = safe_pct(C24050_010E, C24050_001E),
    pct_education_health = safe_pct(C24050_011E, C24050_001E),
    pct_arts_entertainment_food = safe_pct(C24050_012E, C24050_001E),
    pct_other_services = safe_pct(C24050_013E, C24050_001E),
    pct_public_admin = safe_pct(C24050_014E, C24050_001E),

    # --- OCCUPATION ---
    pct_mgmt_business_science = safe_pct(C24050_015E, C24050_001E),
    pct_service_occupations = safe_pct(C24050_029E, C24050_001E),
    pct_sales_office = safe_pct(C24050_043E, C24050_001E),
    pct_construction_maintenance = safe_pct(C24050_057E, C24050_001E),
    pct_production_transport = safe_pct(C24050_071E, C24050_001E),

    # --- LANGUAGE ---
    pct_english_only = safe_pct(B16001_002E, B16001_001E),
    pct_spanish = safe_pct(B16001_003E, B16001_001E),
    pct_spanish_low_english = safe_pct(B16001_005E, B16001_001E),

    # --- FAMILY STRUCTURE ---
    pct_children_single_female = safe_pct(B09005_005E, B09005_001E),
    pct_married_couple = safe_pct(B11001_003E, B11001_001E),
    pct_male_male_hh = safe_pct(B11009_003E, B11009_001E),
    pct_female_female_hh = safe_pct(B11009_005E, B11009_001E),

    # --- GROUP QUARTERS ---
    pct_group_quarters = safe_pct(B26001_001E, B01001_001E),

    # --- DIGITAL ACCESS (Modern Additions) ---
    pct_internet = safe_pct(B28002_002E, B28002_001E),
    pct_broadband = safe_pct(B28002_004E, B28002_001E),
    pct_no_internet = safe_pct(B28002_013E, B28002_001E),
    pct_has_computer = safe_pct(B28001_002E, B28001_001E),
    pct_no_computer = safe_pct(B28001_011E, B28001_001E)
  )

# Get the list of analysis variables (exclude GEOID and NAME)
analysis_vars <- names(df_processed)[!names(df_processed) %in% c("GEOID", "NAME")]
cat("Total analysis variables:", length(analysis_vars), "\n")

# =========================================================================
# 6. SPLIT COMPLETE AND INCOMPLETE CASES
# =========================================================================

cat("\n=== Handling Complete/Incomplete Cases ===\n")

# Identify complete cases
df_complete <- df_processed[complete.cases(df_processed[, analysis_vars]), ]
df_incomplete <- df_processed[!complete.cases(df_processed[, analysis_vars]), ]

cat("Complete cases:", nrow(df_complete), "\n")
cat("Incomplete cases:", nrow(df_incomplete), "\n")

# =========================================================================
# 7. STANDARDIZATION (0-1 Range)
# =========================================================================

cat("\n=== Standardizing Data ===\n")

# Standardize to 0-1 range (as per original methodology)
range01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))  # Handle constant columns
  (x - rng[1]) / (rng[2] - rng[1])
}

# Apply standardization to complete cases only (for clustering)
data_for_clustering <- df_complete %>%
  select(all_of(analysis_vars)) %>%
  mutate(across(everything(), range01))

# Store min/max for later use with incomplete cases
scaling_params <- df_complete %>%
  select(all_of(analysis_vars)) %>%
  summarise(across(everything(), list(min = ~min(., na.rm = TRUE),
                                       max = ~max(., na.rm = TRUE))))

cat("Data standardized to 0-1 range\n")

# =========================================================================
# 8. K-MEANS CLUSTERING (250 Initial Clusters)
# =========================================================================

cat("\n=== Running K-Means Clustering ===\n")
cat("Creating", N_CLUSTERS, "clusters with", N_RUNS, "runs...\n")
cat("(Original paper used 100,000 runs with 1,000,000 max iterations each)\n")

# Convert to matrix for faster clustering
data_matrix <- as.matrix(data_for_clustering)

# Run k-means multiple times and keep the best solution
best_fit <- Inf
best_clusters <- NULL

pb <- progress_bar$new(
  format = "  K-means [:bar] :current/:total (:percent) - Best WSS: :best",
  total = N_RUNS,
  clear = FALSE
)

for (i in 1:N_RUNS) {
  current_clusters <- kmeans(
    x = data_matrix,
    centers = N_CLUSTERS,
    iter.max = MAX_ITER,
    nstart = 1,
    algorithm = "Hartigan-Wong"
  )

  if (current_clusters$tot.withinss < best_fit) {
    best_fit <- current_clusters$tot.withinss
    best_clusters <- current_clusters
  }

  pb$tick(tokens = list(best = format(round(best_fit, 2), big.mark = ",")))
}

cat("\nBest total within-cluster sum of squares:", round(best_fit, 2), "\n")

# =========================================================================
# 9. ASSIGN INCOMPLETE CASES TO NEAREST CLUSTER
# =========================================================================

cat("\n=== Assigning Incomplete Cases ===\n")

if (nrow(df_incomplete) > 0) {
  # For incomplete cases, standardize available values and find nearest centroid
  incomplete_assignments <- integer(nrow(df_incomplete))

  for (row in 1:nrow(df_incomplete)) {
    # Get available values for this observation
    obs <- df_incomplete[row, analysis_vars]
    available_vars <- names(obs)[!is.na(obs)]

    if (length(available_vars) > 0) {
      # Standardize using same parameters as complete cases
      obs_scaled <- sapply(available_vars, function(v) {
        min_val <- scaling_params[[paste0(v, "_min")]]
        max_val <- scaling_params[[paste0(v, "_max")]]
        if (max_val == min_val) return(0.5)
        (obs[[v]] - min_val) / (max_val - min_val)
      })

      # Find distances to all cluster centroids (using available variables only)
      centroid_cols <- match(available_vars, colnames(best_clusters$centers))
      centroid_subset <- best_clusters$centers[, centroid_cols, drop = FALSE]

      distances <- apply(centroid_subset, 1, function(ctr) {
        sqrt(sum((obs_scaled - ctr)^2))
      })

      incomplete_assignments[row] <- which.min(distances)
    } else {
      incomplete_assignments[row] <- NA
    }
  }

  cat("Assigned", sum(!is.na(incomplete_assignments)), "incomplete cases to clusters\n")
}

# =========================================================================
# 10. HIERARCHICAL CLUSTERING (Ward's Method)
# =========================================================================

cat("\n=== Applying Hierarchical Clustering (Ward's Method) ===\n")

# Calculate distance matrix between cluster centroids
centroid_dist <- dist(best_clusters$centers)

# Apply Ward's hierarchical clustering
wards_hclust <- hclust(centroid_dist, method = "ward.D2")

# Cut at multiple levels (as per original: 2, 10, 31, 55 classes)
ward_cuts <- data.frame(
  cluster = 1:N_CLUSTERS,
  X2 = cutree(wards_hclust, k = 2),
  X10 = cutree(wards_hclust, k = 10),
  X31 = cutree(wards_hclust, k = 31),
  X55 = cutree(wards_hclust, k = 55)
)

cat("Created hierarchical classification:\n")
cat("  - X2: 2 supergroups\n")
cat("  - X10: 10 groups (primary classification)\n")
cat("  - X31: 31 subgroups\n")
cat("  - X55: 55 types\n")
cat("  - cluster: 250 microclusters\n")

# =========================================================================
# 11. SILHOUETTE ANALYSIS
# =========================================================================

cat("\n=== Computing Silhouette Scores ===\n")

silhouette_scores <- data.frame(k = 2:100, silhouette = NA)

for (k in 2:100) {
  cuts <- cutree(wards_hclust, k = k)
  sil <- silhouette(cuts, centroid_dist)
  silhouette_scores$silhouette[k-1] <- mean(sil[, 3])
}

# Find optimal k values
top_k <- silhouette_scores %>%
  arrange(desc(silhouette)) %>%
  head(10)

cat("Top silhouette scores:\n")
print(top_k)

# =========================================================================
# 12. BUILD FINAL DATASET
# =========================================================================

cat("\n=== Building Final Dataset ===\n")

# Add cluster assignments to complete cases
df_complete$cluster <- best_clusters$cluster

# Add cluster assignments to incomplete cases
if (nrow(df_incomplete) > 0) {
  df_incomplete$cluster <- incomplete_assignments
}

# Combine
df_final <- bind_rows(df_complete, df_incomplete)

# Merge with ward cuts to get hierarchical classification
df_final <- df_final %>%
  left_join(ward_cuts, by = "cluster")

# Convert to factors with labels for X10
x10_labels <- c(
  "A: Hispanic and Kids",
  "B: Wealthy Nuclear Families",
  "C: Middle Income, Single Family Homes",
  "D: Native American",
  "E: Wealthy Urbanites",
  "F: Low Income and Diverse",
  "G: Old, Wealthy White",
  "H: Low Income Minority Mix",
  "I: Poor, African-American",
  "J: Residential Institutions, Young People"
)

df_final <- df_final %>%
  mutate(
    X2 = factor(X2),
    X10_code = X10,
    X10 = factor(X10, levels = 1:10, labels = LETTERS[1:10]),
    X31 = factor(X31),
    X55 = factor(X55),
    cluster = factor(cluster)
  )

# =========================================================================
# 13. CALCULATE INDEX SCORES
# =========================================================================

cat("\n=== Calculating Index Scores ===\n")

# Index score = (value / national mean) * 100
# Score of 100 = national average
# Score > 100 = overrepresentation
# Score < 100 = underrepresentation

df_index_scores <- df_final %>%
  select(GEOID, NAME, all_of(analysis_vars)) %>%
  mutate(across(
    all_of(analysis_vars),
    ~(. / mean(., na.rm = TRUE)) * 100,
    .names = "{.col}_index"
  ))

# =========================================================================
# 14. CREATE CLUSTER PROFILES
# =========================================================================

cat("\n=== Creating Cluster Profiles ===\n")

# Summarize each X10 group
cluster_profiles <- df_final %>%
  group_by(X10) %>%
  summarise(
    n_tracts = n(),
    across(all_of(analysis_vars), ~mean(., na.rm = TRUE), .names = "mean_{.col}")
  )

# Create index-based profiles
cluster_index_profiles <- df_final %>%
  group_by(X10) %>%
  summarise(
    n_tracts = n(),
    across(
      all_of(analysis_vars),
      ~mean(., na.rm = TRUE) / mean(df_final[[cur_column()]], na.rm = TRUE) * 100,
      .names = "index_{.col}"
    )
  )

cat("Created profiles for", nrow(cluster_profiles), "groups\n")

# =========================================================================
# 15. EXPORT DATA
# =========================================================================

cat("\n=== Exporting Data ===\n")

# Create output directory
output_dir <- here::here("output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Export main dataset
output_file <- file.path(output_dir, paste0("acs_", TARGET_YEAR, "_demographic_clusters.csv"))
write_csv(df_final, output_file)
cat("Saved:", output_file, "\n")

# Export cluster profiles
profiles_file <- file.path(output_dir, paste0("cluster_profiles_", TARGET_YEAR, ".csv"))
write_csv(cluster_profiles, profiles_file)
cat("Saved:", profiles_file, "\n")

# Export index-based profiles
index_profiles_file <- file.path(output_dir, paste0("cluster_index_profiles_", TARGET_YEAR, ".csv"))
write_csv(cluster_index_profiles, index_profiles_file)
cat("Saved:", index_profiles_file, "\n")

# Export silhouette scores
silhouette_file <- file.path(output_dir, paste0("silhouette_scores_", TARGET_YEAR, ".csv"))
write_csv(silhouette_scores, silhouette_file)
cat("Saved:", silhouette_file, "\n")

# Export variable metadata
metadata_file <- file.path(output_dir, "variable_metadata.csv")
write_csv(var_metadata, metadata_file)
cat("Saved:", metadata_file, "\n")

# Save R objects for later analysis
rdata_file <- file.path(output_dir, paste0("clustering_results_", TARGET_YEAR, ".RData"))
save(
  df_final,
  best_clusters,
  wards_hclust,
  ward_cuts,
  cluster_profiles,
  cluster_index_profiles,
  silhouette_scores,
  var_metadata,
  scaling_params,
  file = rdata_file
)
cat("Saved:", rdata_file, "\n")

# =========================================================================
# 16. OPTIONAL: CREATE SPATIAL OUTPUT (Shapefile/GeoPackage)
# =========================================================================

cat("\n=== Creating Spatial Output ===\n")

# Rejoin with geometry
df_spatial <- acs_data %>%
  select(GEOID, geometry) %>%
  left_join(df_final, by = "GEOID")

# Export as GeoPackage (more modern than Shapefile)
gpkg_file <- file.path(output_dir, paste0("us_tract_clusters_", TARGET_YEAR, ".gpkg"))
st_write(df_spatial, gpkg_file, driver = "GPKG", delete_dsn = TRUE)
cat("Saved:", gpkg_file, "\n")

# =========================================================================
# 17. SUMMARY STATISTICS
# =========================================================================

cat("\n")
cat("=" , rep("=", 60), "=\n", sep = "")
cat("                    SUMMARY                    \n")
cat("=" , rep("=", 60), "=\n", sep = "")
cat("\n")
cat("Data Source: ACS 5-Year Estimates", TARGET_YEAR - 4, "-", TARGET_YEAR, "\n")
cat("Total Census Tracts:", nrow(df_final), "\n")
cat("Complete Cases:", nrow(df_complete), "\n")
cat("Incomplete Cases:", nrow(df_incomplete), "\n")
cat("Total Variables:", length(analysis_vars), "\n")
cat("\n")
cat("Classification Hierarchy:\n")
cat("  - 2 Supergroups\n")
cat("  - 10 Groups (Primary)\n")
cat("  - 31 Subgroups\n")
cat("  - 55 Types\n")
cat("  - 250 Microclusters\n")
cat("\n")
cat("X10 Group Distribution:\n")
print(table(df_final$X10))
cat("\n")
cat("Output files saved to:", output_dir, "\n")
cat("\n")
cat("=" , rep("=", 60), "=\n", sep = "")
cat("                    COMPLETE                    \n")
cat("=" , rep("=", 60), "=\n", sep = "")
