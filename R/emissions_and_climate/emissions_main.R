# Overview of R scripts for emissions vetting
# ... builds on `emissions_vetting_allmodels.qmd`


# shared packages for emissions handling ----
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("patchwork")
library("ggthemes")
library("ggsci")
library("testthat")
# library("geomtextpath")
library("stringr")
library("ggthemes")

here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Folders
output.folder.data <- here("data", "data_vetting", "output_data")


# # Download scenarios
# source(here("download_scenarios.R"))
DOWNLOAD.DATE <- "2025-04-16"

# Other shared parameters
HARMONIZATION.YEAR <- 2023

# load functions
source(here("R", "emissions_and_climate", "emissions_utils.R"))


# Load and aggregate scenario data
MESSAGE.INTERNAL.OR.SCEN.EXPL <- "scenario_explorer" # "internal" # for `emissions_load_scenarios.R`
IAM_SCENARIOS_LOCATION <- here("data", "data_vetting", "scens")
IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-04-16.csv"
if (file.exists(
  file.path(IAM_SCENARIOS_LOCATION, paste0(substr(IAM_SCENARIOS_FILE, start=1, stop = nchar(IAM_SCENARIOS_FILE)-4),"_harmonizationsectors",".csv"))
)){
  source(here("R", "emissions_and_climate", "emissions_load_scenarios.R"))
  scenarios_harmonization <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, paste0(substr(IAM_SCENARIOS_FILE, start=1, stop = nchar(IAM_SCENARIOS_FILE)-4),"_harmonizationsectors",".csv")),
                                           mode = "fast") %>%
    iamc_wide_to_long()
} else {
  source(here("R", "emissions_and_climate", "emissions_load_scenarios.R"))
  source(here("R", "emissions_and_climate", "emissions_aggregate_scenarios_to_harmonization_sector.R"))
}


# Load historical emissions
source(here("R", "emissions_and_climate", "emissions_load_historical_emissions.R")) # already at harmonization sector detail
# Fix historical emissions data (region names)
source(here("R", "emissions_and_climate", "emissions_fix_historical_emissions_regionnames.R")) # needs to have run `emissions_load_scenarios.R` already

# # Vetting: reporting checks
# source(here("R", "emissions_and_climate", "emissions_check_aggregation_to_total.R")) # NOT YET IMPLEMENTED, COULD COME BEFORE emissions_aggregate_scenarios_...?
# source(here("R", "emissions_and_climate", "emissions_check_reporting_completeness.R")) # NOT YET IMPLEMENTED, COULD COME BEFORE emissions_aggregate_scenarios_...?
#
# # Processing: process scenarios into harmonization sectors
# source("tbd.R")

# Vetting: historical flags
source(here("R", "emissions_and_climate", "emissions_vetting_historical_flags.R"))

# Vetting: historical plots
source(here("R", "emissions_and_climate", "emissions_vetting_historical_plots.R"))

# Future - Plotting: climate
source(here("R", "emissions_and_climate", "climate_future_plotting.R"))

# Future - Plotting: emissions
source(here("R", "emissions_and_climate", "emissions_future_plotting.R"))
