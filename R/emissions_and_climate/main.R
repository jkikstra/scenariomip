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
# DOWNLOAD.DATE <- "2025-04-16"
# DOWNLOAD.DATE <- "2025-04-25"
# DOWNLOAD.DATE <- "2025-05-06"
DOWNLOAD.DATE <- "MESSAGE-internal-20250512"

# Other shared parameters
HARMONIZATION.YEAR <- 2023

# load functions
source(here("R", "emissions_and_climate", "emissions_utils.R"))
# load other
source(here("R", "visualisation_choices.R"))

# Load and aggregate scenario data
MESSAGE.INTERNAL.OR.SCEN.EXPL <- "internal" # "scenario_explorer" # for `emissions_load_scenarios.R`
IAM_SCENARIOS_LOCATION <- here("data", "data_vetting", "scens")
# IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-04-16.csv"
IAM_SCENARIOS_FILE <- paste0("scenarios_scenariomip_emissions_",DOWNLOAD.DATE,".csv")

if (file.exists(
  file.path(IAM_SCENARIOS_LOCATION, paste0(substr(IAM_SCENARIOS_FILE, start=1, stop = nchar(IAM_SCENARIOS_FILE)-4),"_harmonizationsectors",".csv"))
)){
  source(here("R", "emissions_and_climate", "emissions_load_scenarios.R"))
  scenarios_harmonization <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, paste0(substr(IAM_SCENARIOS_FILE, start=1, stop = nchar(IAM_SCENARIOS_FILE)-4),"_harmonizationsectors",".csv")),
                                           mode = "fast") %>%
    iamc_wide_to_long()
} else {
  source(here("R", "emissions_and_climate", "emissions_load_scenarios.R"))
  source(here("R", "emissions_and_climate", "emissions_aggregation_rules_scenarios.R"))
  source(here("R", "emissions_and_climate", "emissions_aggregate_scenarios_to_harmonization_sector.R"))
}


# Load historical emissions
source(here("R", "emissions_and_climate", "emissions_load_historical_emissions.R")) # already at harmonization sector detail
# Fix historical emissions data (region names)
source(here("R", "emissions_and_climate", "emissions_fix_historical_emissions_regionnames.R")) # needs to have run `emissions_load_scenarios.R` already

# Vetting: reporting checks
source(here("R", "emissions_and_climate", "emissions_check_aggregation_to_total.R")) # ONLY PARTIALLY IMPLEMENTED
sector.list <- hist.national %>% mutate(variable_copy=variable) %>%  iamc_variable_keep_one_level(-1) %>% distinct(variable_copy,variable) %>% rename(sector=variable) %>% rename(variable=variable_copy)
variable.list <- hist.national %>% distinct(variable)
sector.species.list <- hist.national %>% distinct(variable) %>%
  add_sector_and_species_columns() %>%
  distinct(sector,species)
source(here("R", "emissions_and_climate", "emissions_check_reporting_completeness.R")) # ONLY PARTIALLY IMPLEMENTED

# # Processing: process scenarios into harmonization sectors
# source("tbd.R")

# Vetting: historical flags
source(here("R", "emissions_and_climate", "emissions_vetting_historical_flags.R"))

# Vetting: historical plots
source(here("R", "emissions_and_climate", "emissions_vetting_historical_plots.R"))

# Future - Plotting: climate
CLIMATE.DATA.LOCATION <- here("data", "data_vetting", "climate")
if (DOWNLOAD.DATE=="2025-04-25"){
  AR6.CLIMATE.FILE.JSK <- "scenarios_scenariomip_emissions_global_2025-04-16_alloutput.xlsx"
}
AR6.CLIMATE.FOLDER.ZN <- file.path(CLIMATE.DATA.LOCATION, "0020_20250416-112220_ar6-workflow")
AR6.CLIMATE.FOLDER.ZN.OUTPUT <- file.path(AR6.CLIMATE.FOLDER.ZN, "magicc-ar6")
CMIP7.CLIMATE.FOLDER.ZN <- file.path(CLIMATE.DATA.LOCATION, "0020_20250416-112220_updated-workflow")
CMIP7.CLIMATE.FOLDER.ZN.OUTPUT <- file.path(CMIP7.CLIMATE.FOLDER.ZN, "magicc-v7-6-0a3_magicc-ar7-fast-track-drawnset-v0-3-0")
source(here("R", "emissions_and_climate", "climate_ar6workflow_jsk.R"))
# source(here("R", "emissions_and_climate", "climate_cmip7workflow_zn.R")))

# Future - Plotting: emissions
source(here("R", "emissions_and_climate", "emissions_future_plotting.R"))
