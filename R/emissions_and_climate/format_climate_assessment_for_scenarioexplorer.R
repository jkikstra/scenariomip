#' Code for ScenarioMIP data publication of climate assessment data
#' Developed by Jarmo Kikstra


# Load packages ----
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("writexl")

here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Define paths and constants ----

## Constants for format for Scenario Explorer ----
GENERAL_VARIABLE_PREFIX <- "Climate Assessment|"
INFILLING_VARIABLE_PREFIX <- paste0(GENERAL_VARIABLE_PREFIX, "Infilled|")
CLIMATE_VARIABLE_PREFIX <- paste0(GENERAL_VARIABLE_PREFIX)

# MAGICC_VERSION <- 

## Climate Assessment Data ----
MAIN_DATE <- "20260203" # date of climate assessment run
VERSION_RELEASE_SCENARIOMIP <- "v0.1" # version of the data release for ScenarioMIP
MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT <- here("data", "ca", paste0(MAIN_DATE, " (release v0.1)")) # prep for first pre-release (only infilled emissions and climate outcomes)
DATA.EHH.FOLDERS <- c(
  MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT
)
PATH.CLIMATE.DATA <- file.path(DATA.EHH.FOLDERS, "climate-assessment")




# Load data ----

## Climate --------------------------------------------------------------------

### GSAT (assessed) -----------------------------------------------------------
climate.timeseries.gsat <- NULL
for (p in PATH.CLIMATE.DATA){
  dirs = list.dirs(path = p, recursive = FALSE)

  if (length(dirs) == 0) {
    message("No subdirectories found.")
    climate.timeseries.gsat <- climate.timeseries.gsat %>%
      bind_rows(
        load_multiple_files(folder.path = p,
                            iamc = F,
                            pattern = "assessed-warming") %>%
          pivot_longer(cols = `2000`:`2100`,
                       names_to = "year",
                       values_to = "value") %>%
          mutate(year=as.numeric(year)) %>%
          filter(quantile%in%c(0.33,0.5,0.67)) %>%
          mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
          select(-quantile) %>%
          add_scenariomip_info_columns() %>%
          mutate(ca.version = get_date_string_path_climate_data(p))
      )
  } else {
    message("Subdirectories found: ", paste(basename(dirs), collapse = ", "))
    for (d in dirs){
      climate.timeseries.gsat <- climate.timeseries.gsat %>%
        bind_rows(
          load_multiple_files(folder.path = d,
                              iamc = F,
                              pattern = "assessed-warming") %>%
            pivot_longer(cols = `2000`:`2100`,
                         names_to = "year",
                         values_to = "value") %>%
            mutate(year=as.numeric(year)) %>%
            filter(quantile%in%c(0.33,0.5,0.67)) %>%
            mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
            select(-quantile) %>%
            add_scenariomip_info_columns() %>%
            mutate(ca.version = get_date_string_path_climate_data(p))
        )
    }
  }




}

### ERF -----------------------------------------------------------

climate.timeseries.erf <- NULL
for (p in PATH.CLIMATE.DATA){

  dirs = list.dirs(path = p, recursive = FALSE)

  if (length(dirs) == 0) {
    message("No subdirectories found.")
    climate.timeseries.erf <- climate.timeseries.erf %>%
      bind_rows(
        load_multiple_files(folder.path = p,
                            iamc = F,
                            pattern = "erf-timeseries") %>%
          pivot_longer(cols = `2000`:`2100`,
                       names_to = "year",
                       values_to = "value") %>%
          mutate(year=as.numeric(year)) %>%
          filter(quantile%in%c(0.33,0.5,0.67)) %>%
          mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
          select(-quantile) %>%
          add_scenariomip_info_columns() %>%
          mutate(ca.version = get_date_string_path_climate_data(p))
      )
  } else {
    message("Subdirectories found: ", paste(basename(dirs), collapse = ", "))
    for (d in dirs){
      climate.timeseries.erf <- climate.timeseries.erf %>%
        bind_rows(
          load_multiple_files(folder.path = d,
                              iamc = F,
                              pattern = "erf-timeseries") %>%
            pivot_longer(cols = `2000`:`2100`,
                         names_to = "year",
                         values_to = "value") %>%
            mutate(year=as.numeric(year)) %>%
            filter(quantile%in%c(0.33,0.5,0.67)) %>%
            mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
            select(-quantile) %>%
            add_scenariomip_info_columns() %>%
            mutate(ca.version = get_date_string_path_climate_data(p))
        )
    }
  }
}

## Emissions processing ---------------------------------------------------------------

# if climate.timeseries.gsat exists, use the model names from there to load infilled emissions like `climate.timeseries.gsat %>% pull(full.model.name) %>% unique()`, else specify model names in a different way
all.full.model.names <- climate.timeseries.gsat %>% pull(full.model.name) %>% unique()


### Pre-processed (SCM) -----------------------------------------------------
# skipped for now; if this is wanted, see `markers_overview_emissions_scenariomip-description-paper.R` for the code

### Harmonized (SCM) --------------------------------------------------------
# skipped for now; if this is wanted, see `markers_overview_emissions_scenariomip-description-paper.R` for the code

### Infilled (SCM) -----------------------------------------------------
infilled <- tibble()
for (p in PATH.CLIMATE.DATA){
  for (m in all.full.model.names ){
    path <- file.path(p, "..", "emissions", m)
    if (!dir.exists(path)) {
      message("Skipping missing path: ", path)
      next
    }

    df.m <- load_multiple_files(folder.path = path,
                                iamc = T,
                                pattern = "infilled") %>%
      mutate(ca.version = get_date_string_path_climate_data(p))

    infilled <- infilled %>% bind_rows(df.m)
  }
}



# Formatting data ----

## Functions and constants ----
fix_scenario_names <- function(df){
  df %>%
    mutate(new_scenario_name = NA_character_) %>%
    # general renaming
    # tbd...
    # markers
    mutate_cond(model == "AIM 3.0" & scenario == "SSP2 - Low Overshoot_a", new_scenario_name = "Low-to-Negative - SSP2 (marker)") |>
    mutate_cond(model == "REMIND-MAgPIE 3.5-4.11" & scenario == "SSP1 - Very Low Emissions", new_scenario_name = "Very Low - SSP1 (marker)") |>
    mutate_cond(model == "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" & scenario == "SSP2 - Low Emissions", new_scenario_name = "Low - SSP2 (marker)") |>
    mutate_cond(model == "COFFEE 1.6" & scenario == "SSP2 - Medium-Low Emissions", new_scenario_name = "Medium-to-Low - SSP2 (marker)") |>
    mutate_cond(model == "IMAGE 3.4" & scenario == "SSP2 - Medium Emissions", new_scenario_name = "Medium - SSP2 (marker)") |>
    mutate_cond(model == "WITCH 6.0" & scenario == "SSP5 - Medium-Low Emissions_a", new_scenario_name = "High-to-Low - SSP5 (marker)") |>
    mutate_cond(model == "GCAM 8s" & scenario == "SSP3 - High Emissions", new_scenario_name = "High - SSP3 (marker)") %>%
    return()
}
format_emissions_variables <- function(df){
  df |> 
    # add prefix
    mutate(variable = paste0(INFILLING_VARIABLE_PREFIX, variable)) |>
    # align unit with common-definitions
    mutate_cond(unit == "kt HFC4310/yr", unit = "kt HFC4310mee/yr")  %>%
    return()
}
format_climate_variables <- function(df){
  df |> 
    # ensure model is full model name
    mutate(model = full.model.name) |> 
    
    # add prefix
    mutate(variable = paste0(CLIMATE_VARIABLE_PREFIX, variable)) |>
    # merge percentile into variable name (currently working for p33, p50, p67)
    # mutate_cond(percentile == "p33", variable = paste0(variable, "|", "33rd Percentile")) |>
    mutate_cond(percentile %in% c("p33", "p67"), variable = paste0(variable, "|", substr(percentile, 2, nchar(percentile)), "th Percentile")) |>
    mutate_cond(percentile == "p50", variable = paste0(variable, "|", "Median")) |>
    # merge climate model into variable name
    mutate(variable = paste0(variable, " [", climate_model, "]")) %>%
    
    # unit
    mutate_cond(unit == "K", unit = "°C") %>%
    return()
}


## Infilled emissions --------------------------------------------------------
infilled.formatted <- infilled |> 
  format_emissions_variables() |> 
  select(model,scenario,region,variable,unit,year,value,
    ca.version)


## Climate variables --------------------------------------------------------
climate.formatted <- climate.timeseries.gsat |> 
  bind_rows(climate.timeseries.erf) |> 
  format_climate_variables() |> 
  select(model,scenario,region,variable,unit,year,value,
    ca.version)
# View(climate.formatted)


# Combine and write out ----

## Combine ----
combined.formatted <- infilled.formatted |> 
  bind_rows(climate.formatted)

##  Rename scenarios and select scenarios ----
combined.formatted.filtered <- combined.formatted |> 
  fix_scenario_names() |> 
  filter(!is.na(new_scenario_name)) |> 
  filter(
    new_scenario_name != "Medium-to-Low - SSP2 (marker)"
  )

combined.formatted.filtered |> 
  distinct(model,scenario,new_scenario_name)

## Add metadata sheet ---

# TBD.


## Save ----
os.makedirs(filet.path(MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT, "for publication"),exist_ok=True)
write_xlsx(x = combined.formatted.filtered,
  path = file.path(MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT, "for publication", "climate_assessment.xlsx")
)