# Load libraries ---------------------------------------------------------------
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

here::i_am("vetting_iam.Rproj")

source(here("R","utils.R"))

# Scenario input file ----------------------------------------------------------
scenarios_raw <- read_csv(here("data",
                               "downloading_iters",
                               "scenarios_scenariomip_REMIND-MAgPIE 3.4-4.8_SSP2 - Medium Emissions.csv"))

write_delim(x = scenarios_raw,
            file = here("data", "aggregation_testing", "input_scenario.csv"),
            delim = ",")


scenarios <- scenarios_raw %>%
  filter(str_starts(Variable, "Emissions")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  mutate(sector = str_replace(variable, "^Emissions\\|", "")) %>%
  mutate(species = str_extract(sector, "^[^|]+")) %>%
  mutate(sector = ifelse(
    species==sector,
    "Total",
    str_replace(sector, paste0("^",species,"\\|"), "")
  ))


## Industrial Sector -----------------------------------------------------------

scenarios_raw_industrial <- scenarios %>%
  filter(
    sector %in% c(
      # "Energy|Supply",
      "Energy|Demand|Industry",
      "Energy|Demand|Other Sector",
      "Industrial Processes",
      "Other"
    )
  )
scenarios_raw_industrial_summed <- scenarios_raw_industrial %>%
  reframe(
    value = ifelse(is.na(sum(value)),
                   0,
                   sum(value)),
    .by = c("model", "scenario", "region", "unit", "year", "species")
  ) %>%
  mutate(sector = "Industrial Sector")

scenarios_raw_industrial_summed_iamc <- scenarios_raw_industrial_summed %>%
  mutate(variable = paste0("CMIP7 Harmonization|Input|", "Emissions|", species,"|",sector)) %>%
  select(model,scenario,region,variable,unit, year, value)



## Aircraft --------------------------------------------------------------------
scenarios_raw_aircraft <- scenarios %>%
  filter(
    sector %in% c(
      "Energy|Demand|Bunkers|International Aviation",
      "Energy|Demand|Transportation|Domestic Aviation"
    )
  )
scenarios_raw_aircraft_summed <- scenarios_raw_aircraft %>%
  reframe(
    value = ifelse(is.na(sum(value)),
                   0,
                   sum(value)),
    .by = c("model", "scenario", "region", "unit", "year", "species")
  ) %>%
  mutate(sector = "Aircraft")

scenarios_raw_aircraft_summed_iamc <- scenarios_raw_aircraft_summed %>%
  mutate(variable = paste0("CMIP7 Harmonization|Input|", "Emissions|", species,"|",sector)) %>%
  select(model,scenario,region,variable,unit,year,value) %>%
  filter(
    # only keep World for Aircraft
    region=="World"
  )



## Transportation Sector -------------------------------------------------------
scenarios_raw_transport <- scenarios %>%
  filter(
    sector %in% c(
      "Energy|Demand|Transportation",
      "Energy|Demand|Transportation|Domestic Aviation"
    )
  )
scenarios_raw_transport_summed <- scenarios_raw_transport %>%
  select(-variable) %>%
  pivot_wider(names_from = sector, values_from = value) %>%
  mutate(value = `Energy|Demand|Transportation` - `Energy|Demand|Transportation|Domestic Aviation`) %>%
  filter(region=="World") %>%
  reframe(
    value = ifelse(is.na(sum(value)),
                   0,
                   sum(value)),
    .by = c("model", "scenario", "region", "unit", "year", "species")
  ) %>%
  mutate(sector = "Transportation Sector")

scenarios_raw_transport_summed_iamc <- scenarios_raw_transport_summed %>%
  mutate(variable = paste0("CMIP7 Harmonization|Input|", "Emissions|", species,"|",sector)) %>%
  select(model,scenario,region,variable,unit,year,value) %>%
  filter(
    # only keep World for Aircraft
    region=="World"
  )


# Finalise ---------------------------------------------------------------------

## rename existing
scenarios_out <- scenarios %>% filter(
  # filter
  sector %in% c(
    "Total",

    "Energy|Supply",
    "Energy|Demand|Bunkers|International Shipping",
    "Energy|Demand|Residential and Commercial and AFOFI",
    "Product Use",
    "AFOLU|Agriculture",
    "AFOLU|Agricultural Waste Burning",
    "AFOLU|Land|Fires|Forest Burning",
    "AFOLU|Land|Fires|Grassland Burning",
    "AFOLU|Land|Fires|Peat Burning",
    "Waste"
  )
) %>%
  # rename
  mutate_cond(sector=="Energy|Supply", sector="Energy Sector") %>%
  mutate_cond(sector=="Energy|Demand|Bunkers|International Shipping", sector="International Shipping") %>%
  mutate_cond(sector=="Energy|Demand|Residential and Commercial and AFOFI", sector="Residential Commercial Other") %>%
  mutate_cond(sector=="Product Use", sector="Solvents Production and Application") %>%
  mutate_cond(sector=="AFOLU|Agriculture", sector="Agriculture") %>%
  mutate_cond(sector=="AFOLU|Agricultural Waste Burning", sector="Agricultural Waste Burning") %>%
  mutate_cond(sector=="AFOLU|Land|Fires|Forest Burning", sector="Forest Burning") %>%
  mutate_cond(sector=="AFOLU|Land|Fires|Grassland Burning", sector="Grassland Burning") %>%
  mutate_cond(sector=="AFOLU|Land|Fires|Peat Burning", sector="Peat Burning") %>%
  mutate_cond(sector=="Waste", sector="Waste") %>%
  # remove helper columns
  mutate(variable = paste0("CMIP7 Harmonization|Input|", species, "|", sector)) %>%
  select(-species,-sector) %>%
  # add adjusted variables
  bind_rows(scenarios_raw_industrial_summed_iamc) %>%
  bind_rows(scenarios_raw_aircraft_summed_iamc) %>%
  bind_rows(scenarios_raw_transport_summed_iamc) %>%

  # to wide format
  iamc_long_to_wide()

scenarios_out
# Write out test files

write_delim(x = scenarios_out,
            file = here("data", "aggregation_testing", "expected_output_scenario.csv"),
            delim = ",")


