# Load libraries ---------------------------------------------------------------
library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("patchwork")
library("ggthemes")
library("ggsci")
library("testthat")
library("geomtextpath")
library("stringr")
library("ggthemes")

here::i_am("vetting_iam.Rproj")

source(here("R","utils.R"))

# Historical data file ---------------------------------------------------------
history <- load_csv_iamc(
  "C:/Users/kikstra/Documents/GitHub/emissions_harmonization_historical/data/iamc_regions_cmip7_history.csv"
) %>% filter(grepl(region,pattern="MESSAGE",fixed=T)) %>%
  iamc_wide_to_long() %>%
  mutate(variable = str_remove(variable, "^CMIP7 History\\|")) %>% # ad-hoc
  filter(
    year>=1997, year<=2022
  )


# Scenario input file ----------------------------------------------------------
scenarios <- read_csv(
  "C:/Users/kikstra/Documents/GitHub/concordia/results/config_cmip7_v0_testing/scenarios_processed.csv"
) %>% mutate(variable = paste0("Emissions|",species,"|",variable)) %>% select(-species)

scenarios_raw <- read_csv(here("data", paste0("scenarios_scenariomip_allmodels_2025-01-07-message.csv")))

scenarios_raw_industrial <- scenarios_raw %>%
  filter(str_starts(Variable, "Emissions")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  mutate(sector = str_replace(variable, "^Emissions\\|", "")) %>%
  mutate(species = str_extract(sector, "^[^|]+")) %>%
  mutate(sector = ifelse(
    species==sector,
    "Total",
    str_replace(sector, paste0("^",species,"\\|"), "")
  ))
scenarios_raw_industrial_filtered <- scenarios_raw_industrial %>%
  filter(
    sector %in% c(
      "Energy|Supply",
      "Energy|Demand|Industry",
      "Energy|Demand|Other Sector",
      "Industrial Processes",
      "Other"
    )
  )
scenarios_raw_industrial_filtered %>% pull(sector) %>% unique()


scenarios_raw_industrial_summed <- scenarios_raw_industrial_filtered %>%
  reframe(
    fixed.value = sum(value),
    .by = c("model", "scenario", "region", "unit", "year")
  ) %>%
  mutate(sector = "Industrial Sector")

# plot differences -------------------------------------------------------------

scenarios %>% region_unique()
history %>% region_unique()

scenarios %>% variable_unique()
history %>% variable_unique()

# Assuming the column 'variable' always has a string two "|" characters, make a column 'species' that is in-between the two pipe characters, and make a column 'sector' that is after the last pipe character
df <- bind_rows(
  history,
  scenarios
) %>% mutate(
  species = str_extract(variable, "\\|([^|]+)\\|") %>% str_remove_all("\\|"),
  sector = str_extract(variable, "[^|]+$") # Extract text after the last '|'
) %>%
  left_join(scenarios_raw_industrial_summed) %>% # ad-hoc
  mutate_cond(sector=="Industrial Sector" & model!="History", value=fixed.value) %>% select(-fixed.value) %>% # ad-hoc
  mutate_cond(species=="NMVOC", species="VOC") # ad-hoc


# plot
for (s in df %>% pull(sector) %>% unique() ){

  p.data <- df %>% filter(sector==s,
                          year>=2000) %>%
    filter(region!="World") %>%
    mutate(
      region = str_extract(region, "[^|]+$")
    ) %>% filter(
      region=="China",
      grepl(variable,pattern="CO2",fixed=T)
    )

  # Check if the tibble is empty
  if (nrow(p.data) == 0) {
    cat("Empty tibble, skipping iteration\n")
    next  # Skip to the next iteration
  }


  p.all.emissions <- ggplot(p.data,
                          aes(x=year,y=value,colour=scenario)) +
    facet_grid(species~region, scales="free_y") +
    geom_line(
      aes(group=interaction(model,scenario,region,variable))
    ) +
    geom_point(
      data = . %>% filter(model=="History")
    ) +
    theme_jsk()
  p.all.emissions

  save_ggplot(f = here("figures", "emissions-vetting", "message_co2high", s),
              p = p.all.emissions,
              h = 300,
              w = 500
  )
}

