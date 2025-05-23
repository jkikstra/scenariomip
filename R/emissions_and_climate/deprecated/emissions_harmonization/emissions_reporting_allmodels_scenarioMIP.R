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

here::i_am("vetting_iam.Rproj")

source(here("R","utils.R"))

# Scenario input file ----------------------------------------------------------
# SCENARIO_INPUT_FILE <- "scenarios_scenariomip_allmodels_20241001.csv"
SCENARIO_INPUT_FILE <- "scenarios_scenariomip_allmodels_2025-01-07.csv"



# CONFIG -----------------------------------------------------------------------
STARTYEAR <- 2025
MULTIPLE.FILES <- F
PYAM.DOWNLOADED <- T


# Select folder for scenario data ----------------------------------------------
SCENARIO.FILES.FOLDER <- here("data")


### If: one file downloaded with pyam above ------------------------------------
if (PYAM.DOWNLOADED){
  s <- vroom(file.path(SCENARIO.FILES.FOLDER,
                       SCENARIO_INPUT_FILE)) # needs to be same name as the one above
}

# Convert to long --------------------------------------------------------------
s <- s %>% iamc_wide_to_long(upper.to.lower = T)

# General filtering ------------------------------------------------------------
s <- s %>% filter(
  # model!="REMIND 3.1" # was called REMIND 3.1 in first submission, then reuploaded as REMIND-MAgPIE 3.4-4.8 (checked to have exactly the same temperature values)
)


# Filter for emissions ---------------------------------------------------------
s.em <- s %>% filter_begins_with("Emissions")
s.cdr <- s %>% filter_begins_with("Carbon Removal")

# Filter for specific emissions ------------------------------------------------
### CO2 ------------------------------------------------------------------------
AG.VARS <- c(
  "Emissions|CO2|AFOLU", # gross emissions (only positive component)
  "Emissions|CO2|AFOLU|Positive", # gross emissions (only positive component)
  "Emissions|CO2|AFOLU|Agriculture" # Agriculture (note: CEDS does not include CO2 from land-use, because ESMs won't use these emissions - but emulator harmonization does use it.)
)

CDR.VARS <- c(
  "Carbon Removal|Ocean Alkalinity Enhancement",
  "Carbon Removal|Forestry",
  "Carbon Removal|Enhanced Weathering",
  "Carbon Removal|Durable Wood Products",
  "Carbon Removal|Direct Air Capture with CCS",
  "Carbon Removal|Bioenergy with CCS"
)

TRP.VARS <- c(
  "Emissions|CO2|Energy|Demand|Transportation|Shipping|International",
  "Emissions|CO2|Energy|Demand|Transportation|Aviation"
)

EIP.VARS <- c(
  "Emissions|CO2|Energy",
  "Emissions|CO2|Industrial Processes",
  "Emissions|CO2|Energy|Demand|Residential and Commercial and AFOFI",
  "Emissions|CO2|Waste",
  "Emissions|CO2|Product Use|Solvents"
)

CO2.VARS <- c(
  AG.VARS,
  CDR.VARS,
  TRP.VARS,
  EIP.VARS
)

s.em.co2.cmip7 <-
  tibble(variable=CO2.VARS) %>%
  left_join(
   s %>% filter(variable%in%CO2.VARS) %>%
     simplify_model_names() %>%
     distinct(model,variable) %>%
     mutate(reported="True") %>%
     pivot_wider(names_from = model, values_from = reported, values_fill = "False")
  ) %>%
  pivot_longer(cols = AIM:WITCH, names_to = "model", values_to = "value")

s.em.co2.cmip7 %>% mutate_cond(is.na(value), value="Missing") %>%
  ggplot(
    aes(
      x=model,
      y=variable,
      fill=value
    )
  ) +
  geom_tile(color = "white") +  # Heatmap tiles
  scale_fill_manual(values = c("True" = "green", "False" = "red", "Missing" = "grey")) +  # Colors for TRUE/FALSE
  theme_minimal() +  # Clean theme
  labs(title = "CO2 variable reporting", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Non-CO2 --------------------------------------------------------------------
bc.AG.VARS <- c(
  "Emissions|BC|AFOLU",
  "Emissions|BC|AFOLU|Agriculture"
)

bc.BURN.VARS <- c(
  "Emissions|BC|AFOLU|Biomass Burning",
  "Emissions|BC|AFOLU|Land|Forest Burning",
  "Emissions|BC|AFOLU|Land|Grassland Burning",
  "Emissions|BC|AFOLU|Land|Wetlands"
)

bc.TRP.VARS <- c(
  "Emissions|BC|Energy|Demand|Transportation|Shipping|International",
  "Emissions|BC|Energy|Demand|Transportation|Aviation"
)

bc.EIP.VARS <- c(
  "Emissions|BC|Energy",
  "Emissions|BC|Industrial Processes",
  "Emissions|BC|Energy|Demand|Residential and Commercial and AFOFI",
  "Emissions|BC|Waste",
  "Emissions|BC|Product Use|Solvents"
)

NONCO2.VARS <- c(
  bc.AG.VARS,
  bc.BURN.VARS,
  bc.TRP.VARS,
  bc.EIP.VARS
)

other.SPECIES <- c(
  paste0("Emissions|",
         c(
           "HFC", "C2F6", "CF4", "SF6"
         )
         )
)

s.em.bc.cmip7 <-
  tibble(variable=NONCO2.VARS) %>%
  left_join(
    s %>% filter(variable%in%NONCO2.VARS) %>%
      simplify_model_names() %>%
      distinct(model,variable) %>%
      mutate(reported="True") %>%
      pivot_wider(names_from = model, values_from = reported, values_fill = "False")
  ) %>%
  pivot_longer(cols = AIM:WITCH, names_to = "model", values_to = "value")

s.em.other.cmip7 <- tibble(variable=other.SPECIES) %>%
  left_join(
    s %>% filter(variable%in%other.SPECIES) %>%
      simplify_model_names() %>%
      distinct(model,variable) %>%
      mutate(reported="True") %>%
      pivot_wider(names_from = model, values_from = reported, values_fill = "False")
  ) %>%
  pivot_longer(cols = AIM:WITCH, names_to = "model", values_to = "value")

s.em.bc.cmip7 %>% mutate_cond(is.na(value), value="Missing") %>%
  ggplot(
    aes(
      x=model,
      y=variable,
      fill=value
    )
  ) +
  geom_tile(color = "white") +  # Heatmap tiles
  scale_fill_manual(values = c("True" = "green", "False" = "red", "Missing" = "grey")) +  # Colors for TRUE/FALSE
  theme_minimal() +  # Clean theme
  labs(title = "Non-CO2 variable reporting (BC)", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

s.em.other.cmip7 %>% mutate_cond(is.na(value), value="Missing") %>%
  ggplot(
    aes(
      x=model,
      y=variable,
      fill=value
    )
  ) +
  geom_tile(color = "white") +  # Heatmap tiles
  scale_fill_manual(values = c("True" = "green", "False" = "red", "Missing" = "grey")) +  # Colors for TRUE/FALSE
  theme_minimal() +  # Clean theme
  labs(title = "Non-CO2 variable reporting (Other)", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
