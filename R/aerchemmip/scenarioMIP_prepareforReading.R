# Slides for Emissions Harmonization for CMIP7.
#' Produced by Jarmo Kikstra
#'
#'
#' ScenarioMIP - all models.

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
library("officer")


here::i_am("vetting_iam.Rproj")

source(here("R","utils.R"))

# DOWNLOAD SCENARIO DATA -------------------------------------------------------
NEW.DOWNLOAD <- F

if (NEW.DOWNLOAD){
source(here("R", "download_scenarios.R"))
}

# CONFIG -----------------------------------------------------------------------
STARTYEAR <- 2025
MULTIPLE.FILES <- F
PYAM.DOWNLOADED <- T


# Select folder for scenario data ----------------------------------------------
SCENARIO.FILES.FOLDER <- here("data")


### If: one file downloaded with pyam above ------------------------------------
if (PYAM.DOWNLOADED){
  s <- vroom(file.path(SCENARIO.FILES.FOLDER,
                       # 'scenarios_scenariomip_allmodels_20240919.csv'
                       # 'scenarios_scenariomip_allmodels_20240930.csv'
                       'scenarios_scenariomip_allmodels_20241001.csv'
                       )) # needs to be same name as the one above
}

# show scenarios (show first 50)
scen.list <- s %>%
  distinct(
    Model,
    Scenario
  )
scen.list %>%
  print(n=63)



# Convert to long --------------------------------------------------------------
s <- s %>% iamc_wide_to_long(upper.to.lower = T)

# Normalise --------------------------------------------------------------------
s.n <- s %>% normalise_iamc_long(starting.year = STARTYEAR)

# General variables/models to (de)select ---------------------------------------
s <- s %>% filter(
  # model!="REMIND 3.1" # was called REMIND 3.1 in first submission, then reuploaded as REMIND-MAgPIE 3.4-4.8 (checked to have exactly the same temperature values); for original submission version (~19 sept)
)




# ScenarioMIP targets ----------------------------------------------------------

# LOAD TARGET DATA -------------------------------------------------------------
rename_scenario_sandersonsmith_to_scenariomip_guidance <- function(df){
  df %>%

    # # full naming
    # mutate_cond(scenario=="high-extension",scenario="High Emissions (H)") %>%
    # mutate_cond(scenario=="medium-extension",scenario="Medium Emissions (M)") %>%
    # mutate_cond(scenario=="medium-overshoot",scenario="Medium-Low Emissions (ML)") %>%
    # mutate_cond(scenario=="low",scenario="Low Emissions (L)") %>%
    # mutate_cond(scenario=="verylow",scenario="Very Low Emissions (VL)") %>%
    # mutate_cond(scenario=="verylow-overshoot",scenario="Low Overshoot (LOS)") %>%

    # short letters
    mutate_cond(scenario=="high-extension",scenario="H") %>%
    mutate_cond(scenario=="medium-extension",scenario="M") %>%
    mutate_cond(scenario=="medium-overshoot",scenario="ML") %>%
    mutate_cond(scenario=="low",scenario="L") %>%
    mutate_cond(scenario=="verylow",scenario="VL") %>%
    mutate_cond(scenario=="verylow-overshoot",scenario="LOS") %>%

    return()
}


targets.emissions.folder <- "C:\\Users\\kikstra\\IIASA\\ECE.prog - Documents\\SharedSocioEconomicPathways2023\\Scenario_Development_Process\\ScenarioMIP_emissions_pathway_guidelines\\version_20240903"
targets.emissions.timeseries <- read_csv(file.path(targets.emissions.folder,
                                                   "extensions_1750-2500_visualised.csv")) %>%
  rename_scenario_sandersonsmith_to_scenariomip_guidance()
targets.emissions.timeseries.iamc <- targets.emissions.timeseries %>%
  filter(specie!="CO2") %>% bind_rows(
    targets.emissions.timeseries %>% filter(specie%in%c("CO2 AFOLU", "CO2 FFI")) %>% summarise(
      value=sum(value),
      .by = c("timepoints","scenario","config")
    ) %>% mutate(specie="CO2")
  ) %>%
  mutate(year=timepoints-0.5) %>% select(-timepoints) %>%
  mutate(variable=paste0("Emissions|",specie)) %>% select(-specie) %>%
  mutate_cond(variable=="Emissions|CO2 FFI", variable="Emissions|CO2|Energy and Industrial Processes") %>%
  mutate_cond(variable=="Emissions|CO2 AFOLU", variable="Emissions|CO2|AFOLU") %>%

  # adjust units
  mutate_cond(grepl(x=variable, pattern="CO2", fixed = T), value=value*1e3) %>%
  mutate_cond(grepl(x=variable, pattern="N2O", fixed = T), value=value*1e3) %>%

  select(-config)

targets.emissions.timeseries.iamc.wide.co2.foroliver.fiveyear <- targets.emissions.timeseries.iamc %>%
  filter(variable=="Emissions|CO2",
         scenario%in%c("H", "M", "ML", "L", "VL", "LOS"),
         year%in%seq(1990,2100,5)) %>%
  pivot_wider(names_from = "year", values_from = "value")
write_delim(
  x=targets.emissions.timeseries.iamc.wide.co2.foroliver.fiveyear,
  file=here("output-other", "targets_co2_19902100_5.csv"),
  delim=","
)
targets.emissions.timeseries.iamc.wide.co2.foroliver.oneyear <- targets.emissions.timeseries.iamc %>%
  filter(variable=="Emissions|CO2",
         scenario%in%c("H", "M", "ML", "L", "VL", "LOS"),
         year%in%seq(1990,2100,1)) %>%
  pivot_wider(names_from = "year", values_from = "value")
write_delim(
  x=targets.emissions.timeseries.iamc.wide.co2.foroliver.oneyear,
  file=here("output-other", "targets_co2_19902100_1.csv"),
  delim=","
)



targets.emissions.co2budgets <- read_csv(file.path(targets.emissions.folder,
                                                   "co2_budgets_from_jan2025.csv")) %>%
  rename_scenario_sandersonsmith_to_scenariomip_guidance()


# DEFINE SCENARIO GROUPS FOR CHECKS --------------------------------------------
SCENS <- c(
  "SSP1 - High Emissions",
  "SSP1 - Low Emissions",
  "SSP1 - Low Overshoot",
  "SSP1 - Medium Emissions",
  "SSP1 - Medium-Low Emissions",
  "SSP1 - Very Low Emissions",
  "SSP2 - High Emissions",
  "SSP2 - Low Emissions",
  "SSP2 - Low Overshoot",
  "SSP2 - Medium Emissions",
  "SSP2 - Medium Overshoot",
  "SSP2 - Medium-Low Emissions",
  "SSP2 - Very Low Emissions",
  "SSP3 - High Emissions",
  "SSP3 - Medium Emissions",
  "SSP4 - Low Overshoot",
  "SSP5 - High Emissions",
  "SSP5 - Medium Emissions"
)

TARGETS <- c(
  # options
  # 1 high-extension -- High Emissions (H): something that explores high-end range emission pathways, would be good to submit SSP3 and SSP5 based variants; teams may consider even specific high emission variants of these scenarios; as indicated in the document – we expect the scenarios to come out below RCP8.5
  # 2 high-overshoot -- N/A: extension variant after 2125.
  # 3 low -- Low Emissions (L): comparable with likely 2C; reaching net zero GHG; SSP1 / SSP2 based
  # 4 medium-extension -- Medium Emissions (M): following current policies; more or less stable emissions; based on all SSPs, or if one needs to prioritize SSP2
  # 5 medium-overshoot -- Medium-Low Emissions (ML): emissions are reduced slowly after 2050 based on medium; reaching net-zero around 2100
  # 6 verylow -- Very Low Emissions (VL): lowest possible emission trajectory – trying to stay as close to C1 type scenarios as possible; SSP1/SSP2 based, this pathways should depict futures with sustainable levels of land-based CDR. Please note that the 2030 emission level should be ‘ plausible’
  # 7 verylow-overshoot -- Low Overshoot (LOS): transitioning from Low (or slightly above) to Very Low based on high levels of negative emissions (SSP1/SSP2 based), reaching the VL level around 2100 (can be slightly later). The moment of transition from Low is not specified in detail – but could be around 2050

  # # sanderson-smith naming
  # "high-extension"
  # ,"high-extension"
  # ,"medium-extension"
  # ,"medium-overshoot"
  # ,"low"
  # ,"verylow"
  # ,"verylow-overshoot"

  # # full naming
  # "High Emissions (H)"
  # ,"Medium Emissions (M)"
  # ,"Medium-Low Emissions (ML)"
  # ,"Low Emissions (L)"
  # ,"Very Low Emissions (VL)"
  # ,"Low Overshoot (LOS)"

  # short letter naming
  "H"
  ,"M"
  ,"L"
  ,"ML"
  ,"VL"
  ,"LOS"
)
TARGET.COLOURS <- c(
  '#800000', # high-extension (H)
  # '#ff0000', # high-overshoot (N/A)
  '#c87820', # medium-extension (M)
  '#d3a640', # medium-overshoot (ML)
  '#098740', # low (L)
  '#0080d0', # verylow (VL)
  '#100060' # verylow-overshoot (LOS)
)

SCENS_WITH_TARGETS_DF <- tibble(
  scenario = SCENS
) %>% mutate(target = NA) %>%
  mutate_cond(grepl(x=scenario, pattern="High Emissions", fixed=T), target = "H") %>%
  mutate_cond(grepl(x=scenario, pattern="Medium Emissions", fixed=T), target = "M") %>%
  mutate_cond(grepl(x=scenario, pattern="Low Emissions", fixed=T), target = "L") %>% # needs to come before ML and VL, to correctly overwrite
  mutate_cond(grepl(x=scenario, pattern="Medium-Low Emissions", fixed=T), target = "ML") %>%
  mutate_cond(grepl(x=scenario, pattern="Very Low Emissions", fixed=T), target = "VL") %>%
  mutate_cond(grepl(x=scenario, pattern="Low Overshoot", fixed=T), target = "LOS")

add_target <- function(df){
  df %>%
    mutate(target = NA) %>%
    mutate_cond(grepl(x=scenario, pattern="High Emissions", fixed=T), target = "H") %>%
    mutate_cond(grepl(x=scenario, pattern="Medium Emissions", fixed=T), target = "M") %>%
    mutate_cond(grepl(x=scenario, pattern="Low Emissions", fixed=T), target = "L") %>% # needs to come before ML and VL, to correctly overwrite
    mutate_cond(grepl(x=scenario, pattern="Medium-Low Emissions", fixed=T), target = "ML") %>%
    mutate_cond(grepl(x=scenario, pattern="Very Low Emissions", fixed=T), target = "VL") %>%
    mutate_cond(grepl(x=scenario, pattern="Low Overshoot", fixed=T), target = "LOS") %>%
    return()
}

# names(TARGETS) <- SCENS
names(TARGET.COLOURS) <- TARGETS

ALL_SCENS <- SCENS_WITH_TARGETS_DF %>% pull(scenario)
HIGH_SCENS <- SCENS_WITH_TARGETS_DF %>% filter(target%in%c("high-extension", "High Emissions (H)", "H")) %>% pull(scenario)
LOW_SCENS <- SCENS_WITH_TARGETS_DF %>% filter(target%in%c("low", "Low Emissions (L)", "L")) %>% pull(scenario)
MEDIUM_SCENS <- SCENS_WITH_TARGETS_DF %>% filter(target%in%c("medium-extension", "Medium Emissions (M)", "M")) %>% pull(scenario)
MEDIUMOS_SCENS <- SCENS_WITH_TARGETS_DF %>% filter(target%in%c("medium-overshoot", "Medium-Low Emissions (ML)", "ML")) %>% pull(scenario)
VERYLOW_SCENS <- SCENS_WITH_TARGETS_DF %>% filter(target%in%c("verylow", "Very Low Emissions (VL)", "VL")) %>% pull(scenario)
VERYLOWOS_SCENS <- SCENS_WITH_TARGETS_DF %>% filter(target%in%c("verylow-overshoot", "Low Overshoot (LOS)", "LOS")) %>% pull(scenario)

### Load data ------------------------------------------------------------------
rename_scenario_sandersonsmith_to_scenariomip_guidance <- function(df){
  df %>%

    # # full naming
    # mutate_cond(scenario=="high-extension",scenario="High Emissions (H)") %>%
    # mutate_cond(scenario=="medium-extension",scenario="Medium Emissions (M)") %>%
    # mutate_cond(scenario=="medium-overshoot",scenario="Medium-Low Emissions (ML)") %>%
    # mutate_cond(scenario=="low",scenario="Low Emissions (L)") %>%
    # mutate_cond(scenario=="verylow",scenario="Very Low Emissions (VL)") %>%
    # mutate_cond(scenario=="verylow-overshoot",scenario="Low Overshoot (LOS)") %>%

    # short letters
    mutate_cond(scenario=="high-extension",scenario="H") %>%
    mutate_cond(scenario=="medium-extension",scenario="M") %>%
    mutate_cond(scenario=="medium-overshoot",scenario="ML") %>%
    mutate_cond(scenario=="low",scenario="L") %>%
    mutate_cond(scenario=="verylow",scenario="VL") %>%
    mutate_cond(scenario=="verylow-overshoot",scenario="LOS") %>%

    return()
}
targets.emissions.folder <- "C:\\Users\\kikstra\\IIASA\\ECE.prog - Documents\\SharedSocioEconomicPathways2023\\Scenario_Development_Process\\ScenarioMIP_emissions_pathway_guidelines\\version_20240903"
targets.emissions.timeseries <- read_csv(file.path(targets.emissions.folder,
                                                   "extensions_1750-2500_visualised.csv")) %>%
  rename_scenario_sandersonsmith_to_scenariomip_guidance()
targets.emissions.timeseries.iamc <- targets.emissions.timeseries %>%
  filter(specie!="CO2") %>% bind_rows(
    targets.emissions.timeseries %>% filter(specie%in%c("CO2 AFOLU", "CO2 FFI")) %>% summarise(
      value=sum(value),
      .by = c("timepoints","scenario","config")
    ) %>% mutate(specie="CO2")
  ) %>%
  mutate(year=timepoints-0.5) %>% select(-timepoints) %>%
  mutate(variable=paste0("Emissions|",specie)) %>% select(-specie) %>%
  mutate_cond(variable=="Emissions|CO2 FFI", variable="Emissions|CO2|Energy and Industrial Processes") %>%
  mutate_cond(variable=="Emissions|CO2 AFOLU", variable="Emissions|CO2|AFOLU") %>%

  # adjust units
  mutate_cond(grepl(x=variable, pattern="CO2", fixed = T), value=value*1e3) %>%
  mutate_cond(grepl(x=variable, pattern="N2O", fixed = T), value=value*1e3) %>%

  select(-config)

targets.emissions.co2budgets <- read_csv(file.path(targets.emissions.folder,
                                                   "co2_budgets_from_jan2025.csv")) %>%
  rename_scenario_sandersonsmith_to_scenariomip_guidance()



# Harmonization ----------------------------------------------------------------

## Historical data -------------------------------------------------------------

### CEDS -----------------------------------------------------------------------
ceds <- vroom("C:/Users/kikstra/Documents/GitHub/emissions_harmonization_historical/data/national/ceds/processed/ceds_cmip7_national_alpha.csv") %>%
  rename(
    variable=gas,
    region=country
  ) %>%
  mutate(
    model="CEDS",
    scenario="v_2024_07_08"
  ) %>%
  select(model,scenario,region,variable,unit,sector,`1750`:`2022`) %>%
  pivot_longer(
    cols = all_of("1750"):all_of("2022"),
    names_to = "year",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  mutate(year = as.numeric(year)) %>%
  reframe(
    # sum across sectors
    value = sum(value),
    .by = c("model", "scenario", "region", "variable", "unit", "year")
  )
ceds.global <- ceds %>% filter(
  region=="World"
)

### GCB AFOLU ------------------------------------------------------------------
gcb.afolu <- read_excel(path = "C:/Users/kikstra/Documents/GitHub/emissions_harmonization_historical/data/global/other/global_gcb_afolu.xlsx",
                  sheet = "copied_from_global_trajectories") %>%
  rename(value=Net,
         year=Year) %>%
  mutate(value=value*1e3) %>%
  mutate(
    model="GCB",
    scenario="2023_v1_1",
    region="World",
    variable="CO2",
    unit="Mt CO2/yr"
  ) %>%
  select(
    model,scenario,region,variable,unit,year,value
  )

### GFED burning ---------------------------------------------------------------
gfed.burning.global <- read_csv("C:/Users/kikstra/Documents/GitHub/emissions_harmonization_historical/data/national/gfed/processed/gfed_cmip7_global_alpha.csv") %>%
  rename(
    region=country
  ) %>%
  mutate(
    model="GFED",
    scenario="downloaded 10.10.2024"
  ) %>%
  select(model,scenario,region,variable,unit,`1997`:`2023`) %>%
  pivot_longer(
    cols = all_of("1997"):all_of("2023"),
    names_to = "year",
    values_to = "value"
  ) %>%
  drop_na(value) %>%
  mutate(year = as.numeric(year))
gfed.burning.global.co2 <- gfed.burning.global %>%
  filter(grepl(x=variable, pattern="CO2", fixed = T)) %>%
  mutate(variable="CO2", unit = "Mt CO2/yr") %>%
  reframe(
    # sum across sectors
    value = sum(value),
    .by = c("model", "scenario", "region", "variable", "unit", "year")
  )


### Combined -------------------------------------------------------------------
cmip7hist.co2 <- ceds.global %>% filter(variable=="CO2") %>% select(-scenario) %>%
  bind_rows(gfed.burning.global.co2 %>% select(-scenario)) %>%
  bind_rows(gcb.afolu %>% select(-scenario)) %>%
  pivot_wider(
    names_from = model,
    values_from = value
  ) %>%
  mutate(
    value = CEDS + GCB #+ GFED
  ) %>%
  drop_na(
    value
  )

### CMIP7 data -----------------------------------------------------------------
cmip6.scenarios <- read_csv(
  "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/IAM Files from CMIP6/SSP_CMIP6_201811.csv"
) %>% allcaps_to_lower() %>% iamc_wide_to_long()
cmip6.scenarios.global <- cmip6.scenarios %>% filter(region=="World")
cmip6.scenarios.global.peat <- cmip6.scenarios.global %>% filter(
  grepl(x=variable, pattern="Peat", fixed = T)
)


## IAM 2020 handling -----------------------------------------------------------



### CO2 ------------------------------------------------------------------------

p.harmo.co2 <- ggplot(
  mapping = aes(
    x = year,
    y = value
  )
) +
  facet_wrap(~model) +
  geom_line(
    data = s %>% filter(variable=="Emissions|CO2",
                 region=="World",
                 year>=2000, year<=2030),
    colour = "dodgerblue", alpha = 0.3,
    mapping = aes(group = interaction(model,scenario,region,variable))
  ) +
  geom_line(
    data = cmip7hist.co2 %>% filter(variable=="CO2", year>=2000),
    colour = "black", linewidth = 1.2
  ) +
  mark_history() +
  ylab("Mt CO2/yr") +
  labs(title = "Initial scenario submission (will be updated)",
       subtitle = "Black: historical [CEDS + GCB]") +
  theme_jsk() + theme(panel.grid.major.y = element_line(colour = "lightgrey"))
p.harmo.co2

save_ggplot(
  f = here("figures", "harmonization_co2"),
  p = p.harmo.co2
)

## Burning ---------------------------------------------------------------------

add_species_column <- function(df){
  df %>%
    mutate(species=NA_character_) %>%
    mutate_cond(grepl(x=variable, pattern="CH4"), species="CH4") %>%
    mutate_cond(grepl(x=variable, pattern="CO2"), species="CO2") %>%
    mutate_cond(grepl(x=variable, pattern="N2O"), species="N2O") %>%
    return()
}

### Peat -----------------------------------------------------------------------

peat.iam.global <- s %>% filter(variable%in%c(
  "Emissions|CH4|AFOLU|Land|Wetlands",
  "Emissions|CO2|AFOLU|Land|Wetlands",
  "Emissions|N2O|AFOLU|Land|Wetlands"
)) %>% filter(region=="World") %>%
  add_species_column()
peat.history.global <- gfed.burning.global %>%
  filter(grepl(x=variable, pattern="Peat", fixed = T)) %>%
  filter(variable %in% c(
    "CMIP7 History|Emissions|CH4|Peat Burning",
    "CMIP7 History|Emissions|CO2|Peat Burning",
    "CMIP7 History|Emissions|N2O|Peat Burning"
  )) %>%
  # mutate(variable="CO2", unit = "Mt CO2/yr") %>%
  reframe(
    # sum across sectors
    value = sum(value),
    .by = c("model", "scenario", "region", "variable", "unit", "year")
  ) %>%
  add_species_column()
peat.cmip6 <-


p.harmo.peat <- ggplot(
  mapping = aes(
    x = year,
    y = value
  )
) +
  facet_wrap(~species, scales="free_y") +
  geom_line(
    data = cmip6.scenarios.global.peat %>% add_species_column() %>%
      filter(!is.na(species)) %>%
      filter(year>=2000, year<=2050),
    colour = "red", alpha = 0.3,
    mapping = aes(group = interaction(model,scenario,region,variable))
  ) +
  geom_line(
    data = peat.iam.global %>% filter(year>=2000, year<=2050),
    colour = "dodgerblue", alpha = 0.3,
    mapping = aes(group = interaction(model,scenario,region,variable))
  ) +
  geom_line(
    data = peat.history.global,
    colour = "black", linewidth = 1.2
  ) +
  mark_history() +
  # ylab("Mt CO2/yr") +
  labs(title = "Current scenario submission CMIP7/ScenarioMIP",
       subtitle = "Black: historical [GFED]\nRed: CMIP6 emissions") +
  theme_jsk() + theme(panel.grid.major.y = element_line(colour = "lightgrey"))
p.harmo.peat

save_ggplot(
  f = here("figures", "harmonization_peat"),
  p = p.harmo.peat
)






# Short-lived forcers / pollution ----------------------------------------------
slcf.variables <- c(
  "Emissions|Sulfur",
  "Emissions|BC"
)
s.slcf.r10 <- s %>% filter(variable%in%slcf.variables) %>% filter(
  grepl(x=region, pattern="R10", fixed=T)
)

for (v in slcf.variables){
  df <- s.slcf.r10 %>%
    filter(
      variable == v,
      year %in% seq(2010,2100,10)
    )

  variable.saving.name <- df %>%
    iamc_variable_keep_one_level(level = -1) %>%
    pull(variable) %>% unique()

  slcf.unit <- df %>%
    pull(unit) %>% unique()

  p.slcf.r10 <- ggplot(df) +
    facet_grid(model~scenario) +
    theme_jsk() +
    mark_history() +
    geom_area(
      aes(x=year, y=value,
          fill = region)
    ) +
    labs(title = paste0("Total regional emissions for: ", variable.saving.name)) +
    ylab(slcf.unit)
  p.slcf.r10

  save_ggplot(
    f = here("figures", paste0("slcf_regional_levels_",variable.saving.name)),
    p = p.slcf.r10,
    h = 400,
    w = 1000
  )


  df.percentage <- df %>%
    group_by(model,scenario,variable,unit,year) %>%
    mutate(global=sum(value)) %>%
    mutate(share=value/global)

  p.slcf.r10.percentage <- ggplot(df.percentage) +
    facet_grid(model~scenario) +
    theme_jsk() +
    mark_history() +
    geom_area(
      aes(x=year, y=share,
          fill = region)
    ) +
    labs(title = paste0("Regional share of global emissions for: ", variable.saving.name),
         caption = "Downloaded on 1 Oct 2024 from internal ScenarioMIP database") +
    ylab(slcf.unit) +
    scale_y_continuous(labels = scales::percent)
  p.slcf.r10.percentage

  save_ggplot(
    f = here("figures", paste0("slcf_regional_shares_",variable.saving.name)),
    p = p.slcf.r10.percentage,
    h = 400,
    w = 1000
  )



}

