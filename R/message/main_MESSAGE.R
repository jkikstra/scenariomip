# Do vetting checks for IAM scenarios - developed for ScenarioMIP, related to SSP update.
#' Produced by Jarmo Kikstra
#'
#'
#' Mainly for MESSAGEix usage.


# TODO list --------------------------------------------------------------------
#' - [x] loading scenarios
#' - [ ] trend breaks
#' - [ ] scenario logic numbs
#' - [ ] aggregation checks
#'

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


# NOTES (10.09.2024) ====
#' things to look at scenariomip
#' - [ ]
#' - [ ] contraints for negative emissions after 2070 (Yoga: CO2 injection constraint)
#' - [ ] all NDC 2030 where necessary?
#' - [ ] 2030 verylow 'plausible'; needs to be less than 25% -- Volker: gut 30-35. Jarmo: ideally regional feasibility constraints
#' - [ ] low enough negative; in the end of century strongly negative



# DOWNLOAD SCENARIO DATA -------------------------------------------------------
NEW.DOWNLOAD <- T

if (NEW.DOWNLOAD){
# Install reticulate package if not already installed
if (!require("reticulate")) install.packages("reticulate")

# Import the reticulate package
library(reticulate)

# Configure reticulate to use the correct Python environment
use_python("C:\\Users\\kikstra\\AppData\\Local\\miniconda3\\envs\\pyam")  # Adjust this path to your Python environment if needed

# # Install Python packages if needed
# py_install("pyam-iamc", pip = TRUE)

# Define the Python code to retrieve the scenario
py_run_string("
import pyam
import os

# Establish connection to the ixmp4 platform 'ece-internal'
pyam.iiasa.Connection('ece_internal')

# Retrieve the desired scenarios
df = pyam.read_iiasa(
    'ece_internal',
    model=[#'SSP_LED_v1.0', # not submitted as such
    'SSP_SSP1_v1.0', 'SSP_SSP2_v1.0', 'SSP_SSP3_v1.0', 'SSP_SSP4_v1.0', 'SSP_SSP5_v1.0'],
    scenario=[
    #'baseline', 'baseline_1000f', # not submitted
    'SSP1 - Low Emissions', 'SSP1 - Very Low Emissions',
    'SSP2 - Low Overshoot', 'SSP2 - Medium Emissions', 'SSP2 - Medium-Low Emissions',  'SSP2 - Low Emissions',  'SSP2 - Very Low Emissions',
    'SSP3 - High Emissions',
    'SSP4 - Low Overshoot',
    'SSP5 - High Emissions',
    #'SSP5 - Low Overshoot' # not submitted
    ]
)

# Write out the scenario data
output_file = 'scenarios_message.csv'
df.to_csv(
    os.path.join('data',output_file)
)
print(f'Data has been written to {output_file}')

")
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
                       'scenarios_message.csv')) # needs to be same name as the one above
}


### If: multiple files ---------------------------------------------------------
if (MULTIPLE.FILES){

  ONLY.CSV <- T
  ONLY.EXCEL <- F
  if (!ONLY.EXCEL){
    FILES.csv <- file.path(SCENARIO.FILES.FOLDER, dir(SCENARIO.FILES.FOLDER, pattern = "*.csv"))  # get file names
    scenarios_csv <- FILES.csv %>%
      map(~ (vroom(.)) ) %>%
      reduce(rbind)
  }


  if (ONLY.CSV){
    s <- scenarios_csv
  } else {
    FILES.xlsx <- file.path(SCENARIO.FILES.FOLDER, dir(SCENARIO.FILES.FOLDER, pattern = "*.xlsx"))  # get file names
    scenarios_xlsx <- FILES.xlsx %>%
      map(~ (read_excel(.)) ) %>%
      reduce(rbind)
    if (ONLY.EXCEL){
      s <- scenarios_xlsx
    } else {
      s <- scenarios_csv %>%
        bind_rows(scenarios_xlsx)
    }
  }

}

# show scenarios (show first 50)
scen.list <- s %>%
  distinct(
    Model,
    Scenario
  )
scen.list %>%
  print(n=50)



# Convert to long --------------------------------------------------------------
s <- s %>% iamc_wide_to_long(upper.to.lower = T)

# Normalise --------------------------------------------------------------------
s.n <- s %>% normalise_iamc_long(starting.year = STARTYEAR)

# General variables to select --------------------------------------------------
s <- s %>% filter()










# AGGREGATION CHECKS -----------------------------------------------------------
# Primary Energy Coal
agg.list <- list(
  `Primary Energy|Coal` = c(
    "Primary Energy|Coal|Ammonia",
    "Primary Energy|Coal|Electricity",
    "Primary Energy|Coal|Gases",
    "Primary Energy|Coal|Heat",
    "Primary Energy|Coal|Hydrogen",
    "Primary Energy|Coal|Liquids",
    "Primary Energy|Coal|Solids"
  ), #


  `Primary Energy|Coal` = c(
    "Primary Energy|Coal|w/ CCS",
    "Primary Energy|Coal|w/o CCS"
  ),

  `Primary Energy|Gas` = c(
    "Primary Energy|Gas|Ammonia",
    "Primary Energy|Gas|Electricity",
    "Primary Energy|Gas|Gases",
    "Primary Energy|Gas|Heat",
    "Primary Energy|Gas|Hydrogen",
    "Primary Energy|Gas|Liquids",
    "Primary Energy|Gas|Solids"
  ),
  `Primary Energy|Gas` = c(
    "Primary Energy|Gas|w/ CCS",
    "Primary Energy|Gas|w/o CCS"
  ),

  `Primary Energy|Oil` = c(
    "Primary Energy|Oil|Ammonia",
    "Primary Energy|Oil|Electricity",
    "Primary Energy|Oil|Gases",
    "Primary Energy|Oil|Heat",
    "Primary Energy|Oil|Hydrogen",
    "Primary Energy|Oil|Liquids",
    "Primary Energy|Oil|Solids"
  ),
  `Primary Energy|Oil` = c(
    "Primary Energy|Oil|w/ CCS",
    "Primary Energy|Oil|w/o CCS"
  ),

  # add: biomass (ask yoga)

  `Primary Energy` = c(
    "Primary Energy|Biomass",
    "Primary Energy|Coal",
    "Primary Energy|Gas",
    "Primary Energy|Geothermal",
    "Primary Energy|Hydro",
    "Primary Energy|Nuclear",
    "Primary Energy|Ocean",
    "Primary Energy|Oil",
    "Primary Energy|Other",
    "Primary Energy|Secondary Energy Trade",
    "Primary Energy|Solar",
    "Primary Energy|Wind"
  ),

  # primary energy|Fossil: check sum of ...









  `Final Energy` = c(
    "Final Energy|Transportation",
    "Final Energy|Residential and Commercial",
    "Final Energy|Industry",
    "Final Energy|Non-Energy Use"
  ),
  `Final Energy` = c(
    "Final Energy|Electricity",
    "Final Energy|Gases",
    "Final Energy|Geothermal",
    "Final Energy|Heat",
    "Final Energy|Hydrogen",
    "Final Energy|Liquids",
    "Final Energy|Solar",
    "Final Energy|Solids"
  )#,
  #' to ADD:
  #' Secondary Energy
  #' Primary Energy (total)
  #' Primary Energy|* (other than coal)
)

return_diff_aggregation <- function(df, agg.list, agg.list.entry){

  df.total.values <- df %>% filter(variable==names(agg.list)[agg.list.entry])
  df.agg.values <- df %>% filter(variable%in%(agg.list[agg.list.entry] %>% unlist())) %>%
    reframe(agg.value=sum(value),
            .by = c("model", "scenario", "region", "unit", "year"))
  df.diff <- df.total.values %>%
    left_join(df.agg.values, by = join_by("model", "scenario", "region", "unit", "year")) %>%
    mutate(diff=value-agg.value) %>%
    filter(diff!=0)

  return(df.diff)

}

test_that("Aggregation of secondary level variables; compare to reported total",{

  for (i in seq(1,length(agg.list))){

    df.diff = return_diff_aggregation(df=s, agg.list = agg.list, agg.list.entry = i)

    expect_equal(
      df.diff %>% pull(diff) %>% sum(),
      0,
      tolerance = 1e-10,
      label = paste("Aggregation check fails for:", names(agg.list)[i], "\n")
    )
  }

})








# TREND BREAK CHECKS -----------------------------------------------------------
#' TODO:
#' - [ ] add smoothing / trend (e.g., average change in past 15 years) & average from previous timesteps (rolling window) for information on trend


# tbd.
VERY.SMALL.NUMBER.ABSOLUTE <- 0.01
VERY.SMALL.NUMBER.GLOBALSHARE <- 0.01
VERY.SMALL.NUMBER.NORMALISED <- 0.1

TOO.BIG.CHANGE.DOWN <- -0.5
TOO.BIG.CHANGE.UP <- 1

s.start <- s %>% filter(year==STARTYEAR) %>% select(-year) %>% rename(start.value=value)
s.start.global <- s.start %>% filter(region=="World") %>% select(-region) %>% rename(global.start.value=start.value)

s.n.trends <- s.n %>%
  # filter(variable=="Primary Energy") %>%
  group_by(model,scenario,region,variable,unit) %>%
  mutate(change.from.start=value-1) %>%
  mutate(previous.value=lag(value,default=NA_real_)) %>%
  mutate(change.timestep=(value-previous.value)/previous.value) %>%
  filter(year>STARTYEAR) %>%
  ungroup()

s.n.trends.info <- s.n.trends %>%
  left_join(s.start) %>% left_join(s.start.global) %>%
  # flag values with a very small starting value
  mutate(flag.small.absolute.start = ifelse(
    start.value < VERY.SMALL.NUMBER.ABSOLUTE,
    T,F
  )) %>%
  mutate(flag.small.globalshare.start = ifelse(
    start.value/global.start.value < VERY.SMALL.NUMBER.GLOBALSHARE,
    T,F
  )) %>%
  mutate(flag.small.previoustimestep.normalised = ifelse(
    lag(value) < VERY.SMALL.NUMBER.NORMALISED,
    T,F
  ))


s.n.trends.potential.issues.down <- s.n.trends.info %>%
  filter(change.timestep < TOO.BIG.CHANGE.DOWN)
s.n.trends.potential.issues.up <- s.n.trends.info %>%
  filter(change.timestep < TOO.BIG.CHANGE.UP)



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

targets.emissions.co2budgets <- read_csv(file.path(targets.emissions.folder,
                                                   "co2_budgets_from_jan2025.csv")) %>%
  rename_scenario_sandersonsmith_to_scenariomip_guidance()


# DEFINE SCENARIO GROUPS FOR CHECKS --------------------------------------------
SCENS <- c(
  "baseline",
  "SSP2 - Very Low Emissions",
  "baseline",
  "SSP1 - Low Emissions",
  "SSP1 - Very Low Emissions",
  "baseline",
  "baseline_1000f",
  "SSP2 - Low Overshoot",
  "SSP2 - Medium Emissions",
  "SSP2 - Medium-Low Emissions",
  "SSP2 - Low Emissions",
  "SSP3 - High Emissions",
  "baseline_1000f",
  "baseline",
  "baseline_1000f",
  "SSP4 - Low Overshoot",
  "SSP5 - High Emissions",
  "SSP5 - Low Overshoot",
  "baseline_1000f"
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





# NARRATIVE CHECKS -------------------------------------------------------------

### Temperature ----------------------------------------------------------------
s.temp50 <- s %>% filter(variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile")
s.exc.2c <- s %>% filter(variable=="AR6 climate diagnostics|Exceedance Probability 2.0C|MAGICCv7.5.3")
s.exc.1p5c <- s %>% filter(variable=="AR6 climate diagnostics|Exceedance Probability 1.5C|MAGICCv7.5.3")

test_that("Is temperature is in the expected ranges",{

  # H
  for (s.h in HIGH_SCENS){
    expect_gt(
      s.temp50 %>% filter(year==2100, scenario==s.h) %>% pull(value),
      3, label = paste("Temperature too low for scenario:", s.h, "\n")
    )
    expect_lt(
      s.temp50 %>% filter(year==2100, scenario==s.h) %>% pull(value),
      4, label = paste("Temperature too high for scenario:", s.h, "\n")
    )

    expect_gt(
      s.exc.2c %>% filter(year==2100, scenario==s.h) %>% pull(value),
      0.99, label = paste("Chance to exceed 2C in 2100 too low for scenario:", s.h, "\n")
    )
    expect_gt(
      s.exc.2c %>% filter(year==2050, scenario==s.h) %>% pull(value),
      0.4, label = paste("Chance to exceed 2C in 2050 too low for scenario:", s.h, "\n")
    )
    expect_gt(
      s.exc.1p5c %>% filter(year==2050, scenario==s.h) %>% pull(value),
      0.75, label = paste("Chance to exceed 1.5C in 2050 too low for scenario:", s.h, "\n")
    )
  }

  # L : likely 2C
  # TBD



  # VL and LOS
  for (s.h in c(VERYLOW_SCENS, VERYLOWOS_SCENS)){
    # 2100 within 1 and 1.5 (medium)
    expect_gt(
      s.temp50 %>% filter(year==2100, scenario==s.h) %>% pull(value),
      1, label = paste("Temperature too low for scenario:", s.h, "\n")
    )
    expect_lt(
      s.temp50 %>% filter(year==2100, scenario==s.h) %>% pull(value),
      1.5, label = paste("Temperature too high for scenario:", s.h, "\n")
    )

    # probabilities of exceeding
    expect_gt(
      s.exc.1p5c %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      0.5, label = paste("Chance to exceed 1.5C unexpectedly low for scenario:", s.h, "\n")
    )
    expect_lt(
      s.exc.1p5c %>% filter(year==2100, scenario==s.h) %>% pull(value),
      0.33, label = paste("Chance to exceed 1.5C in 2100 too high (>33%) for scenario:", s.h, "\n")
    )
    expect_gt(
      s.exc.2c %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      0.1, label = paste("Chance to exceed 2C unexpectedly low for scenario:", s.h, "\n")
    )
  }
  # VL (additional)
  for (s.h in c(VERYLOW_SCENS)){
    # medium temp
    expect_lt(
      s.temp50 %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      1.7, label = paste("Temperature too high for scenario:", s.h, "\n")
    )

    # probabilities of exceeding
    expect_lt(
      s.exc.1p5c %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      0.7, label = paste("Chance to exceed 1.5C unexpectedly high (>70%) for scenario:", s.h, "\n")
    )
  }
  # LOS (additional)
  for (s.h in c(VERYLOWOS_SCENS)){
    # medium temp
    expect_gt(
      s.temp50 %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      1.6, label = paste("Temperature overshoot too low for scenario:", s.h, "\n")
    )
    # probabilities of exceeding
    expect_gt(
      s.exc.1p5c %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      0.65, label = paste("Chance to exceed 1.5C unexpectedly low for scenario:", s.h, "\n")
    )
    expect_gt(
      s.exc.2c %>% filter(scenario==s.h) %>% pull(value) %>% max(),
      0.2, label = paste("Chance to exceed 2C unexpectedly low for scenario:", s.h, "\n")
    )
  }

})

##### Visualise temperatute ----------------------------------------------------
p.temps <- ggplot(
  s.temp50 %>%
    add_target(),
  aes(x=year,y=value,colour=target,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(~target) +
  mark_history() +
  geom_texthline(yintercept = 1.5, label="1.5C", linetype="dashed", colour="black") +
  geom_textpath(aes(label=scenario), linetype="solid", size = 3,
                hjust=0.9) +
  geom_text(data=. %>% filter(year==2100), aes(label=round(value,digits = 1),
                                               x=2110)) +
  theme_jsk() +
  ylab("C above pre-industrial") +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS))

save_ggplot(
  p = p.temps,
  h = 200,
  w = 450,
  f = here("figures", "temps")
)

### Fossil Primary Energy ------------------------------------------------------

##### Gas & Coal & Oil: Overview comparison figures -----------------------------------

p.gascoaloil.normalised2025 <- ggplot(
  s.n %>% filter(
    variable %in% c(
      "Primary Energy|Coal",
      "Primary Energy|Gas",
      "Primary Energy|Oil"
    )
  ) %>%
    add_target(),
  aes(x=year,y=value,colour=target,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(variable~region, scales = "free_y") +
  mark_history() +
  geom_hline(yintercept = 1, linetype="dashed", colour="black") +
  geom_textpath(aes(label=scenario), linetype="solid", size = 2) +
  theme_jsk() +
  ylab("Normalised (=1) in 2025") +
  scale_y_continuous(transform = "log10") +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS))

save_ggplot(
  p = p.gascoaloil.normalised2025,
  h = 400,
  w = 400,
  f = here("figures", "coal_gas_oil_normalised2025")
)

p.gascoaloil.absolute2025 <- ggplot(
  s %>% filter(
    variable %in% c(
      "Primary Energy|Coal",
      "Primary Energy|Gas",
      "Primary Energy|Oil"
    )
  ) %>%
    add_target(),
  aes(x=year,y=value,colour=variable,linetype=scenario,
      group=interaction(model,scenario,region,variable))
) +
  facet_wrap(~region, scales = "free_y") +
  mark_history() +
  geom_textpath(aes(label=scenario), linetype="solid", size = 2) +
  theme_jsk() +
  ylab("EJ/yr")

save_ggplot(
  p = p.gascoaloil.absolute2025,
  h = 400,
  w = 400,
  f = here("figures", "coal_gas_oil_absolute")
)

##### Figures per fossil variable ----------------------------------------------

for (v in c("Primary Energy|Coal",
            "Primary Energy|Gas",
            "Primary Energy|Oil")){
  p.fossil.normalised2025 <- ggplot(
    s.n %>% filter(
      variable==v
    ) %>%
      add_target(),
    aes(x=year,y=value,colour=target,
        group=interaction(model,scenario,region,variable))
  ) +
    facet_grid(target~region, scales = "free_y") +
    mark_history() +
    geom_hline(yintercept = 1, linetype="dashed", colour="black") +
    geom_textpath(aes(label=scenario), linetype="solid", size = 2) +
    theme_jsk() +
    ylab("Normalised (=1) in 2025") +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    labs(title = v)

  save_ggplot(
    p = p.fossil.normalised2025,
    h = 400,
    w = 400,
    f = here("figures", paste0(gsub("\\|", "_", v), "_normalised2025"))
  )

  p.fossil.absolute2025 <- ggplot(
    s %>% filter(
      variable == v
    ) %>%
      add_target(),
    aes(x=year,y=value,colour=target,linetype=scenario,
        group=interaction(model,scenario,region,variable))
  ) +
    facet_grid(target~region, scales = "free_y") +
    mark_history() +
    geom_textpath(aes(label=scenario), linetype="solid", size = 2) +
    theme_jsk() +
    ylab("EJ/yr") +
    labs(title = v)

  save_ggplot(
    p = p.fossil.absolute2025,
    h = 400,
    w = 400,
    f = here("figures", paste0(gsub("\\|", "_", v), "_absolute"))
  )
}

##### Figures per target scenario ----------------------------------------------

for (t in TARGETS){
  p.fossil.normalised2025 <- ggplot(
    s.n %>% add_target() %>%
      filter(
        target==t
      ),
    aes(x=year,y=value,colour=target,
        group=interaction(model,scenario,region,variable))
  ) +
    facet_grid(target~region, scales = "free_y") +
    mark_history() +
    geom_hline(yintercept = 1, linetype="dashed", colour="black") +
    geom_textpath(aes(label=scenario), linetype="solid", size = 2) +
    theme_jsk() +
    ylab("Normalised (=1) in 2025") +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    labs(title = v)

  save_ggplot(
    p = p.fossil.normalised2025,
    h = 400,
    w = 400,
    f = here("figures", paste0(gsub("\\|", "_", v), "_normalised2025"))
  )

  p.fossil.absolute2025 <- ggplot(
    s %>% add_target() %>%
      filter(
        target==t
      ),
    aes(x=year,y=value,colour=variable,linetype=scenario,
        group=interaction(model,scenario,region,variable))
  ) +
    facet_grid(target~region, scales = "free_y") +
    mark_history() +
    geom_textpath(aes(label=scenario), linetype="solid", size = 2) +
    theme_jsk() +
    ylab("EJ/yr") +
    labs(title = v)

  save_ggplot(
    p = p.fossil.absolute2025,
    h = 400,
    w = 400,
    f = here("figures", paste0(gsub("\\|", "_", v), "_absolute"))
  )
}



### Value checks ---------------------------------------------------------------

##### Coal ---------------------------------------------------------------------
s.coal <- s %>% filter(variable=="Primary Energy|Coal")
s.coal.n <- s.coal %>% normalise_iamc_long(starting.year=STARTYEAR)

test_that("Is coal in the expected ranges",{

  # High emissions pathways
  for (s.h in HIGH_SCENS){
    expect_gt(
      s.coal.n %>% filter(year==2100, scenario==s.h, region=="World") %>% pull(value),
      1.75, label = paste("Global coal is not higher than 175% of the ", as.character(STARTYEAR)," value in 2100, for scenario:", s.h, "\n")
    )
    expect_lt(
      s.coal.n %>% filter(year==2100, scenario==s.h, region=="World") %>% pull(value),
      3, label = paste("Global coal is more than 300% of the ", as.character(STARTYEAR)," value in 2100, for scenario:", s.h, "\n")
    )

    expect_gt(
      s.coal.n %>% filter(year==2100, scenario==s.h) %>% pull(value) %>% min(), # all regions: no phase out in any region
      0.75, label = paste("Coal in some region has decreased by more than 25% from its ", as.character(STARTYEAR)," value by 2100, for scenario:", s.h, "\n")
    )

  }

})





##### Shares of use ------------------------------------------------------------
# QUESTIONS:
#' * look into sectors: where does coal & gas go.
#' - coal to liquids?
#' - direct end-use (least likely for SSP5 under high development)
#' i.e. "Primary Energy|Coal|Liquids" and "Primary Energy|Coal|Gases" (- shouldn't be too much hydrogen)

s %>% filter(grepl(x=variable, pattern="Primary Energy|Coal",fixed=T)) %>% variable_unique()
s.coal.uses <- s %>%
  filter(
    variable %in% c(
      "Primary Energy|Coal|Ammonia",
      "Primary Energy|Coal|Electricity",
      "Primary Energy|Coal|Gases",
      "Primary Energy|Coal|Heat",
      "Primary Energy|Coal|Hydrogen",
      "Primary Energy|Coal|Liquids",
      "Primary Energy|Coal|Solids"
    )
  ) #%>%
  # mutate_cond(
  #   variable%nin%c("Primary Energy|Coal|Liquids",
  #                  "Primary Energy|Coal|Electricity"),
  #   variable = "Primary Energy|Coal|Other"
  # ) %>%
  # reframe(
  #   value=sum(value), .by = c("model", "scenario", "region", "variable", "unit", "year")
  # )

p.coal.uses <- ggplot(
  s.coal.uses,
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s.coal, linetype="solid", label="Total") +
  theme_jsk() +
  ylab("EJ/yr")


p.coal.uses

save_ggplot(
  p = p.coal.uses,
  h = 500,
  w = 600,
  f = here("figures", "coal_aggregation_commodities")
)


### Emissions ------------------------------------------------------------------
target.emissions.set <- targets.emissions.timeseries.iamc %>%
  pull(variable) %>% unique() %>% sort()

s.emissions <- s %>% filter(variable%in%target.emissions.set)

s.emissions.with.targets <- s.emissions %>%
  add_target() %>%
  left_join(
    targets.emissions.timeseries.iamc %>% rename(target = scenario, target.value = value),
    by = join_by("year", "target", "variable")
  )

##### Visualise all emissions trajectories -------------------------------------
KEY.EMISSIONS <- c(
  "Emissions|CO2",
  "Emissions|CH4",
  "Emissions|Sulfur",
  "Emissions|N2O",
  "Emissions|BC",
  "Emissions|OC",
  "Emissions|CO2|AFOLU",
  "Emissions|CO2|Energy and Industrial Processes"
)

for (t in TARGETS %>% unique()){
  p.all.key.emissions <- ggplot(
    s.emissions.with.targets %>% filter(variable%in%KEY.EMISSIONS,
                                        target==t,
                                        region=="World") %>%
      iamc_variable_keep_one_level(level=-1),
    aes(x=year, colour=target)
  ) +
    facet_wrap(~variable, scales = "free_y") +
    mark_history() +
    geom_textpath(
      data = . %>% select(variable,target,year,target.value) %>% distinct(),
      aes(y = target.value,
          label = target,
          group = interaction(target, variable))
    ) +
    geom_textpath(
      aes(y=value, colour=target,
          label = scenario,
          group = interaction(scenario, variable)),
      linetype = "dashed"
    ) +
    theme_jsk() +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS))
  p.all.key.emissions

  save_ggplot(
    p = p.all.key.emissions,
    h = 200,
    w = 350,
    f = here("figures", paste0("key_emissions", "_", t))
  )


  p.all.emissions <- ggplot(
    s.emissions.with.targets %>% filter(#variable%in%KEY.EMISSIONS,
      target==t,
      region=="World") %>%
      iamc_variable_keep_one_level(level=-1),
    aes(x=year, colour=target)
  ) +
    facet_wrap(~variable, scales = "free_y") +
    mark_history() +
    geom_textpath(
      data = . %>% select(variable,target,year,target.value) %>% distinct(),
      aes(y = target.value,
          label = target,
          group = interaction(target, variable))
    ) +
    geom_textpath(
      aes(y=value, colour=target,
          label = scenario,
          group = interaction(scenario, variable)),
      linetype = "dashed"
    ) +
    theme_jsk() +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS))
  p.all.emissions

  save_ggplot(
    p = p.all.emissions,
    h = 200,
    w = 350,
    f = here("figures", paste0("all_emissions", "_", t))
  )
}




##### co2 ----------------------------------------------------------------------
s %>% filter(variable=="Emissions|CO2", region=="World")

# calculate differences

# relative differences

# absolute differences

# visualise pathways



##### bc and oc ----------------------------------------------------------------
s.bc <- s %>% filter(variable=="Emissions|BC", region=="World")
s.oc <- s %>% filter(variable=="Emissions|OC", region=="World")

test_that("BC not lower than pre-industrial",{
  for (s.h in SCENS){
    expect_gt(
      s.bc %>% filter(scenario==s.h) %>% pull(value) %>% min(),
      2.097770755, label = paste("BC gets lower than pre-industrial for scenario:", s.h, "\n")
    )
  }
})
test_that("OC not lower than pre-industrial",{
  for (s.h in SCENS){
    expect_gt(
      s.oc %>% filter(scenario==s.h) %>% pull(value) %>% min(),
      15.44766815, label = paste("OC gets lower than pre-industrial for scenario:", s.h, "\n")
    )
  }
})

# SIMPLE UNIT CHECKS -----------------------------------------------------------

### starting point all the same?
# tbd.


# IDENTIFIED ISSUES: ----------------------------------------------------------------------


### Non-energy use in total; aggregation issue ---------------------------------
vars.nonenergyaggregation <- c(
  # "Final Energy",
  "Final Energy|Transportation",
  "Final Energy|Residential and Commercial",
  "Final Energy|Industry"
  # "Final Energy|Non-Energy Use"
)

p.non.energy.aggregation <- ggplot(
  s %>% filter(variable%in%vars.nonenergyaggregation, region=="World"),
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s %>% filter(variable=="Final Energy",
                                  region=="World"), linetype="solid", label="Total") +
  theme_jsk() +
  # scale_y_continuous(transform = "log10") +
  ylab("EJ/yr")
p.non.energy.aggregation

save_ggplot(
  p = p.non.energy.aggregation,
  h = 200,
  w = 300,
  f = here("figures", "issue_nonenergyuse_aggregation_reporting")
)

### Final energy use total; aggregation issue of commodities -------------------
vars.finalenergy.aggregation <- c(
  "Final Energy|Electricity",
  "Final Energy|Gases",
  "Final Energy|Geothermal",
  "Final Energy|Heat",
  "Final Energy|Hydrogen",
  "Final Energy|Liquids",
  "Final Energy|Solar",
  "Final Energy|Solids"
)

p.final.energy.aggregation <- ggplot(
  s %>% filter(variable%in%vars.finalenergy.aggregation, region=="World"),
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s %>% filter(variable=="Final Energy",
                                  region=="World"), linetype="solid", label="Total") +
  theme_jsk() +
  # scale_y_continuous(transform = "log10") +
  ylab("EJ/yr")
p.final.energy.aggregation

save_ggplot(
  p = p.final.energy.aggregation,
  h = 200,
  w = 300,
  f = here("figures", "issue_finalenergycommodity_aggregation_reporting")
)

### Non-energy use gases jump --------------------------------------------------
vars.nonenergyjump <- c(
    # "Final Energy",
    "Final Energy|Gases",
    "Final Energy|Non-Energy Use",
    "Final Energy|Non-Energy Use|Gases",
    "Final Energy|Non-Energy Use|Solids",
    "Final Energy|Non-Energy Use|Liquids"
  )

p.non.energy.jump <- ggplot(
  s %>% filter(variable%in%vars.nonenergyjump),
  aes(x=year, colour=scenario)
) +
  facet_grid(region~variable, scales = "free_y") +
  mark_history() +
  geom_line(
    aes(y=value),
    data = s %>% filter(variable == "Final Energy") %>% select(-variable),
    linetype = "dotted"
  ) +
  geom_line(
    aes(y=value,
        group = interaction(scenario, variable))
  ) +
  theme_jsk() +
  labs(caption="Dotted = 'Final Energy' - total")
p.non.energy.jump

save_ggplot(
  p = p.non.energy.jump,
  h = 500,
  w = 600,
  f = here("figures", "issue_nonenergyuse_jump_accounting_reporting")
)


### Primary Energy|*|* aggregation issues --------------------------------------

##### Coal ---------------------------------------------------------------------
s.coal.uses <- s %>%
  filter(
    variable %in% c(
      "Primary Energy|Coal|Ammonia",
      "Primary Energy|Coal|Electricity",
      "Primary Energy|Coal|Gases",
      "Primary Energy|Coal|Heat",
      "Primary Energy|Coal|Hydrogen",
      "Primary Energy|Coal|Liquids",
      "Primary Energy|Coal|Solids"
    )
  )
s.coal.total <- s %>% filter(variable=="Primary Energy|Coal")

p.coal.uses <- ggplot(
  s.coal.uses,
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s.coal.total, linetype="solid", label="Total") +
  theme_jsk() +
  ylab("EJ/yr")


p.coal.uses

save_ggplot(
  p = p.coal.uses,
  h = 400,
  w = 300,
  f = here("figures", "coal_aggregation_commodities")
)

##### Gas ----------------------------------------------------------------------
s.gas.uses <- s %>%
  filter(
    variable %in% c(
      "Primary Energy|Gas|Ammonia",
      "Primary Energy|Gas|Electricity",
      "Primary Energy|Gas|Gases",
      "Primary Energy|Gas|Heat",
      "Primary Energy|Gas|Hydrogen",
      "Primary Energy|Gas|Liquids",
      "Primary Energy|Gas|Solids"
    )
  )
s.gas.total <- s %>% filter(variable=="Primary Energy|Gas")

p.gas.uses <- ggplot(
  s.gas.uses,
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s.gas.total, linetype="solid", label="Total") +
  theme_jsk() +
  ylab("EJ/yr")


p.gas.uses

save_ggplot(
  p = p.gas.uses,
  h = 400,
  w = 300,
  f = here("figures", "gas_aggregation_commodities")
)

##### Oil ----------------------------------------------------------------------
s.oil.uses <- s %>%
  filter(
    variable %in% c(
      "Primary Energy|Oil|Ammonia",
      "Primary Energy|Oil|Electricity",
      "Primary Energy|Oil|Gases",
      "Primary Energy|Oil|Heat",
      "Primary Energy|Oil|Hydrogen",
      "Primary Energy|Oil|Liquids",
      "Primary Energy|Oil|Solids"
    )
  )
s.oil.total <- s %>% filter(variable=="Primary Energy|Oil")

p.oil.uses <- ggplot(
  s.oil.uses,
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s.oil.total, linetype="solid", label="Total") +
  theme_jsk() +
  ylab("EJ/yr")


p.oil.uses

save_ggplot(
  p = p.oil.uses,
  h = 400,
  w = 300,
  f = here("figures", "oil_aggregation_commodities")
)

##### Total ----------------------------------------------------------------------
s.total.uses <- s %>%
  filter(
    variable %in% c(
      "Primary Energy|Biomass",
      "Primary Energy|Coal",
      "Primary Energy|Gas",
      "Primary Energy|Geothermal",
      "Primary Energy|Hydro",
      "Primary Energy|Nuclear",
      "Primary Energy|Ocean",
      "Primary Energy|Oil",
      "Primary Energy|Other",
      "Primary Energy|Secondary Energy Trade",
      "Primary Energy|Solar",
      "Primary Energy|Wind"
    )
  )
s.total.total <- s %>% filter(variable=="Primary Energy")

p.total.uses <- ggplot(
  s.total.uses,
  aes(x=year,y=value,fill=variable,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(region~scenario, scales = "free_y") +
  mark_history() +
  geom_area() +
  geom_textpath(data=s.total.total, linetype="solid", label="Total") +
  theme_jsk() +
  ylab("EJ/yr")


p.total.uses

save_ggplot(
  p = p.total.uses,
  h = 400,
  w = 300,
  f = here("figures", "total_aggregation_commodities")
)
