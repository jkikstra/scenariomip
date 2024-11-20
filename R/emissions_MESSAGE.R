# Do vetting checks for IAM scenarios - developed for ScenarioMIP, related to SSP update.
#' Produced by Jarmo Kikstra
#'
#'
#' Mainly for MESSAGEix usage.



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
SCENARIO_INPUT_FILE <- "scenarios_scenariomip_allmodels_20240919.csv"





# DOWNLOAD SCENARIO DATA -------------------------------------------------------
NEW.DOWNLOAD <- F

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
                       SCENARIO_INPUT_FILE)) # needs to be same name as the one above
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

# General filtering ------------------------------------------------------------
s <- s %>% filter(
  model=="MESSAGEix-GLOBIOM 2.1-M-R12"
)


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




### Assign targets to emissions pathways ---------------------------------------

target.emissions.set <- targets.emissions.timeseries.iamc %>%
  pull(variable) %>% unique() %>% sort()

s.emissions <- s %>% filter(variable%in%target.emissions.set)

s.emissions.with.targets <- s.emissions %>%
  add_target() %>%
  left_join(
    targets.emissions.timeseries.iamc %>% rename(target = scenario, target.value = value),
    by = join_by("year", "target", "variable")
  )

s.all.emissions.with.targets <- s %>%
  add_target() %>%
  left_join(
    targets.emissions.timeseries.iamc %>% rename(target = scenario, target.value = value),
    by = join_by("year", "target", "variable")
  )

# Select emissions to Visualise ------------------------------------------------
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
EMISSIONS.TO.VISUALISE <- s.emissions.with.targets %>% drop_na() %>% pull(variable) %>% unique()

# MESSAGE Emissions (Global) ---------------------------------------------------
target.emissions.set <- targets.emissions.timeseries.iamc %>%
  pull(variable) %>% unique() %>% sort()

s.emissions <- s %>% filter(variable%in%target.emissions.set)

world.em <- s.emissions %>%
  add_target() %>%
  simplify_model_names() %>%
  left_join(
    targets.emissions.timeseries.iamc %>% rename(target = scenario, target.value = value),
    by = join_by("year", "target", "variable")
  ) %>%
  filter(
    region=="World"
  )
world.em.co2.budgets <- world.em %>%
  filter(
    year %in% c(seq(2020,2060,5), seq(2070,2100,10)),
    region == "World",
    variable == "Emissions|CO2"
  ) %>%
  mutate(weight=NA) %>%
  mutate_cond(year==2020, weight=3.5) %>%
  mutate_cond(year%in%seq(2025,2055,5), weight=5) %>%
  mutate_cond(year==2060, weight=7.5) %>%
  mutate_cond(year%in%seq(2070,2090,10), weight=10) %>%
  mutate_cond(year==2100, weight=5) %>%
  reframe(
    co2budget=sum(value*weight),
    .by=c("model","scenario","region", "variable", "unit")
  )
world.em <- world.em %>%
  left_join(world.em.co2.budgets) %>%
  mutate(scenario=paste(scenario,
                        as.character(
                          round(co2budget/1e4)*10
                        ),
                        "Gt"
  ))

### Main plot ------------------------------------------------------------------
for(t in
    (
      world.em %>% pull(target) %>% unique()
    )
){

  print(t)

  df.em <- world.em %>% filter(variable=="Emissions|CO2",
                               model=="MESSAGE",
                               year>=2015,year<=2100) %>%
    mutate(value=value/1e3,target.value=target.value/1e3)
  p.M.target.main <- ggplot(
    df.em,
    aes(x=year)
  ) +
    mark_history() +
    geom_hline(yintercept = 0) +
    geom_line(
      data = . %>% filter(target%nin%t),
      aes(y=target.value,
          group=interaction(model,scenario,variable)),
      alpha=0.1,
    ) +
    geom_line(
      data = . %>% filter(target%in%t),
      aes(y=value,
          linetype=scenario,
          colour=target,
          # label=substr(scenario,1,4),
          group=interaction(model,scenario,variable)),
      linewidth=1.3
    ) +
    geom_line(
      data = . %>% filter(target%nin%t),
      linetype="solid",
      aes(y=target.value,
          group=interaction(scenario,variable)),
      alpha=0.2,
      linewidth=0.7
    ) +
    geom_textpath(
      data = . %>% filter(target%in%t),
      linetype="solid",
      aes(y=target.value,
          group=interaction(scenario,variable)),
      label = "Ref",
      hjust=0.9,
      linewidth=1.1
    ) +
    theme_jsk() +
    ylab(expression("Gt"~ CO[2] ~ "/" ~ yr)) +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    scale_x_continuous(
      expand = c(0,0)
    ) +
    guides(
      label = "none",
      colour = "none",
      linetype = guide_legend(title="")
    ) +
    labs(
      title=bquote(.("Scenario:") ~ bold(.(t)) ),
      caption="Budgets: ENGAGE-style accounting"
    ) +
    scale_y_continuous(breaks = seq(-25,75,20)) +
    theme(
      plot.margin = unit(c(1, 2, 1, 1), "lines")  # Add extra space to the right
    )

  save_ggplot(
    p = p.M.target.main,
    h = 200,
    w = 200,
    f = here("figures", "scenariomip-MESSAGE", paste0("main_emissions_co2_",t)  )
  )
}
for (e in EMISSIONS.TO.VISUALISE){
  print(e)

  df.em <- world.em %>% filter(variable==e,
                               model=="MESSAGE",
                               year>=2015,year<=2100) %>%
    mutate(ssp=substr(scenario,1,4))
  unit.em <- df.em %>% pull(unit) %>% unique()
  label.em <- df.em %>% iamc_variable_keep_one_level(level=-1) %>% pull(variable) %>% unique()

  p.M.main.allscens <- ggplot(
    df.em,
    aes(x=year)
  ) +
    mark_history() +
    geom_hline(yintercept = 0) +
    geom_line(
      aes(y=value,
          colour=target,
          group=interaction(model,scenario,variable)),
      linewidth=0.8
    ) +
    geom_point(
      aes(y=value,
          colour=target,
          shape=ssp),
      size=2
    ) +
    theme_jsk() +
    ylab(unit.em) +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    scale_x_continuous(
      expand = c(0,0)
    ) +
    guides(
      label = "none",
      colour = "none",
      shape = guide_legend(title="")
    ) +
    labs(
      title = bquote(.("Scenario:") ~ bold(.("all")) ),
      subtitle = e
    ) +
    # scale_y_continuous(breaks = seq(-25,75,20)) +
    theme(
      plot.margin = unit(c(1, 2, 1, 1), "lines")  # Add extra space to the right
    )

  save_ggplot(
    p = p.M.main.allscens,
    h = 200,
    w = 200,
    f = here("figures", "scenariomip-MESSAGE", paste0("main_emissions_allscenarios_", label.em)  )
  )
}


### Small alignment plots ------------------------------------------------------
for (m in (world.em %>% pull(model) %>% unique()) ){
  for (e in c(
    EMISSIONS.TO.VISUALISE
  )) {
    if (
      nrow(
        world.em %>% filter(variable==e,
                            model==m)
      ) != 0
    ){
      print(e)

      df.em <- world.em %>% filter(variable==e,
                                   model==m)
      label.em <- df.em %>% iamc_variable_keep_one_level(level=-1) %>% pull(variable) %>% unique()

      p.mini.ali <- ggplot(
        df.em,
        aes(x=year)
      ) +
        facet_wrap(~target, ncol=3) +
        mark_history() +
        geom_line(
          linetype="dashed",
          aes(y=value,
              group=interaction(model,scenario,variable))
        ) +
        geom_line(
          linetype="solid",
          aes(y=target.value,
              group=interaction(scenario,variable))
        ) +
        theme_void() +
        theme(panel.background = element_rect(colour = "grey", linewidth = 0.2))
      p.mini.ali

      save_ggplot(
        p = p.mini.ali,
        h = 75,
        w = 100,
        f = here("figures", paste0("scenariomip-",m), paste0("alignment_emissions", "_", label.em ))
      )
    }
  }
}















# Very low scenarios plots -----------------------------------------------------

vl.em <- s.all.emissions.with.targets %>% filter(target=="VL")
vl.em.w <- vl.em %>% filter(region=="World")

### Humpenoeder emissions ------------------------------------------------------
hump.em.sust <- read_csv(file = "C:\\Users\\kikstra\\OneDrive - IIASA\\_Other\\ClimateAssessmentRun2023 - Carl-LandUse\\run_20230620\\input\\carl_landuse_15_v2.csv") %>%
  filter(
    model == "MESSAGEix-GLOBIOM 1.0",
    scenario == "LowEnergyDemand_1.3_IPCC-Global-Sustainability"
  ) %>%
  filter(
    variable %in% c("Emissions|CH4|AFOLU")
  ) %>%
  iamc_wide_to_long()

p.em.afolu <- ggplot(data=vl.em.w %>% filter(
  variable %in% c("Emissions|CO2|AFOLU","Emissions|CH4|AFOLU")
),
aes(x=year, group=interaction(model,scenario,region,variable),
    colour=scenario)) +
  facet_grid(variable~., scales="free_y")+
  mark_history() +
  geom_line(
    aes(y=value),
  ) +
  geom_line(
    data=. %>% mutate(scenario="Sanderson/Smith target"),
    aes(y=target.value),
  ) +
  geom_line(
    data=hump.em.sust %>% mutate(scenario="Humpenoder - sustainability"),
    aes(y=value)
  ) +
  theme_jsk()
p.em.afolu


save_ggplot(
  p = p.em.afolu,
  h = 160,
  w = 250,
  f = here("figures", paste0("scenariomip-","MESSAGE"), paste0("VL_ch4_afolu" ))
)

### Net-zero emissions ---------------------------------------------------------

s.emissions.with.targets %>%
  filter(variable=="Emissions|CO2", region=="World") %>%
  mutate(
    zero = ifelse(value<=0,"yes", "no")
  ) %>%
  group_by(model,scenario,target) %>%
  filter(zero=="yes") %>%
  summarise(first.negative.year=first(year)) %>% arrange(first.negative.year)

### Carbon Sequestration -------------------------------------------------------

s.seq <- s %>% filter_starts_with("Carbon Removal")
s.seq %>% distinct(variable)
s.seq.w <- s.seq %>% filter(region=="World")
s.seq.w.vl <- s.seq.w %>% filter(grepl(x=scenario, pattern="Very Low", fixed=T))

p.em.cdr <- ggplot(data=s.seq.w.vl,
aes(x=year, group=interaction(model,scenario,region,variable),
    fill=variable)) +
  facet_grid(.~scenario, scales="free_y")+
  mark_history() +
  geom_area(
    data=. %>%filter(
      variable %in% c("Carbon Removal|Bioenergy with CCS",
                       "Carbon Removal|Direct Air Capture with CCS",
                       "Carbon Removal|Forestry")
    ),
    aes(y=value)
  ) +
  geom_textpath(
    data=. %>% filter(variable=="Carbon Removal"),
    aes(y=value),
    label="Total"
  ) +
  theme_jsk() +
  guides(fill=guide_legend(title=NULL))
p.em.cdr


save_ggplot(
  p = p.em.cdr,
  h = 160,
  w = 250,
  f = here("figures", paste0("scenariomip-","MESSAGE"), paste0("VL_cdr" ))
)


### CO2 AFOLU ------------------------------------------------------------------
smessagereporting <- vroom(here("data","scenarios_message.csv")) %>%
  iamc_wide_to_long(upper.to.lower = T)
s.co2afolu <- smessagereporting %>% filter_starts_with("Emissions|CO2|AFOLU")
s.co2afolu %>% distinct(variable)
s.co2afolu.w <- s.co2afolu %>% filter(region=="World")
s.co2afolu.w.vl <- s.co2afolu.w %>% filter(grepl(x=scenario, pattern="Very Low", fixed=T))

p.em.co2afolu <- ggplot(data=s.co2afolu.w.vl %>% #filter(variable!="Emissions|CO2|AFOLU") %>%
                          left_join(
                            s.co2afolu.w.vl %>% filter(variable=="Emissions|CO2|AFOLU") %>% select(-variable) %>% rename(total=value)
                          ),
                   aes(x=year, group=interaction(model,scenario,region,variable),
                       fill=variable)) +
  facet_wrap(.~variable, nrow=2)+
  mark_history() +
  geom_area(
    data=. %>%filter(
      # variable %nin% c("Emissions|CO2|AFOLU")
    ),
    aes(y=value)
  ) +
  geom_textpath(
    aes(y=total),
    label="Total"
  ) +
  geom_texthline(
    data=s.co2afolu.w.vl %>% filter(
      variable %in% c("Emissions|CO2|AFOLU|Afforestation")
    ) %>% mutate(yi=-3800),
    aes(yintercept = yi),
    linetype = "dashed",
    label = "Limit",
    hjust = 0.1
  ) +
  geom_texthline(
    data=s.co2afolu.w.vl %>% filter(
      variable %in% c("Emissions|CO2|AFOLU")
    ) %>% mutate(yi=-5100),
    aes(yintercept = yi),
    linetype = "dashed",
    label = "Limit",
    hjust = 0.1
  ) +
  theme_jsk() +
  guides(fill="none") +
  labs(caption="Limit: medium risk Dezprez2024, 3.8 (reforestation) and 5.1 (nature-based).")
p.em.co2afolu


save_ggplot(
  p = p.em.co2afolu,
  h = 200,
  w = 300,
  f = here("figures", paste0("scenariomip-","MESSAGE"), paste0("VL_co2afolu" ))
)


### kcal livestock -------------------------------------------------------------
s.diets <- smessagereporting %>%
  filter(variable=="Food Demand|Livestock")
s.diets.vl <- s.diets %>% filter(grepl(x=scenario, pattern="Very Low", fixed=T))
s.diets.w <- s.diets %>% filter(region=="World")

p.diets <- ggplot(s.diets.vl,
                  aes(x=year,y=value)) +
  facet_wrap(~region, scales="free_x") +
  mark_history() +
  geom_line(aes(colour=region)) +
  theme_jsk() +
  guides(colour="none")

p.diets

save_ggplot(
  p = p.diets,
  h = 160,
  w = 250,
  f = here("figures", paste0("scenariomip-","MESSAGE"), paste0("VL_diets_livestockcalories" ))
)


### Final Energy ---------------------------------------------------------------
s.fe <- s %>% filter(variable%in%c("Final Energy",
                                   "Final Energy|Residential and Commercial",
                                   "Final Energy|Industry",
                                   "Final Energy|Transportation"))
s.fe.vl <- s.fe %>% filter(grepl(x=scenario, pattern="Very Low", fixed=T))
s.fe.vl.w <- s.fe.vl %>% filter(region=="World")

p.fe <- ggplot(s.fe.vl.w,
               aes(x=year,y=value),
               group=interaction(model,scenario,variable,region)) +
  facet_wrap(~scenario, scales="free_x") +
  mark_history() +
  geom_textpath(data=. %>% filter(variable=="Final Energy"),
            label = "total") +
  geom_area(data=. %>% filter(variable!="Final Energy"),
            aes(fill=variable)) +
  theme_jsk() +
  guides(colour="none")

p.fe

save_ggplot(
  p = p.fe,
  h = 160,
  w = 250,
  f = here("figures", paste0("scenariomip-","MESSAGE"), paste0("VL_finalenergy" ))
)

p.fe.region <- ggplot(s.fe.vl %>% filter(grepl(x=region,pattern="(R5)",fixed=T)),
               aes(x=year,y=value),
               group=interaction(model,scenario,variable,region)) +
  facet_grid(scenario~region, scales="free_y") +
  mark_history() +
  geom_textpath(data=. %>% filter(variable=="Final Energy"),
                label = "total") +
  geom_area(data=. %>% filter(variable!="Final Energy"),
            aes(fill=variable)) +
  theme_jsk() +
  guides(colour="none")

p.fe.region

save_ggplot(
  p = p.fe.region,
  h = 160,
  w = 250,
  f = here("figures", paste0("scenariomip-","MESSAGE"), paste0("VL_finalenergy_region" ))
)
