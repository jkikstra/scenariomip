# Do vetting checks for IAM scenarios - developed for ScenarioMIP, related to SSP update.
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


# Temperature ------------------------------------------------------------------
s.temp50 <- s %>% filter(variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile")

library(ggrepel)

### Trajectories ---------------------------------------------------------------
# # Define a function to safely add geom_text_repel() only when data is present
# safe_geom_text_repel <- function(data, ...) {
#   if (nrow(data) == 0) {
#     return()  # Return NULL if the data subset is empty
#   } else {
#     return(geom_text_repel(data = data, ...))  # Apply normally if data exists
#   }
# }

p.temps <- ggplot(
  s.temp50 %>%
    add_target() %>% filter(!is.na(target), !is.na(value)) %>%
    simplify_model_names() %>% #filter(model=="MESSAGE") %>%
    mutate(ssp=substr(scenario,1,4)),
  aes(x=year,y=value,colour=target,
      group=interaction(model,scenario,region,variable))
) +
  facet_grid(model~target, scales = "free") +
  mark_history() +
  geom_texthline(yintercept = 1.5, label="1.5C", linetype="dashed", colour="black",
                 hjust=0.05) +
  # geom_textpath(aes(label=scenario), linetype="solid", size = 3,
  #               hjust=0.9) +
  geom_line(
    aes(y=value,
        colour=target,
        linetype=ssp,
        # label=substr(scenario,1,4),
        group=interaction(model,scenario,variable)),
    linewidth=0.5
  ) +
  geom_point(
    data=. %>% filter(year%in%seq(1990,2100,10)),
    aes(y=value,
        colour=target,
        shape=ssp),
    size=2
  ) +
  # safe_geom_text_repel(data=. %>% filter(year==2100) %>% group_by(model,target) %>% filter(n() > 0),  # Filter only non-empty groups dynamically
  #                 na.rm = TRUE,                                        # Removes missing values
  #                 aes(label=round(value,digits = 1),
  #                     colour=target),
  #           segment.square  = T,
  #           segment.inflect = T,
  #           force             = 0.5,
  #           nudge_x           = 0.15,
  #           direction         = "y",
  #           hjust             = 0,
  #           segment.size      = 0.2,
  #           segment.curvature = -0.1) +
  geom_text(data=. %>% filter(year==2100),
                       aes(label=round(value,digits = 1),
                           colour=target,
                           x=2110)) +
  # xlim(2000,2115) +
  theme_jsk() +
  ylab("C above pre-industrial") +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS))
p.temps
save_ggplot(
  p = p.temps,
  h = 400,
  w = 450,
  f = here("figures", "scenariomip-allteams", "temps")
)

### Peak of p50 ----------------------------------------------------------------

##### By model and SSP ---------------------------------------------------------
s.temp50 %>%
  add_target() %>% filter(!is.na(target)) %>%
  simplify_model_names() %>%
  mutate(ssp=substr(scenario,1,4)) %>%
  filter(
    target %in% c("VL", "LOS", "L")
  ) %>%
  reframe(
    peakT = max(value),
    .by = c("model", "ssp", "target")
  ) %>%
  pivot_wider(names_from = ssp, values_from = peakT) %>%
  write_delim(
    x=.,
    file = here("output-other", "peakT_lowscenarios.csv"),
    delim = ","
  )


##### Cross-model-SSP range ----------------------------------------------------
s.temp50 %>%
  add_target() %>% filter(!is.na(target)) %>%
  simplify_model_names() %>%
  mutate(ssp=substr(scenario,1,4)) %>%
  filter(
    target %in% c("VL", "LOS", "L")
  ) %>%
  reframe(
    peakT = max(value),
    .by = c("model", "ssp", "target")
  ) %>%
  reframe(
    `Range of peak Temp options` = paste0(
      min(peakT, na.rm = T) %>% round(digits = 2) %>% sprintf("%.2f",.) %>% as.character(),
      "-",
      max(peakT, na.rm = T) %>% round(digits = 2) %>% sprintf("%.2f",.) %>% as.character()
    ),
    model="Across models and SSPs",
    .by = c("target")
  ) %>%
  write_delim(
    x=.,
    file = here("output-other", "peakT_lowscenarios_acrossoptions.csv"),
    delim = ","
  )



# Emissions --------------------------------------------------------------------
target.emissions.set <- targets.emissions.timeseries.iamc %>%
  pull(variable) %>% unique() %>% sort()

s.emissions <- s %>% filter(variable%in%target.emissions.set)

s.emissions.with.targets <- s.emissions %>%
  add_target() %>%
  left_join(
    targets.emissions.timeseries.iamc %>% rename(target = scenario, target.value = value),
    by = join_by("year", "target", "variable")
  )

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

ppt <- read_pptx()
for (t in TARGETS %>% unique()){
  p.all.key.emissions <- ggplot(
    s.emissions.with.targets %>% filter(variable%in%KEY.EMISSIONS,
                                        target==t,
                                        year>=2015,year<=2100,
                                        region=="World") %>%
      iamc_variable_keep_one_level(level=-1) %>%
      simplify_model_names(),
    aes(x=year)
  ) +
    facet_wrap(~variable, scales = "free_y") +
    mark_history() +
    geom_textpath(
      data = . %>% select(variable,target,year,target.value) %>% distinct(),
      aes(y = target.value,
          label = target,
          group = interaction(target, variable))
    ) +
    geom_line(
      aes(y=value, colour=model,
          group = interaction(model,scenario, variable))
    ) +
    geom_point(
      aes(y=value, colour=model, shape=scenario)
    ) +
    theme_jsk() +
    theme(
      legend.position = c(.95, .05),
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.box = "horizontal",
      legend.margin = margin(6, 6, 6, 6),
      legend.byrow = TRUE
    )
  # p.all.key.emissions

  # save_ggplot(
  #   p = p.all.key.emissions,
  #   h = 200,
  #   w = 350,
  #   f = here("figures", "scenariomip-allteams", paste0("key_emissions", "_", t))
  # )

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
          linetype = model,
          group = interaction(scenario, variable)),
      linetype = "dashed"
    ) +
    theme_jsk() +
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS))
  # p.all.emissions

  # save_ggplot(
  #   p = p.all.emissions,
  #   h = 200,
  #   w = 350,
  #   f = here("figures", "scenariomip-allteams", paste0("all_emissions", "_", t))
  # )

  # Add a section title slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
  ppt <- ph_with(ppt, value = paste("Target:", t), location = ph_location_type(type = "ctrTitle"))
  for (
    e in c(
      s.emissions.with.targets %>% pull(variable) %>% unique()
    )
  ) {
    p.data.e <- s.emissions.with.targets %>% filter(
      variable==e,
      target==t,
      region=="World") %>%
      iamc_variable_keep_one_level(level=-1) %>%
      simplify_model_names() %>%
      mutate(ssp=substr(scenario,1,4))
    p.all.emissions.e <- ggplot(
      p.data.e,
      aes(x=year)
    ) +
      # facet_wrap(~variable, scales = "free_y") +
      mark_history() +
      geom_textpath(
        data = . %>% filter(year>=2015) %>% select(variable,target,year,target.value) %>% distinct(),
        aes(y = target.value,
            label = target,
            group = interaction(target, variable))
      ) +
      geom_line(
        aes(y=value,
            #colour=target,
            colour=model,
            # label = scenario,
            # linetype = model,
            group = interaction(model, scenario, variable)),
        linetype = "solid",
        linewidth = 0.5
      ) +
      geom_point(
        data=. %>% filter(year%in%seq(2020,2100,10)),
        aes(y=value, shape=ssp, colour=model)
      )+
      theme_jsk() +
      labs(title=e) +
      ylab(
        p.data.e %>% pull(unit) %>% unique()
      ) +
      guides(color = guide_legend(ncol = 2),
             shape = guide_legend(ncol = 2)) +  # Set legend with 2 columns
      theme(legend.position = "bottom")         # Optional: place legend at the bottom
    p.all.emissions.e

    # create powerpoint slides
    # Create a temporary file to save the plot
    temp_file <- tempfile(fileext = ".png")

    # Save the plot as a PNG file
    ggsave(temp_file, plot = p.all.emissions.e, width = 6, height = 5)

    # Add a new slide to the PowerPoint
    ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

    # Add the plot image to the slide
    ppt <- ph_with(ppt, external_img(temp_file), location = ph_location_fullsize())
  }
}
print(ppt, target = here("figures","scenariomip-allteams","all_overview_per_target_emissions.pptx"))





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
world.em.scenwbudgets <- world.em %>%
  left_join(world.em.co2.budgets) %>%
  mutate(scenario=paste(scenario,
                        as.character(
                          round(co2budget/1e4)*10
                        ),
                        "Gt"
                        ))

### Main plot ------------------------------------------------------------------
for(t in
    c(
      TARGETS %>% unique()#,
      # list("M","ML"),
      # list(TARGETS %>% unique())
    )
    ){

  print(t)

  p.M.target.main <- ggplot(
    world.em %>% filter(variable=="Emissions|CO2",
                        model=="MESSAGE",
                        year>=2015,year<=2100) %>%
      mutate(value=value/1e3,target.value=target.value/1e3),
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
    f = here("figures", "scenariomip-MESSAGE", paste0("main_emissions_",t)  )
  )
}

p.M.main.allscens <- ggplot(
  world.em %>% filter(variable=="Emissions|CO2",
                      model=="MESSAGE",
                      year>=2015,year<=2100) %>%
    mutate(value=value/1e3,target.value=target.value/1e3) %>%
    mutate(ssp=substr(scenario,1,4)),
  aes(x=year)
) +
  mark_history() +
  geom_hline(yintercept = 0) +
  geom_line(
    aes(y=value,
        colour=target,
        # label=substr(scenario,1,4),
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
  ylab(expression("Gt"~ CO[2] ~ "/" ~ yr)) +
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
    title=bquote(.("Scenario:") ~ bold(.("all")) ),
    caption="Budgets: ENGAGE-style accounting"
  ) +
  scale_y_continuous(breaks = seq(-25,75,20)) +
  theme(
    plot.margin = unit(c(1, 2, 1, 1), "lines")  # Add extra space to the right
  )

save_ggplot(
  p = p.M.main.allscens,
  h = 200,
  w = 200,
  f = here("figures", "scenariomip-MESSAGE", paste0("main_emissions_allscenarios")  )
)

### Small alignment plots ------------------------------------------------------
ppt <- read_pptx()
for (m in (world.em %>% pull(model) %>% unique()) ){

  # Add a section title slide
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
  ppt <- ph_with(ppt, value = paste("Model:", m), location = ph_location_type(type = "ctrTitle"))
  ppt <- ph_with(ppt, value = c("Scenarios:\n", paste(world.em %>%
                                                        mutate(st = paste0("Target:", target, ". ", scenario)) %>%
                                      filter(model==m) %>% pull(st) %>% unique() %>% sort(), sep = "/n")
                                    ), location = ph_location_type(type = "subTitle"))

  for (e in c(
    # "Emissions|CO2",
    # "Emissions|CH4",
    # "Emissions|Sulfur"
    s.emissions.with.targets %>% pull(variable) %>% unique()
  )) {
    if (
      nrow(
        world.em %>% filter(variable==e,
                            model==m)
      ) != 0
    ){

      p.mini.ali <- ggplot(
        world.em %>% filter(variable==e,
                            model==m),
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
        theme(panel.background = element_rect(colour = "grey", linewidth = 0.2)) +
        labs(title = e, subtitle = m)
      p.mini.ali




      # plot as separate plots
      save.string <- here("figures", paste0("scenariomip-",m), paste0("alignment_emissions", "_", gsub("\\|", "_", substr(e,nchar("Emissions|")+1,nchar(e)) ) ))
      # save_ggplot(
      #   p = p.mini.ali,
      #   h = 75,
      #   w = 100,
      #   f = save.string
      # )

      # create powerpoint slides
      # Create a temporary file to save the plot
      temp_file <- tempfile(fileext = ".png")

      # Save the plot as a PNG file
      ggsave(temp_file, plot = p.mini.ali, width = 6, height = 5)

      # Add a new slide to the PowerPoint
      ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

      # Add the plot image to the slide
      ppt <- ph_with(ppt, external_img(temp_file), location = ph_location_fullsize())

    }
  }
}
print(ppt, target = here("figures","scenariomip-allteams","all_minialignmentplots.pptx"))



# Harmonization emissions analysis ---------------------------------------------
s.em.harm <- s %>%
  filter(region=="World") %>%
  filter(grepl(x=variable,pattern="Emissions",fixed=T)) %>%
  add_emissions_processing_col() %>%
  filter(emissions.step%in%c("Native", "Harmonized")) %>%
  pivot_wider(
    names_from = emissions.step,
    values_from = value
  ) %>%
  add_target() %>%
  mutate(
    ssp = substr(scenario,1,4)
  )

##### CO2 AFOLU ----------------------------------------------------------------
ggplot(
  s.em.harm %>%
    filter(variable%in%c(
      "CO2|AFOLU",
      "CO2|Energy and Industrial Processes"
    )) %>%
    filter(!is.na(Harmonized-Native)),
  aes(x=year,y=Harmonized-Native,
      group=interaction(model,scenario,region,variable,unit))
) +
  facet_grid(model~target) +
  geom_line(
    aes(linetype=ssp,
        colour=variable),
    linewidth=1.2
  )


s.em.harm %>%
  filter(variable%in%c(
    # "CO2|AFOLU",
    # "CO2|Energy and Industrial Processes",
    "CO2|Energy",
    "CO2|Industrial Processes",
    "CO2|Other",
    "CO2|Waste"
  )) %>%
  filter(target=="LOS",
         year==2100) %>% View()
