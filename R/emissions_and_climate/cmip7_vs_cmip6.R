#' Code for comparing CMIP6 and CMIP7 markers ScenarioMIP
#' Developed by Jarmo Kikstra


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

# Ten core species ----
the.ten <- c("CO2", "N2O", "BC", "OC", "CH4", "NH3", "Sulfur", "VOC", "NOx", "CO")

# Load data ----
## cmip6 scenarios ----
cmip6.scenarios <- read_csv(
  "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/IAM Files from CMIP6/SSP_CMIP6_201811.csv"
) %>% allcaps_to_lower() %>% iamc_wide_to_long()
cmip6.scenarios.global <- cmip6.scenarios %>% filter(region=="World") |>
  iamc_variable_keep_two_levels(level = c(2,3)) |>
  filter(variable%in%the.ten) |> # distinct() |>
  mutate_cond(variable=="VOC", variable="NMVOC") %>%
  mutate_cond(variable=="Sulfur", variable="SO2")  %>%
  mutate_cond(variable=="NOx", unit="Mt NO2/yr") %>%
  mutate_cond(variable=="CO2", value=value/1e3) %>%
  mutate_cond(variable=="CO2", unit="Gt CO2/yr") %>%
  filter(
    scenario %in% c(
      "SSP1-19",
      "SSP1-26",
      "SSP2-45",
      "SSP3-70 (Baseline)",
      "SSP5-85 (Baseline)"
    )
  ) |>
  mutate_cond(scenario=="SSP3-70 (Baseline)",scenario="SSP3-70") |>
  mutate_cond(scenario=="SSP5-85 (Baseline)",scenario="SSP5-85")

cmip6.scenarios.global |> distinct(variable,unit)
cmip6.scenarios.global |> distinct(scenario)

## cmip7 scenarios ----

rename_cmip7_scenarios <- function(df){
  df %>%
    mutate_cond(scenario=="High - SSP3 (Marker)", scenario="H") %>%
    mutate_cond(scenario=="High-to-Low - SSP5 (Marker)", scenario="HL") %>%
    mutate_cond(scenario=="Medium - SSP2 (Marker)", scenario="M") %>%
    mutate_cond(scenario=="Medium-to-Low - SSP2 (Marker)", scenario="ML") %>%
    mutate_cond(scenario=="Low - SSP2 (Marker)", scenario="L") %>%
    mutate_cond(scenario=="Low-to-Negative - SSP2 (Marker)", scenario="LN") %>%
    mutate_cond(scenario=="Very Low - SSP1 (Marker)", scenario="VL")
  # ,"High - SSP3 (Marker)"
  # ,"High-to-Low - SSP5 (Marker)"
  # ,"Medium - SSP2 (Marker)"
  # ,"Medium-to-Low - SSP2 (Marker)"
  # ,"Low - SSP2 (Marker)"
  # ,"Low-to-Negative - SSP2 (Marker)"
  # ,"Very Low - SSP1 (Marker)"
}

### Zenodo dataset ----
cmip7.scenarios.global <- read_excel(
  "C:\\Users\\kikstra\\OneDrive - IIASA\\_Other\\Data\\Scenario data\\Scenario Databases\\ScenarioMIP-CMIP7\\ScenarioMIP_emissions_marker_scenarios_v0.1.xlsx",
  sheet = "data"
) %>% iamc_wide_to_long() |>
  iamc_variable_keep_one_level(level=-1) |>
  filter(variable%in%the.ten) |>
  filter(unit!="Gt CO2") |>
  mutate_cond(variable=="VOC", variable="NMVOC") %>%
  mutate_cond(variable=="Sulfur", variable="SO2") %>%
  mutate_cond(variable=="CO2", value=value/1e3) %>%
  mutate_cond(variable=="CO2", unit="Gt CO2/yr")
cmip7.scenarios.global |> distinct(variable,unit)
cmip7.scenarios.global |> distinct(scenario)

### add ML ----
ml <- read_csv(
  "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/ScenarioMIP Emulator workflow/08 March 2026 (COFFEE)/emissions/COFFEE 1.6/infilled_COFFEE 1.6.csv"
) %>% iamc_wide_to_long()
cmip7.scenarios.global.ml <- ml %>%
  iamc_variable_keep_one_level(level=-1) |>
  filter(variable%in%the.ten) |>
  filter(unit!="Gt CO2") |>
  mutate_cond(variable=="VOC", variable="NMVOC") %>%
  mutate_cond(variable=="Sulfur", variable="SO2") %>%
  mutate_cond(variable=="CO2", value=value/1e3) %>%
  mutate_cond(variable=="CO2", unit="Gt CO2/yr")

cmip7.scenarios.global <- cmip7.scenarios.global %>%
  bind_rows(
    cmip7.scenarios.global.ml %>% mutate(scenario = "Medium-to-Low - SSP2 (Marker)")
  ) %>%
  distinct() %>%
  rename_cmip7_scenarios()


## cmip7 history ----
cmip7.history <- read_csv(
  "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Emissions data/cmip7/global-workflow-history.csv"
) %>% iamc_wide_to_long() |>
  mutate_cond(variable%in%c("Emissions|CO2|AFOLU", "Emissions|CO2|Energy and Industrial Processes"),
              variable="Emissions|CO2") %>%
  iamc_variable_keep_one_level(level=-1) |>
  reframe(value=sum(value),.by=c(scenario,region,variable,unit,year)) %>% # sum CO2
  filter(variable%in%the.ten) %>%
  mutate_cond(variable=="VOC", variable="NMVOC") %>%
  mutate_cond(variable=="Sulfur", variable="SO2") %>%
  mutate_cond(variable=="CO2", value=value/1e3) %>%
  mutate_cond(variable=="CO2", unit="Gt CO2/yr")
cmip7.history |> distinct(variable,unit)




# |||| ----
# Plotting styles ----
# |||| ----

SCENARIOS.6 <- c(
  # short letter naming
  "SSP5-85"
  ,"SSP3-70"
  ,"SSP2-45"
  ,"SSP1-26"
  ,"SSP1-19"
)
SCENARIOS.6.COLOURS <- c(
  '#800000', # SSP5-85
  '#ff0000', # SSP3-70
  # '#c87820', #
  '#d3a640', # SSP2-45
  # '#098740', # L
  '#0080d0', # SSP1-26
  '#100060' # SSP1-19
)
names(SCENARIOS.6.COLOURS) <- SCENARIOS.6

SCENARIOS.7 <- c(
  # short letter naming
  "H"
  ,"HL"
  ,"M"
  ,"ML"
  ,"L"
  ,"LN"
  ,"VL"

  ,"High - SSP3 (Marker)"
  ,"High-to-Low - SSP5 (Marker)"
  ,"Medium - SSP2 (Marker)"
  ,"Medium-to-Low - SSP2 (Marker)"
  ,"Low - SSP2 (Marker)"
  ,"Low-to-Negative - SSP2 (Marker)"
  ,"Very Low - SSP1 (Marker)"

  ,"High"
  ,"High-to-Low"
  ,"Medium"
  ,"Medium-to-Low"
  ,"Low"
  ,"Low-to-Negative"
  ,"Very Low"


)
SCENARIOS.7.COLOURS <- c(
  '#800000', # H
  '#ff0000', # HL
  '#c87820', # M
  '#d3a640', # ML
  '#098740', # L
  '#0080d0', # LN
  '#100060', # VL

  '#800000', # H
  '#ff0000', # HL
  '#c87820', # M
  '#d3a640', # ML
  '#098740', # L
  '#0080d0', # LN
  '#100060', # VL

  '#800000', # H
  '#ff0000', # HL
  '#c87820', # M
  '#d3a640', # ML
  '#098740', # L
  '#0080d0', # LN
  '#100060' # VL
)
names(SCENARIOS.7.COLOURS) <- SCENARIOS.7

# |||| ----
# Plots ----
# |||| ----

add_facet_label <- function(df){
  df %>%
    mutate(facet_label = paste0(variable, "\n(", unit, ")"))
}



# CMIP7 vs CMIP6 ----

## CMIP6 only ----
p.global.cmip6.co2 <- ggplot(
  cmip6.scenarios.global |> filter(variable=="CO2") |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario))
  ) +
  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none")

p.global.cmip6.co2

p.global.cmip6.nonco2ghg <- ggplot(
  cmip6.scenarios.global |> filter(variable%in%c("CH4", "N2O")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario))
  ) +
  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none")

p.global.cmip6.nonco2ghg

p.global.cmip6.slcfs <- ggplot(
  cmip6.scenarios.global |> filter(variable%nin%c("CO2", "CH4", "N2O")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", ncol = 1, strip.position = "right") +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario))
  ) +
  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.text.y.right = element_text(angle = 0))

p.global.cmip6.slcfs


p.global.cmip6 <- (p.global.cmip6.co2 + p.global.cmip6.nonco2ghg + p.global.cmip6.slcfs) +
  plot_layout(
    design = "
    AAC
    AAC
    BBC
    "
  )

p.global.cmip6

save_ggplot(
  p = p.global.cmip6,
  f = here("figures", "cmip6_withouthistory"),
  h = 150, w = 250
)


## Updated history vs CMIP6 ----

p.global.cmip6.co2.whistory <- ggplot(
  cmip6.scenarios.global |> filter(variable=="CO2") |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario))
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="CO2", year>=1990),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none")

p.global.cmip6.co2.whistory

p.global.cmip6.nonco2ghg.whistory <- ggplot(
  cmip6.scenarios.global |> filter(variable%in%c("CH4", "N2O")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario))
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable%in%c("CH4", "N2O"), year>=1990) |>
      add_facet_label(),
    aes(group = interaction(scenario,variable)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "none")

p.global.cmip6.nonco2ghg.whistory

p.global.cmip6.slcfs.whistory <- ggplot(
  cmip6.scenarios.global |> filter(variable%nin%c("CO2", "CH4", "N2O")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", ncol = 1, strip.position = "right") +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario))
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable%nin%c("CO2", "CH4", "N2O"), year>=1990) |>
      add_facet_label(),
    aes(group = interaction(scenario,variable)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right",
        strip.text.y.right = element_text(angle = 0))

p.global.cmip6.slcfs.whistory


p.global.cmip6.whistory <- (p.global.cmip6.co2.whistory + p.global.cmip6.nonco2ghg.whistory + p.global.cmip6.slcfs.whistory) +
  plot_layout(
    design = "
    AAC
    AAC
    BBC
    "
  )

p.global.cmip6.whistory

save_ggplot(
  p = p.global.cmip6.whistory,
  f = here("figures", "cmip6_withhistory"),
  h = 150, w = 250
)


## ALL CMIP7 vs ALL CMIP6 ----
p.global.cmip7.cmip6 <- ggplot(
  cmip7.scenarios.global |> filter(variable=="CO2") |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = cmip6.scenarios.global |> filter(variable=="CO2") %>% add_facet_label(),
    aes(group = interaction(model, scenario)),
    colour="grey",
    linewidth=1.1
  ) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="CO2", year>=1990),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.cmip6
save_ggplot(
  p = p.global.cmip7.cmip6,
  f = here("figures", "cmip6_vs_cmip7_all"),
  h = 150, w = 250
)

## H CMIP7 vs H CMIP6 ----
p.global.cmip7.cmip6.high <- ggplot(
  cmip7.scenarios.global |> filter(variable=="CO2", scenario%in%c("H","HL")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = cmip6.scenarios.global |> filter(variable=="CO2", scenario%in%c("SSP5-85", "SSP3-70")) %>% add_facet_label(),
    aes(colour=scenario, group = interaction(model, scenario)),
    linetype="dotted",
    linewidth=1.1
  ) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="CO2", year>=1990),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=c(SCENARIOS.7,SCENARIOS.6),values=c(SCENARIOS.7.COLOURS, SCENARIOS.6.COLOURS)) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.cmip6.high
save_ggplot(
  p = p.global.cmip7.cmip6.high,
  f = here("figures", "cmip6_vs_cmip7_high"),
  h = 150, w = 250
)


## Medium CMIP7 vs Medium CMIP6 ----
p.global.cmip7.cmip6.medium <- ggplot(
  cmip7.scenarios.global |> filter(variable=="CO2", scenario%in%c("M", "ML")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = cmip6.scenarios.global |> filter(variable=="CO2", scenario%in%c("SSP2-45")) %>% add_facet_label(),
    aes(colour=scenario, group = interaction(model, scenario)),
    linetype="dotted",
    linewidth=1.1
  ) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="CO2", year>=1990),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=c(SCENARIOS.7,SCENARIOS.6),values=c(SCENARIOS.7.COLOURS, SCENARIOS.6.COLOURS)) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.cmip6.medium
save_ggplot(
  p = p.global.cmip7.cmip6.medium,
  f = here("figures", "cmip6_vs_cmip7_medium"),
  h = 150, w = 250
)

## Low CMIP7 vs Low CMIP6 ----
p.global.cmip7.cmip6.low <- ggplot(
  cmip7.scenarios.global |> filter(variable=="CO2", scenario%in%c("L","LN","VL")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = cmip6.scenarios.global |> filter(variable=="CO2", scenario%in%c("SSP1-19", "SSP1-26")) %>% add_facet_label(),
    aes(colour=scenario, group = interaction(model, scenario)),
    linetype="dotted",
    linewidth=1.1
  ) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="CO2", year>=1990),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=c(SCENARIOS.7,SCENARIOS.6),values=c(SCENARIOS.7.COLOURS, SCENARIOS.6.COLOURS)) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.cmip6.low
save_ggplot(
  p = p.global.cmip7.cmip6.low,
  f = here("figures", "cmip6_vs_cmip7_low"),
  h = 150, w = 250
)


## Medium CMIP7 vs Medium CMIP6 (CH4) ----
p.global.cmip7.cmip6.medium.ch4 <- ggplot(
  cmip7.scenarios.global |> filter(variable=="CH4", scenario%in%c("M", "ML")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = cmip6.scenarios.global |> filter(variable=="CH4", scenario%in%c("SSP2-45")) %>% add_facet_label(),
    aes(colour=scenario, group = interaction(model, scenario)),
    linetype="dotted",
    linewidth=1.1
  ) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="CH4", year>=1990),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +
  scale_color_manual(breaks=c(SCENARIOS.7,SCENARIOS.6),values=c(SCENARIOS.7.COLOURS, SCENARIOS.6.COLOURS)) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.cmip6.medium.ch4
save_ggplot(
  p = p.global.cmip7.cmip6.medium.ch4,
  f = here("figures", "cmip6_vs_cmip7_medium_ch4"),
  h = 150, w = 250
)
