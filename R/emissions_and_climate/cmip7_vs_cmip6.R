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

### china ----
cmip7.history.chn <- vroom(
  "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Emissions data/cmip7/country-history.csv"
) %>% filter(region=="chn") %>%
  iamc_wide_to_long()



## cmip7: country-level (downscaled) ----
cmip7.scenarios.china <- vroom(
  # H
  "C:/Users/kikstra/Downloads/downscaled-only-h_1-1-0.csv"
) %>%
  bind_rows(
    # HL
    vroom(
      "C:/Users/kikstra/Downloads/downscaled-only-results_20260302_hl.csv"
    )
  ) %>%
  bind_rows(
    # M
    vroom(
      "C:/Users/kikstra/Downloads/downscaled-only-results_20260302_m.csv"
    )
  ) %>%
  bind_rows(
    # ML
    vroom(
      "C:/Users/kikstra/Downloads/downscaled-only-results_20260309_ml.csv"
    )
  ) %>%
  bind_rows(
    # L
    vroom(
      "C:/Users/kikstra/Downloads/downscaled-only-results_20260305_l.csv"
    )
  ) %>%
  bind_rows(
    # LN
    vroom(
      "C:/Users/kikstra/Downloads/downscaled-only-results_20260302_ln.csv"
    )
  ) %>%
  bind_rows(
    # VL
    vroom(
      "C:/Users/kikstra/Downloads/downscaled-only-vl_1-1-0.csv"
    )
  ) %>%
  filter(country=="chn")
# test with available data
# cmip7.scenarios.china <- vroom(
#   "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/Shared emission fields data/v1_1-testing-findmistakes/vl_1-1-0/downscaled-only-vl_1-1-0.csv"
# ) %>% filter(country=="chn")

# ## cmip7: count regions per model
# vroom(
#   # H
#   "C:/Users/kikstra/Downloads/downscaled-only-h_1-1-0.csv"
# ) %>%
#   bind_rows(
#     # HL
#     vroom(
#       "C:/Users/kikstra/Downloads/downscaled-only-results_20260302_hl.csv"
#     )
#   ) %>%
#   bind_rows(
#     # M
#     vroom(
#       "C:/Users/kikstra/Downloads/downscaled-only-results_20260302_m.csv"
#     )
#   ) %>%
#   bind_rows(
#     # ML
#     vroom(
#       "C:/Users/kikstra/Downloads/downscaled-only-results_20260309_ml.csv"
#     )
#   ) %>%
#   bind_rows(
#     # L
#     vroom(
#       "C:/Users/kikstra/Downloads/downscaled-only-results_20260305_l.csv"
#     )
#   ) %>%
#   bind_rows(
#     # LN
#     vroom(
#       "C:/Users/kikstra/Downloads/downscaled-only-results_20260302_ln.csv"
#     )
#   ) %>%
#   bind_rows(
#     # VL
#     vroom(
#       "C:/Users/kikstra/Downloads/downscaled-only-vl_1-1-0.csv"
#     )
#   ) %>%
#   distinct(model,region) %>% filter(region!="World") %>%
#   group_by(model) %>%
#   count()


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
## Medium CMIP7 vs Medium CMIP6 (N2O) ----
p.global.cmip7.cmip6.medium.n2o <- ggplot(
  cmip7.scenarios.global |> filter(variable=="N2O", scenario%in%c("M", "ML")) |>
    add_facet_label(),
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = cmip6.scenarios.global |> filter(variable=="N2O", scenario%in%c("SSP2-45")) %>% add_facet_label(),
    aes(colour=scenario, group = interaction(model, scenario)),
    linetype="dotted",
    linewidth=1.1
  ) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  geom_line(
    data = cmip7.history %>% filter(variable=="N2O", year>=1990),
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
p.global.cmip7.cmip6.medium.n2o
save_ggplot(
  p = p.global.cmip7.cmip6.medium.n2o,
  f = here("figures", "cmip6_vs_cmip7_medium_n2o"),
  h = 150, w = 250
)



# Country-level: only CMIP7 ----

## China: CO2 emissions ----
p.global.cmip7.china <- ggplot(
  cmip7.scenarios.china |> filter(gas=="CO2") %>%
    rename(variable=gas) %>%
    select(-method,-region) %>%
    rename(region=country) %>%
    pivot_longer(
      cols = `2023`:`2100`,
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year)) %>%
    reframe(
      value = sum(value),
      .by = c(model,scenario,region,variable,unit,year)
    ) %>%
    mutate_cond(scenario=="SSP1 - Very Low Emissions",scenario="VL") %>%
    mutate_cond(scenario=="SSP2 - Low Overshoot_a",scenario="LN") %>%
    mutate_cond(scenario=="SSP2 - Low Emissions",scenario="L") %>%
    mutate_cond(scenario=="SSP2 - Medium-Low Emissions",scenario="ML") %>%
    mutate_cond(scenario=="SSP2 - Medium Emissions",scenario="M") %>%
    mutate_cond(scenario=="SSP5 - Medium-Low Emissions_a",scenario="HL") %>%
    mutate_cond(scenario=="SSP3 - High Emissions",scenario="H"),
  aes(x = year, y = value/1e3)
) +
  facet_wrap(~variable, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(data=cmip7.history.chn %>%
              filter(unit == "Mt CO2/yr") %>%
              mutate(variable="CO2") %>%
              reframe(
                value=sum(value),
                .by = c(scenario,region,variable,unit,year)
              ) %>% filter(year>=2000),
            colour="black",
            linewidth=1.3) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  # geom_line(
  #   data = cmip7.history %>% filter(variable=="CO2", year>=1990),
  #   aes(group = interaction(scenario)),
  #   linewidth=1.3,
  #   colour="black"
  # ) +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = "GtCO2/yr") +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.china
save_ggplot(
  p = p.global.cmip7.china + theme(legend.position="none"),
  f = here("figures", "cmip7_china_co2"),
  h = 150, w = 230
)
## China: SO2 emissions ----
p.global.cmip7.china.so2 <- ggplot(
  cmip7.scenarios.china |> filter(gas=="SO2") %>%
    rename(variable=gas) %>%
    select(-method,-region) %>%
    rename(region=country) %>%
    pivot_longer(
      cols = `2023`:`2100`,
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year)) %>%
    reframe(
      value = sum(value),
      .by = c(model,scenario,region,variable,unit,year)
    ) %>%
    mutate_cond(scenario=="SSP1 - Very Low Emissions",scenario="VL") %>%
    mutate_cond(scenario=="SSP2 - Low Overshoot_a",scenario="LN") %>%
    mutate_cond(scenario=="SSP2 - Low Emissions",scenario="L") %>%
    mutate_cond(scenario=="SSP2 - Medium-Low Emissions",scenario="ML") %>%
    mutate_cond(scenario=="SSP2 - Medium Emissions",scenario="M") %>%
    mutate_cond(scenario=="SSP5 - Medium-Low Emissions_a",scenario="HL") %>%
    mutate_cond(scenario=="SSP3 - High Emissions",scenario="H"),
  aes(x = year, y = value/1e3)
) +
  facet_wrap(~variable, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(data=cmip7.history.chn %>%
              filter(unit == "Mt SO2/yr") %>%
              mutate(variable="SO2") %>%
              reframe(
                value=sum(value),
                .by = c(scenario,region,variable,unit,year)
              ) %>% filter(year>=2000),
            colour="black",
            linewidth=1.3) +
  geom_line(
    aes(colour=scenario, group = interaction(model, scenario)),
    linewidth=1.2
  ) +
  # geom_line(
  #   data = cmip7.history %>% filter(variable=="CO2", year>=1990),
  #   aes(group = interaction(scenario)),
  #   linewidth=1.3,
  #   colour="black"
  # ) +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_x_continuous(limits = c(2010,2100),expand = c(0,0)) +
  theme_jsk() +
  mark_history(sy = 2025) +
  labs(y = "GtCO2/yr") +
  theme(legend.title = element_blank(),
        legend.position = "right")
p.global.cmip7.china.so2
save_ggplot(
  p = p.global.cmip7.china.so2 + theme(legend.position="none"),
  f = here("figures", "cmip7_china_so2"),
  h = 150, w = 230
)














# QUICCA: example of UPDATE / EXTENSION / HARMONIZATION / NEW SCENARIOS ----

## Idea ----
# - Col 1: CMIP6-standard
# - Row 1: extension (WP4)
#   * col 2: CMIP6-like + additional year, no scenarios
#   * col 3: CMIP6-like + additional year, scenarios reharmonized
# - Row 2: extension (WP4)
#   * col 2: CMIP6-like + CMIP7 history (scenarios old CMIP6 harmonization)
#   * col 3: CMIP7-like, scenarios reharmonized
# - Row 3: new scenarios (not in QUICCA)
#   * col 2: ...
#   * col 3: ...


## Data ----

### History ----

#### CMIP6 ----
hist.cmip6 <- read_csv(
  "C:/Users/kikstra/Downloads/history_ar6.csv"
) %>%
  iamc_wide_to_long(upper.to.lower = T) %>% select(-model) %>%
  filter(year>=1990,
         variable=="AR6 climate diagnostics|Emissions|CO2|Unharmonized") %>%
  mutate(variable="CO2",unit="Gt CO2/yr",value=value/1e3)


#### Extension ----
extension.new.year <- 2016
extension.adjustment.factor <- 1.10
harmonization.final.convergence.year <- 2040

#### Update ----
hist.cmip7 <- cmip7.history %>% filter(variable=="CO2", year>=1990)



### Scenarios ----

#### CMIP6 ----
hist.cmip6.extension <- hist.cmip6 %>% bind_rows(
  hist.cmip6 %>% filter(year==2015) %>% mutate(year=2016,value=value*extension.adjustment.factor)
) %>% distinct()


update.new.year <- 2023
# harmonization.final.convergence.year <- 2040

scen.cmip6 <- cmip6.scenarios.global |> filter(variable=="CO2") %>% add_facet_label() %>%
  filter(scenario %in% c("SSP1-19", "SSP2-45", "SSP5-85"))

#### Harmonization ----

##### Extension ----
scen.cmip6.extension <- scen.cmip6 %>%
  bind_rows(
    # add a 2016 timepoint
    scen.cmip6 %>% filter(year==2015) %>% mutate(year=2016,value=value*extension.adjustment.factor)
  ) %>%
  distinct() %>% arrange(model,scenario,region,variable,unit,year) %>%
  mutate_cond(year>extension.new.year & year<harmonization.final.convergence.year,
              value=value*(1 + (extension.adjustment.factor - 1) *
                             (harmonization.final.convergence.year - year) /
                             (harmonization.final.convergence.year - extension.new.year))) %>%
  mutate_cond(year>=harmonization.final.convergence.year, value=value) %>%
  filter(year>=extension.new.year)

##### Update ----
# Linear interpolation to add 2023 between 2020 and 2030
scen.cmip6.w2023 <- scen.cmip6 %>%
  bind_rows(
    scen.cmip6 %>%
      filter(year %in% c(2020, 2030)) %>%
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(
        year  = 2023,
        value = `2020` + (`2030` - `2020`) * (2023 - 2020) / (2030 - 2020)
      ) %>%
      select(-`2020`, -`2030`)
  ) %>%
  arrange(model, scenario, region, variable, unit, year)

update.adjustment.factor.per_scen <- scen.cmip6.w2023 %>%
  filter(year == 2023) %>%
  mutate(hist = (hist.cmip7 %>% filter(year == 2023) %>% pull(value))) %>%
  mutate(update.adjustment.diff = value - hist) %>%
  select(model, scenario, update.adjustment.diff)

scen.cmip6.updated <- scen.cmip6.w2023 %>%
  filter(year >= update.new.year) %>%
  distinct() %>%
  arrange(model, scenario, region, variable, unit, year) %>%
  left_join(update.adjustment.factor.per_scen, by = c("model", "scenario")) %>%
  mutate(value = case_when(
    year == update.new.year ~
      value - update.adjustment.diff,
    year > update.new.year & year < harmonization.final.convergence.year ~
      value - update.adjustment.diff * (harmonization.final.convergence.year - year) /
      (harmonization.final.convergence.year - update.new.year),
    year >= harmonization.final.convergence.year ~
      value
  ))

#### CMIP7 ----
scen.cmip7 <- cmip7.scenarios.global |> filter(variable=="CO2") |> add_facet_label() %>%
  filter(scenario %in% c("VL", "ML", "H"))


## Plots ----
library(geomtextpath)

### Column 1 ----
c1 <- ggplot(
  scen.cmip7,
  aes(x = year, y = value)
) +
  annotate(
    "text",
    x = 2015-0.5, y = 17,
    label = "CMIP history",
    hjust = 1,
    vjust = 0,
    fontface = "bold",
    colour = "black",
    size = 3.5
  ) +
  geom_line(
    data = scen.cmip6,
    aes(colour = scenario, group = interaction(model, scenario)),
    linewidth = 1.1
  ) +
  geom_labelline(
    data = scen.cmip6 %>% filter(scenario == "SSP2-45"),
    aes(group = interaction(model, scenario),
        colour = scenario,
        label = "SSP2-4.5"),          # hardcoded string, not aes mapping
    linewidth = 1.5,
    fontface = "bold",
    hjust = 0.16,
    size = 3,
    fill = "white",                  # box background
    boxcolour = NA,                  # box border (NA = no border, or set to match line colour)
    label.padding = unit(0.15, "lines")
  ) +
  geom_line(
    data = hist.cmip6,
    aes(group = interaction(scenario)),
    linewidth = 1.3,
    colour = "black"
  ) +
  geom_textvline(
    xintercept = 2025,
    label = "Present day",
    linetype = "dashed",
    colour = "grey40",
    hjust = 0.05
  ) +
  scale_color_manual(breaks = SCENARIOS.6, values = SCENARIOS.6.COLOURS) +
  coord_cartesian(xlim = c(2005, 2035), ylim = c(15, 65)) +
  scale_x_continuous(breaks = seq(2005, 2035, 5), labels = c()) +
  theme_jsk() +
  mark_history(sy = 2015) +
  labs(y = "Gt CO2/yr",
       title = "State of the Art") +
  theme(legend.title = element_blank(),
        legend.position = "none")
c1

### Row 1 ----


r1c2 <- ggplot(
  scen.cmip7,
  aes(x = year, y = value)
) +
  # facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  # geom_line(
  #   data = scen.cmip6,
  #   aes(colour=scenario,group = interaction(model, scenario)),
  #   linewidth=1.1
  # ) +
  geom_textvline(
    xintercept = 2025,
    label = "",
    linetype = "dashed",
    colour = "grey40",
    hjust = 0.05
  ) +
  geom_line(
    data = hist.cmip6,
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="darkgrey"
  ) +
  geom_line(
    data = hist.cmip6.extension %>% filter(year>=2015),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +

  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  coord_cartesian(xlim = c(2005,2035), ylim = c(15,65)) +
  scale_x_continuous(breaks=seq(2005,2035,5),labels=c()) +
  theme_jsk() +
  mark_history(sy = 2015) +
  labs(y = "Gt CO2/yr",
       title = expression(bold(Extension) ~ "of history")) +
  theme(legend.title = element_blank(),
        legend.position = "none")
r1c2

r1c3 <- ggplot(
  scen.cmip7,
  aes(x = year, y = value)
) +
  # facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_textvline(
    xintercept = 2025,
    label = "",
    linetype = "dashed",
    colour = "grey40",
    hjust = 0.05
  ) +
  geom_line(
    data = scen.cmip6,
    aes(colour=scenario,group = interaction(model, scenario)),
    linewidth=0.5,
    # colour="grey",
    linetype="dashed"
  ) +
  geom_line(
    data = scen.cmip6.extension,
    aes(colour=scenario,group = interaction(model, scenario)),
    linewidth=1.1
  ) +
  geom_line(
    data = hist.cmip6,
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="darkgrey"
  ) +
  geom_line(
    data = hist.cmip6.extension %>% filter(year>=2015),
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +


  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  coord_cartesian(xlim = c(2005,2035), ylim = c(15,65)) +
  scale_x_continuous(breaks=seq(2005,2035,5),labels=c()) +
  theme_jsk() +
  mark_history(sy = 2015) +
  labs(y = "Gt CO2/yr",
       title = "Reharmonization") +
  theme(legend.title = element_blank(),
        legend.position = "none")

r1c3

### Row 2 ----

r2c2 <- ggplot(
  scen.cmip7,
  aes(x = year, y = value)
) +
  # facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  # geom_line(
  #   data = scen.cmip6,
  #   aes(colour=scenario,group = interaction(model, scenario)),
  #   linewidth=1.1
  # ) +
  geom_textvline(
    xintercept = 2025,
    label = "",
    linetype = "dashed",
    colour = "grey40",
    hjust = 0.05
  ) +
  geom_line(
    data = hist.cmip6,
    aes(group = interaction(scenario)),
    linewidth=0.5,
    colour="darkgrey",
    linetype="dashed"
  ) +
  geom_line(
    data = hist.cmip7,
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +

  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  coord_cartesian(xlim = c(2005,2035), ylim = c(15,65)) +
  scale_x_continuous(breaks=seq(2005,2035,5),labels=c()) +
  theme_jsk() +
  mark_history(sy = 2015) +
  labs(y = "Gt CO2/yr",
       title = expression(bold(Update) ~ "of history")) +
  theme(legend.title = element_blank(),
        legend.position = "none")
r2c2

r2c3 <- ggplot(
  scen.cmip7,
  aes(x = year, y = value)
) +
  # facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_textvline(
    xintercept = 2025,
    label = "",
    linetype = "dashed",
    colour = "grey40",
    hjust = 0.05
  ) +
  geom_line(
    data = scen.cmip6,
    aes(colour=scenario,group = interaction(model, scenario)),
    linewidth=0.5,
    # colour="grey",
    linetype="dashed"
  ) +
  geom_line(
    data = scen.cmip6.updated,
    aes(colour=scenario,group = interaction(model, scenario)),
    linewidth=1.1
  ) +
  geom_line(
    data = hist.cmip6,
    aes(group = interaction(scenario)),
    linewidth=0.5,
    colour="darkgrey",
    linetype="dashed"
  ) +
  geom_line(
    data = hist.cmip7,
    aes(group = interaction(scenario)),
    linewidth=1.3,
    colour="black"
  ) +

  scale_color_manual(breaks=SCENARIOS.6,values=SCENARIOS.6.COLOURS) +
  coord_cartesian(xlim = c(2005,2035), ylim = c(15,65)) +
  scale_x_continuous(breaks=seq(2005,2035,5),labels=c()) +
  theme_jsk() +
  mark_history(sy = 2015) +
  labs(y = "Gt CO2/yr",
       title = "Reharmonization") +
  theme(legend.title = element_blank(),
        legend.position = "none")

r2c3

### Row 3 (TBD) ----
r3c2 <- ggplot() + theme_void()

r3c3 <- ggplot(
  scen.cmip7,
  aes(x = year, y = value)
) +
  facet_wrap(~facet_label, scales = "free_y", nrow = 1) +
  geom_hline(yintercept=0, linetype = "dotted") +
  geom_line(
    data = hist.cmip6,
    aes(group = interaction(scenario)),
    linewidth=0.5,
    linetype="dashed",
    colour="darkgrey"
  ) +
  geom_line(
    data = scen.cmip6,
    aes(group = interaction(model, scenario)),
    colour="darkgrey",
    linetype="dashed",
    linewidth=0.5
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
  coord_cartesian(xlim = c(2005,2035), ylim = c(15,65)) +
  scale_x_continuous(breaks=seq(2005,2035,5)) +
  theme_jsk() +
  mark_history(sy = 2023) +
  labs(y = "Gt CO2/yr",
       title = expression(bold(Update) ~ "of scenarios")) +
  theme(legend.title = element_blank(),
        legend.position = "right")
# r3c3



### COMBINED ----

# p.quicca.harmonisation <- c1 + r1c2 + r1c3 + r2c2 + r2c3 + plot_layout(
#   design = "ABC
#             ADE"
# )
# p.quicca.harmonisation


library(grid)

arrow_plot <- ggplot() +
  theme_void() +
  annotation_custom(
    grob = segmentsGrob(
      x0 = 0.2, x1 = 0.8,
      y0 = 0.5, y1 = 0.5,
      arrow = arrow(length = unit(6, "pt"), type = "closed"),
      gp = gpar(fill = "black", lwd = 1.5)
    )
  )

p.quicca.harmonisation.v1 <- c1 + r1c2 + arrow_plot + r1c3 +
  r2c2 + arrow_plot + r2c3 +
  plot_layout(
    design = "ABCD
              AEFG",
    widths = c(1.5, 1, 0.15, 1)
  )
p.quicca.harmonisation.v1

save_ggplot(
  p = p.quicca.harmonisation.v1,
  f = here("figures", "quicca_updates_v1_2"),
  h = 120, w = 230
)

p.quicca.harmonisation.v2 <- c1 +
  r1c2 + arrow_plot + r1c3 +
  r2c2 + arrow_plot + r2c3 +
  r3c2 + arrow_plot + r3c3 +
  plot_layout(
    design = "ABCD
              AEFG
              AHIJ",
    widths = c(2, 1, 0.15, 1)
  )
p.quicca.harmonisation.v2


save_ggplot(
  p = p.quicca.harmonisation.v2,
  f = here("figures", "quicca_updates_v2_x"),
  h = 200, w = 280
)

