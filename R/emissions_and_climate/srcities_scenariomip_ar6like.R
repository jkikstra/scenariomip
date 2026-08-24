#' Code for comparing AR6-like assessment of ScenarioMIP with standard CMIP7-like assessment
#' Developed by Jarmo Kikstra
# * Author: Jarmo S. Kikstra
# * Dates edited:
# * - March, 12, 2026 (first run)
# * - March, 19, 2026 (add MESSAGE, write out data)
# * - July, 3, 2026 (final ScenarioMIP marker emissions data): as run under '/Users/jarmo/Library/CloudStorage/OneDrive-IIASA/_Other/ClimateAssessmentRun2026 - SRCITIES_ScenarioMIP/run_20260703'
# ********************************************************


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

# Base path for data ----
PATH.srcities.scenariomip.data <- '/Users/jarmo/Library/CloudStorage/OneDrive-IIASA/_Other/ClimateAssessmentRun2026 - SRCITIES_ScenarioMIP'
PATH.climate.run <- file.path(PATH.srcities.scenariomip.data, 'run_20260703','output')
PATH.other.data <- file.path(PATH.srcities.scenariomip.data, 'other_data')
FILE.climate.run <- file.path(PATH.climate.run, 'SRCITIES_ScenarioMIP_20260703_alloutput.xlsx')


# Ten core species ----
the.ten <- c(#"CO2",
             "CO2|AFOLU", "CO2|Energy and Industrial Processes",
             "N2O", "BC", "OC", "CH4", "NH3", "Sulfur", "VOC", "NOx", "CO")

# Plot design ----
scenario_order <- c("VL", "LN", "L", "ML", "M", "HL", "H")
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

# Load Data ----
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

### CMIP7-like: Zenodo dataset ----
scenariomip.like7 <- read_excel(
  file.path(PATH.srcities.scenariomip.data, 'other_data', 'cmip7', 'ScenarioMIP_emissions_marker_scenarios_v0.2.xlsx'),
  sheet = "data"
) %>%
  iamc_wide_to_long()
scenariomip.like7 |> distinct(variable,unit)
scenariomip.like7 |> distinct(scenario)

### CMIP7-like: history ----
cmip7.history <- read_csv(
  file.path(PATH.srcities.scenariomip.data, 'other_data', 'cmip7', 'global-workflow-history.csv')
) %>% iamc_wide_to_long()


### AR6-like: ran locally on Jarmo's laptop ----
fix_scenario_names <- function(df){
  df %>%
    mutate(new_scenario_name = NA_character_) %>%
    # general renaming
    # tbd...
    # markers
    mutate_cond(model == "AIM 3.0" & scenario == "SSP2 - Low Overshoot_a", new_scenario_name = "Low-to-Negative - SSP2 (Marker)") |>
    mutate_cond(model == "REMIND-MAgPIE 3.5-4.11" & scenario == "SSP1 - Very Low Emissions", new_scenario_name = "Very Low - SSP1 (Marker)") |>
    mutate_cond(model == "MESSAGEix-GLOBIOM-GAINS 2.1-M-R12" & scenario == "SSP2 - Low Emissions", new_scenario_name = "Low - SSP2 (Marker)") |>
    mutate_cond(model == "COFFEE 1.6" & scenario == "SSP2 - Medium-Low Emissions", new_scenario_name = "Medium-to-Low - SSP2 (Marker)") |>
    mutate_cond(model == "IMAGE 3.4" & scenario == "SSP2 - Medium Emissions", new_scenario_name = "Medium - SSP2 (Marker)") |>
    mutate_cond(model == "WITCH 6.0" & scenario == "SSP5 - Medium-Low Emissions_a", new_scenario_name = "High-to-Low - SSP5 (Marker)") |>
    mutate_cond(model == "GCAM 8s" & scenario == "SSP3 - High Emissions", new_scenario_name = "High - SSP3 (Marker)") %>%
    return()
}
scenariomip.like6 <- read_excel(
  FILE.climate.run,
  sheet = "data"
) %>% upper_to_lower() %>% mutate(full.model.name=model) %>%
  fix_scenario_names() %>%
  mutate(scenario=new_scenario_name) %>% select(-new_scenario_name, -full.model.name) %>%
  iamc_wide_to_long() 

scenariomip.like6 |> distinct(variable,unit)
scenariomip.like6 |> distinct(scenario)


### AR6-like: history ----
ar6.history <- read_csv(
  file.path(PATH.other.data, 'cmip6', 'history_ar6.csv')
) %>%
  iamc_wide_to_long(upper.to.lower = T)




# ECMWF report March 2026 ----

## Figure 1 ----
f1a <- ggplot(mapping=aes(x=year)) +
  facet_grid(interaction(variable)~scenario, scales="free_y") +

  geom_line(
    data = cmip7.history %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%paste0("Emissions|",
                                c("CO2|AFOLU", "CO2|Energy and Industrial Processes"))) %>%
      iamc_variable_keep_one_level(-1) %>%
      select(-scenario) %>%
      filter(
        year>=2010
      ),
    aes(y=value/1e3, linetype = `Assessment workflow`),
    colour="black"
  ) +

  geom_line(
    data = scenariomip.like7 %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%c(paste0("Climate Assessment|Harmonized and Infilled|Emissions|",
                                  c("CO2|AFOLU", "CO2|Energy and Industrial Processes")),
                           paste0("Infilled|Emissions|",
                                  c("CO2|AFOLU", "CO2|Energy and Industrial Processes")))) %>%
      iamc_variable_keep_one_level(-1) %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value/1e3, colour=scenario, linetype=`Assessment workflow`)
  ) +

  # geom_line(
  #   data = scenariomip.like7 %>% mutate(
  #     `Assessment workflow` = "CMIP7"
  #   ) %>%
  #     filter(variable%in%c(paste0("Climate Assessment|Harmonized and Infilled|Emissions|",
  #                                 c("CO2|AFOLU", "CO2|Energy and Industrial Processes")),
  #                          paste0("Infilled|Emissions|",
  #                                 c("CO2|AFOLU", "CO2|Energy and Industrial Processes")))) %>%
  #     iamc_variable_keep_one_level(-1) %>%
  #     rename_cmip7_scenarios() %>%
  #     filter(scenario=="L") %>%
  #     mutate(scenario = factor(scenario, levels = scenario_order)),
  #   aes(y=value/1e3, colour=scenario, linetype=`Assessment workflow`),
  #   linewidth=1.2
  # ) +


  theme_jsk() +
  mark_history(sy=2025) +
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  ) +
  guides(
    color="none",
    linetype="none"
  ) +
  labs(y="GtCO2 / year",
       title = "ScenarioMIP-CMIP7 emissions and climate",
       subtitle = "CO2 emissions trajectories") +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  # scale_fill_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_x_continuous(expand = c(0,0))
f1a

f1b <- ggplot(mapping=aes(x=year)) +
  # facet_grid(variable~scenario, scales="free_y") +
  geom_line(
    data = scenariomip.like7 %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%c(
        "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICCv7.6.0a3]",
        "Surface Temperature (GSAT) - 0.5 - MAGICCv7.6.0a3"

      )) %>%
      mutate(variable="GSAT") %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +

  # range L
  geom_ribbon(
    data = scenariomip.like7 %>%
      filter(variable%in%c(
        "Climate Assessment|Surface Temperature (GSAT)|67th Percentile [MAGICCv7.6.0a3]",
        "Surface Temperature (GSAT) - 0.67 - MAGICCv7.6.0a3"
      )) %>%
      mutate(variable = "p67") %>%
      bind_rows(
        scenariomip.like7 %>%
        filter(variable%in%c(
          "Climate Assessment|Surface Temperature (GSAT)|33rd Percentile [MAGICCv7.6.0a3]",
          "Surface Temperature (GSAT) - 0.33 - MAGICCv7.6.0a3"
        )) %>%
          mutate(variable = "p33")
      ) %>%
      rename_cmip7_scenarios() %>%
      filter(scenario=="L") %>%
      pivot_wider(names_from = variable, values_from = value),
    aes(ymin=`p33`,
        ymax=`p67`,
        fill=scenario),
    alpha=0.3
  ) +

  # highlight L
  geom_line(
    data = scenariomip.like7 %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%c(
        "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICCv7.6.0a3]",
        "Surface Temperature (GSAT) - 0.5 - MAGICCv7.6.0a3"
      )) %>%
      mutate(variable="GSAT") %>%
      rename_cmip7_scenarios() %>%
      filter(scenario=="L") %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario),
    linewidth=1.2
  ) +


  mark_history(sy = 2025) +
  theme_jsk() +
  guides(
    color="none",
    linetype="none",
    fill="none"
  ) +
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  ) +
  labs(subtitle="Emulated GSAT outcomes",caption="33-67th percentile range visualised for the L scenario") +
  ylab("Temperature above\n1850-1900 mean [\u00B0C]") +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_fill_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_x_continuous(expand = c(0,0))

f1b

f1 <- f1a + f1b +
  plot_layout(
    design =
    "A
    B",
    heights = c(1,2)
  )
f1

save_ggplot(
  p = f1,
  f = here("figures", "ecmwf_march2026_f1_v1_0"),
  h = 250, w = 250
)



## Figure 2 ----
# --> see `cmip7_vs_cmip6`


# Plot the ten core emissions (tbd) ----
em10 <- ggplot(mapping=aes(x=year)) +
  facet_grid(interaction(variable,unit)~scenario, scales="free_y") +

  geom_line(
    data = ar6.history %>% mutate(
      `Assessment workflow` = "AR6"
    ) %>%
      filter(variable%in%paste0("AR6 climate diagnostics|Emissions|",
                                the.ten,
                                "|Unharmonized")) %>%
      iamc_variable_keep_one_level(-2) %>%
      select(-scenario) %>%
      filter(
        year>=2010
      ),
    aes(y=value, linetype = `Assessment workflow`),
    colour="black"
  ) +

  geom_line(
    data = cmip7.history %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%paste0("Emissions|",
                                the.ten)) %>%
      iamc_variable_keep_one_level(-1) %>%
      select(-scenario) %>%
      filter(
        year>=2010
      ),
    aes(y=value, linetype = `Assessment workflow`),
    colour="black"
  ) +

  geom_line(
    data = scenariomip.like6 %>% mutate(
      `Assessment workflow` = "AR6"
    ) %>%
      filter(variable%in%paste0("AR6 climate diagnostics|Infilled|Emissions|",
                                the.ten)) %>%
      iamc_variable_keep_one_level(-1) %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +
  geom_line(
    data = scenariomip.like7 %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%c(paste0("Climate Assessment|Harmonized and Infilled|Emissions|",
                                the.ten),
                           paste0("Infilled|Emissions|",
                                  the.ten))) %>%
      iamc_variable_keep_one_level(-1) %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +
  theme_jsk() +
  mark_history(sy=2025) +
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  ) +
  guides(
    color="none"
  ) +
  ylab(NULL) +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_fill_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS)
em10

# Plot temperatures----
temp.p50 <- ggplot(mapping=aes(x=year)) +
  facet_grid(variable~scenario, scales="free_y") +
  geom_line(
    data = scenariomip.like6 %>% mutate(
      `Assessment workflow` = "AR6"
    ) %>%
      filter(variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile") %>%
      mutate(variable="GSAT") %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +
  geom_line(
    data = scenariomip.like7 %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%c(
        "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICCv7.6.0a3]",
        "Surface Temperature (GSAT) - 0.5 - MAGICCv7.6.0a3"

      )) %>%
      mutate(variable="GSAT") %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +
  mark_history(sy = 2025) +
  theme_jsk() +
  guides(
    color="none"
  ) +
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  ) +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_fill_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS)
temp.p50

# Plot temperatures: SRCITIES possible figure ----
temp.p50.srcities.plot <- ggplot(mapping=aes(x=year)) +

  geom_ribbon(
    data = scenariomip.like6 %>% mutate(
      `Assessment workflow` = "AR6"
    ) %>%
      filter(variable%in%c(
        "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|33.0th Percentile",
        "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|67.0th Percentile"
      )) %>%
      rename_cmip7_scenarios() %>%
      pivot_wider(names_from = variable, values_from = value),
    aes(ymin=`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|33.0th Percentile`,
        ymax=`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|67.0th Percentile`,
        fill=scenario, linetype=`Assessment workflow`),
    alpha=0.3
  ) +

  geom_line(
    data = scenariomip.like6 %>% mutate(
      `Assessment workflow` = "AR6"
    ) %>%
      filter(variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile") %>%
      mutate(variable="GSAT") %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +

  mark_history(sy = 2025) +
  theme_jsk() +
  guides(
    color="none"
  ) +
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  ) +
  labs(caption="33-67th percentile range") +
  ylab("Temperature above 1850-1900 mean [\u00B0C]") +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_fill_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS)

temp.p50.srcities.plot

save_ggplot(
  p = temp.p50.srcities.plot,
  f = here("figures", "scenariomip_forSRCITIES_TEMPS_ar6like_v3"),
  h = 200, w = 200
)



# Plot ERW: GHG total | AER total | OC direct ----
erw.p50 <- ggplot(mapping=aes(x=year)) +
  facet_grid(variable~scenario, scales="free_y") +
  geom_line(
    data = scenariomip.like6 %>% mutate(
      `Assessment workflow` = "AR6"
    ) %>%
      filter(variable%in%c(
        "AR6 climate diagnostics|Effective Radiative Forcing|Basket|Greenhouse Gases|MAGICCv7.5.3|50.0th Percentile",
        "AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|Direct Effect|OC|MAGICCv7.5.3|50.0th Percentile",
        "AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|MAGICCv7.5.3|50.0th Percentile",
        "AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|Direct Effect|Sulfur|MAGICCv7.5.3|50.0th Percentile",
        "AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|Indirect Effect|MAGICCv7.5.3|50.0th Percentile"
      )) %>%
      mutate_cond(variable=="AR6 climate diagnostics|Effective Radiative Forcing|Basket|Greenhouse Gases|MAGICCv7.5.3|50.0th Percentile", variable="ERW|GHG") %>%
      mutate_cond(variable=="AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|MAGICCv7.5.3|50.0th Percentile", variable="ERW|Aerosols") %>%
      mutate_cond(variable=="AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|Direct Effect|OC|MAGICCv7.5.3|50.0th Percentile", variable="ERW|OC (direct only)") %>%
      mutate_cond(variable=="AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|Direct Effect|Sulfur|MAGICCv7.5.3|50.0th Percentile", variable="ERW|SOx (direct only)") %>%
      mutate_cond(variable=="AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|Indirect Effect|MAGICCv7.5.3|50.0th Percentile", variable="ERW|Aerosols (indirect only)") %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +
  geom_line(
    data = scenariomip.like7 %>% mutate(
      `Assessment workflow` = "CMIP7"
    ) %>%
      filter(variable%in%c(
        # raw output, ML
        "Effective Radiative Forcing|Greenhouse Gases - 0.5 - MAGICCv7.6.0a3",
        "Effective Radiative Forcing|Aerosols - 0.5 - MAGICCv7.6.0a3",
        "Effective Radiative Forcing|Aerosols|Direct Effect|OC - 0.5 - MAGICCv7.6.0a3",
        "Effective Radiative Forcing|Aerosols|Direct Effect|SOx - 0.5 - MAGICCv7.6.0a3",
        "Effective Radiative Forcing|Aerosols|Indirect Effect - 0.5 - MAGICCv7.6.0a3",

        # scenarioMIP scenario explorer style, all
        "Climate Assessment|Effective Radiative Forcing|Greenhouse Gases|Median [MAGICCv7.6.0a3]",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Median [MAGICCv7.6.0a3]",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Direct Effect|OC|Median [MAGICCv7.6.0a3]",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Direct Effect|SOx|Median [MAGICCv7.6.0a3]",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Indirect Effect|Median [MAGICCv7.6.0a3]"
      )) %>%
      mutate_cond(variable%in%c(
        "Effective Radiative Forcing|Greenhouse Gases - 0.5 - MAGICCv7.6.0a3",
        "Climate Assessment|Effective Radiative Forcing|Greenhouse Gases|Median [MAGICCv7.6.0a3]"
      ), variable="ERW|GHG") %>%
      mutate_cond(variable%in%c(
        "Effective Radiative Forcing|Aerosols - 0.5 - MAGICCv7.6.0a3",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Median [MAGICCv7.6.0a3]"
      ), variable="ERW|Aerosols") %>%
      mutate_cond(variable%in%c(
        "Effective Radiative Forcing|Aerosols|Direct Effect|OC - 0.5 - MAGICCv7.6.0a3",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Direct Effect|OC|Median [MAGICCv7.6.0a3]"
      ), variable="ERW|OC (direct only)") %>%
      mutate_cond(variable%in%c(
        "Effective Radiative Forcing|Aerosols|Direct Effect|SOx - 0.5 - MAGICCv7.6.0a3",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Direct Effect|SOx|Median [MAGICCv7.6.0a3]"
      ), variable="ERW|SOx (direct only)") %>%
      mutate_cond(variable%in%c(
        "Effective Radiative Forcing|Aerosols|Indirect Effect - 0.5 - MAGICCv7.6.0a3",
        "Climate Assessment|Effective Radiative Forcing|Aerosols|Indirect Effect|Median [MAGICCv7.6.0a3]"
      ), variable="ERW|Aerosols (indirect only)") %>%
      rename_cmip7_scenarios() %>%
      mutate(scenario = factor(scenario, levels = scenario_order)),
    aes(y=value, colour=scenario, linetype=`Assessment workflow`)
  ) +
  mark_history(sy = 2025) +
  theme_jsk() +
  guides(
    color="none"
  ) +
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  ) +
  scale_color_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS) +
  scale_fill_manual(breaks=SCENARIOS.7,values=SCENARIOS.7.COLOURS)
erw.p50



# combined plot ----
compare_data <- (temp.p50 + em10 + erw.p50) + plot_layout(
  design = "
  AAA
  BBB
  BBB
  BBB
  BBB
  CCC
  CCC
  "
)


save_ggplot(
  p = compare_data,
  f = here("figures", "scenariomip_forSRCITIES_ar6-vs-cmip7_with-history_v3"),
  h = 800, w = 300
)



# Save out emissions data ----
scenariomip.like6.infilled <- scenariomip.like6 %>% iamc_long_to_wide() %>%
  filter(
    grepl(pattern = "Infilled", x=variable, fixed=T),
    !grepl(pattern = "Kyoto", x=variable, fixed=T)
  )



write_delim(x = scenariomip.like6.infilled,
            file = file.path(PATH.srcities.scenariomip.data, "emissions_for_scm/emissions_for_scm_v3.csv"),
            delim = ","
)

# Save out temperature data ----
scenariomip.like6.temperature <- scenariomip.like6 %>% iamc_long_to_wide() %>%
  filter(
    grepl(pattern = "|Surface Temperature (GSAT)", x=variable, fixed=T)
  )



write_delim(x = scenariomip.like6.temperature,
            file = file.path(PATH.srcities.scenariomip.data, "temperature_from_scm/temperature_magicc_v3.csv"),
            delim = ","
)


# Differences emissions and temperature data (v2 vs. v3) ----

temp.v2 <- read_csv(file.path(PATH.srcities.scenariomip.data, "temperature_from_scm/temperature_magicc_v2.csv")) %>% 
  iamc_wide_to_long()
temp.v3 <- read_csv(file.path(PATH.srcities.scenariomip.data, "temperature_from_scm/temperature_magicc_v3.csv")) %>% 
  iamc_wide_to_long()
temp <- temp.v3 %>% mutate(version="v3") %>% 
  bind_rows(temp.v2 %>% mutate(version="v2"))

temp.diff <- temp %>% pivot_wider(names_from = version, values_from = value) %>% 
  mutate(diff = v3-v2)
View(temp.diff)
temp.diff %>%
  filter(diff!=0) %>% 
  distinct(scenario)
write_delim(x = temp.diff,
            file = file.path(PATH.srcities.scenariomip.data, "temperature_from_scm/temperature_magicc_diff_v3_v2.csv"),
            delim = ","
)

em.v2 <- read_csv(file.path(PATH.srcities.scenariomip.data, "emissions_for_scm/emissions_for_scm_v2.csv")) %>% 
  iamc_wide_to_long()
em.v3 <- read_csv(file.path(PATH.srcities.scenariomip.data, "emissions_for_scm/emissions_for_scm_v3.csv")) %>% 
  iamc_wide_to_long()
em <- em.v3 %>% mutate(version="v3") %>% 
  bind_rows(em.v2 %>% mutate(version="v2"))

em.diff <- em %>% pivot_wider(names_from = version, values_from = value) %>% 
  mutate(diff = v3-v2)
View(em.diff)
em.diff %>%
  filter(diff!=0) %>% 
  distinct(scenario)
write_delim(x = em.diff,
            file = file.path(PATH.srcities.scenariomip.data, "emissions_for_scm/emissions_for_scm_diff_v3_v2.csv"),
            delim = ","
)


