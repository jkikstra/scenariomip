#' Code for comparing AR6-like assessment of ScenarioMIP with standard CMIP7-like assessment
#' Developed by Jarmo Kikstra
# * Author: Jarmo S. Kikstra
# * Dates edited:
# * - March, 12, 2026 (first run)
# * - March, 19, 2026 (add MESSAGE, write out data)
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
  "C:\\Users\\kikstra\\OneDrive - IIASA\\_Other\\Data\\Scenario data\\Scenario Databases\\ScenarioMIP-CMIP7\\ScenarioMIP_emissions_marker_scenarios_v0.1.xlsx",
  sheet = "data"
) %>%
  bind_rows(
    # add ML: temps
    read_csv(
      "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/ScenarioMIP Emulator workflow/08 March 2026 (COFFEE)/climate-assessment/COFFEE 1.6/assessed-warming-timeseries-quantiles_COFFEE 1.6.csv"
    ) %>% mutate(scenario = "Medium-to-Low - SSP2 (Marker)") %>%
      mutate(variable=paste(variable,"-",quantile, "-", climate_model)) %>%
      select(-quantile,-climate_model)
  ) %>%
  bind_rows(
    # add ML: emissions
    read_csv(
      "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/ScenarioMIP Emulator workflow/08 March 2026 (COFFEE)/emissions/COFFEE 1.6/infilled_COFFEE 1.6.csv"
    ) %>% mutate(scenario = "Medium-to-Low - SSP2 (Marker)",
                 variable = paste0("Infilled|",variable))
  ) %>%
  bind_rows(
    # add ML: ERW
    read_csv(
      "C:/Users/kikstra/IIASA/ECE.prog - Documents/Projects/CMIP7/IAM Data Processing/ScenarioMIP Emulator workflow/08 March 2026 (COFFEE)/climate-assessment/COFFEE 1.6/erf-timeseries-quantiles_COFFEE 1.6.csv"
    ) %>% mutate(scenario = "Medium-to-Low - SSP2 (Marker)") %>%
      mutate(variable=paste(variable,"-",quantile, "-", climate_model)) %>%
      select(-quantile,-climate_model)
  ) %>%
  iamc_wide_to_long()
scenariomip.like7 |> distinct(variable,unit)
scenariomip.like7 |> distinct(scenario)

### CMIP7-like: history ----
cmip7.history <- read_csv(
  "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Emissions data/cmip7/global-workflow-history.csv"
) %>% iamc_wide_to_long()


### AR6-like: ran locally on Jarmo's laptop ----
scenariomip.like6 <- read_excel(
  "C:/Users/kikstra/OneDrive - IIASA/_Other/ClimateAssessmentRun2026 - SRCITIES_ScenarioMIP/run_20260312/output/v1/SRCITIES_ScenarioMIP_alloutput.xlsx",
  sheet = "data"
) %>% upper_to_lower() %>% mutate(full.model.name=model) %>%
  fix_scenario_names() %>%
  mutate(scenario=new_scenario_name) %>% select(-new_scenario_name, -full.model.name) %>%
  iamc_wide_to_long() %>%
  bind_rows(
    read_excel(
      "C:/Users/kikstra/OneDrive - IIASA/_Other/ClimateAssessmentRun2026 - SRCITIES_ScenarioMIP/run_20260318/output/v1/onlyMESSAGE_SRCITIES_ScenarioMIP_alloutput.xlsx",
      sheet = "data"
    ) %>% upper_to_lower() %>% mutate(full.model.name=model) %>%
      fix_scenario_names() %>%
      mutate(scenario=new_scenario_name) %>% select(-new_scenario_name, -full.model.name) %>%
      iamc_wide_to_long()
  )

scenariomip.like6 |> distinct(variable,unit)
scenariomip.like6 |> distinct(scenario)


### AR6-like: history ----
ar6.history <- read_csv(
  "C:/Users/kikstra/Downloads/history_ar6.csv"
) %>%
  iamc_wide_to_long(upper.to.lower = T)




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
  f = here("figures", "scenariomip_forSRCITIES_TEMPS_ar6like"),
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
  f = here("figures", "scenariomip_forSRCITIES_ar6-vs-cmip7_with-history_v2"),
  h = 800, w = 300
)



# Save out emissions data ----
scenariomip.like6.infilled <- scenariomip.like6 %>% iamc_long_to_wide() %>%
  filter(
    grepl(pattern = "Infilled", x=variable, fixed=T),
    !grepl(pattern = "Kyoto", x=variable, fixed=T)
  )



write_delim(x = scenariomip.like6.infilled,
            file = "C:/Users/kikstra/OneDrive - IIASA/_Other/ClimateAssessmentRun2026 - SRCITIES_ScenarioMIP/emissions_for_scm/emissions_for_scm_v2.csv",
            delim = ","
)

# Save out temperature data ----
scenariomip.like6.temperature <- scenariomip.like6 %>% iamc_long_to_wide() %>%
  filter(
    grepl(pattern = "|Surface Temperature (GSAT)", x=variable, fixed=T)
  )



write_delim(x = scenariomip.like6.temperature,
            file = "C:/Users/kikstra/OneDrive - IIASA/_Other/ClimateAssessmentRun2026 - SRCITIES_ScenarioMIP/temperature_from_scm/temperature_magicc_v2.csv",
            delim = ","
)
