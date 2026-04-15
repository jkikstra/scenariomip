#' Code for ScenarioMIP data publication of climate assessment data
#' Developed by Jarmo Kikstra


# Load packages ----
required_packages <- c("here", "tidyverse", "vroom", "readxl", "writexl", "openxlsx", "testthat")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("writexl")
library("openxlsx")

here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Define paths and constants ----


## Constants for format for Scenario Explorer ----
GENERAL_VARIABLE_PREFIX <- "Climate Assessment|"
INFILLING_VARIABLE_PREFIX <- paste0(GENERAL_VARIABLE_PREFIX, "Harmonized and Infilled|")
CLIMATE_VARIABLE_PREFIX <- paste0(GENERAL_VARIABLE_PREFIX)

# MAGICC_VERSION <- 

## Climate Assessment Data ----
MAIN_DATE <- "20260205" # date of climate assessment run (v0.1)
VERSION_RELEASE_SCENARIOMIP <- "v0.1" # version of the data release for ScenarioMIP

MAIN_DATE <- "20260325" # date of climate assessment run (v0.2)
VERSION_RELEASE_SCENARIOMIP <- "v0.2" # version of the data release for ScenarioMIP
VERSION_RELEASE_SCENARIOMIP_PREVIOUS <- "v0.1"

MAIN_DATA_FOLDER_NAME <- paste0(MAIN_DATE, " (release ", VERSION_RELEASE_SCENARIOMIP, ")")
MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT <- here("data", "ca", MAIN_DATA_FOLDER_NAME) # prep for first pre-release (only infilled emissions and climate outcomes)
DATA.EHH.FOLDERS <- c(
  MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT
)
PATH.CLIMATE.DATA <- file.path(DATA.EHH.FOLDERS, "climate-assessment")
if (VERSION_RELEASE_SCENARIOMIP == "v0.1") {
  PATH.CLIMATE.DATA.EXCEEDANCE <- file.path(DATA.EHH.FOLDERS, "exceedance_probabilities")
} else if (VERSION_RELEASE_SCENARIOMIP == "v0.2") {
  PATH.CLIMATE.DATA.EXCEEDANCE <- file.path(DATA.EHH.FOLDERS, "exceedance-probabilities")
} else {
  message("Unknown version, skipping exceedance probabilities.")
  next
}

## Output ----
PUBLICATION_FOLDER <- file.path(MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT, "for publication")
FINAL_DATA_FILE <- file.path(PUBLICATION_FOLDER, paste0("climate_assessment " , MAIN_DATA_FOLDER_NAME,".xlsx"))

## README sheet ----
# README_FILE_PATH <- file.path(MAIN_DATA_FOLDER_CLIMATE_ASSESSMENT, "climate_assessment_with_manual_readme_example.xlsx")
# Do manually, because formatting does not work nicely when copying over.

# Local utils ----
get_date_string_path_climate_data <- function(p){
  parts <- strsplit(p, "/")[[1]]
  date_str <- parts[length(parts) - 1]

  return(date_str)
}


# Load timeseries data ----

## Climate --------------------------------------------------------------------

### GSAT (assessed) -----------------------------------------------------------
climate.timeseries.gsat <- NULL
for (p in PATH.CLIMATE.DATA){
  dirs = list.dirs(path = p, recursive = FALSE)

  if (length(dirs) == 0) {
    message("No subdirectories found.")
    climate.timeseries.gsat <- climate.timeseries.gsat %>%
      bind_rows(
        load_multiple_files(folder.path = p,
                            iamc = F,
                            pattern = "assessed-warming") %>%
          pivot_longer(cols = `2000`:`2100`,
                       names_to = "year",
                       values_to = "value") %>%
          mutate(year=as.numeric(year)) %>%
          filter(quantile%in%c(0.33,0.5,0.67)) %>%
          mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
          select(-quantile) %>%
          add_scenariomip_info_columns() %>%
          mutate(ca.version = get_date_string_path_climate_data(p))
      )
  } else {
    message("Subdirectories found: ", paste(basename(dirs), collapse = ", "))
    for (d in dirs){
      climate.timeseries.gsat <- climate.timeseries.gsat %>%
        bind_rows(
          load_multiple_files(folder.path = d,
                              iamc = F,
                              pattern = "assessed-warming") %>%
            pivot_longer(cols = `2000`:`2100`,
                         names_to = "year",
                         values_to = "value") %>%
            mutate(year=as.numeric(year)) %>%
            filter(quantile%in%c(0.33,0.5,0.67)) %>%
            mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
            select(-quantile) %>%
            add_scenariomip_info_columns() %>%
            mutate(ca.version = get_date_string_path_climate_data(p))
        )
    }
  }




}

### ERF -----------------------------------------------------------

climate.timeseries.erf <- NULL
for (p in PATH.CLIMATE.DATA){

  dirs = list.dirs(path = p, recursive = FALSE)

  if (length(dirs) == 0) {
    message("No subdirectories found.")
    climate.timeseries.erf <- climate.timeseries.erf %>%
      bind_rows(
        load_multiple_files(folder.path = p,
                            iamc = F,
                            pattern = "erf-timeseries") %>%
          pivot_longer(cols = `2000`:`2100`,
                       names_to = "year",
                       values_to = "value") %>%
          mutate(year=as.numeric(year)) %>%
          filter(quantile%in%c(0.33,0.5,0.67)) %>%
          mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
          select(-quantile) %>%
          add_scenariomip_info_columns() %>%
          mutate(ca.version = get_date_string_path_climate_data(p))
      )
  } else {
    message("Subdirectories found: ", paste(basename(dirs), collapse = ", "))
    for (d in dirs){
      climate.timeseries.erf <- climate.timeseries.erf %>%
        bind_rows(
          load_multiple_files(folder.path = d,
                              iamc = F,
                              pattern = "erf-timeseries") %>%
            pivot_longer(cols = `2000`:`2100`,
                         names_to = "year",
                         values_to = "value") %>%
            mutate(year=as.numeric(year)) %>%
            filter(quantile%in%c(0.33,0.5,0.67)) %>%
            mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
            select(-quantile) %>%
            add_scenariomip_info_columns() %>%
            mutate(ca.version = get_date_string_path_climate_data(p))
        )
    }
  }
}

## Emissions processing ---------------------------------------------------------------

# if climate.timeseries.gsat exists, use the model names from there to load infilled emissions like `climate.timeseries.gsat %>% pull(full.model.name) %>% unique()`, else specify model names in a different way
all.full.model.names <- climate.timeseries.gsat %>% pull(full.model.name) %>% unique()


### Pre-processed (SCM) -----------------------------------------------------
# skipped for now; if this is wanted, see `markers_overview_emissions_scenariomip-description-paper.R` for the code

### Harmonized (SCM) --------------------------------------------------------
# skipped for now; if this is wanted, see `markers_overview_emissions_scenariomip-description-paper.R` for the code

### Infilled (SCM) -----------------------------------------------------
infilled <- tibble()
for (p in PATH.CLIMATE.DATA){
  for (m in all.full.model.names ){
    path <- file.path(p, "..", "emissions", m)
    if (!dir.exists(path)) {
      message("Skipping missing path: ", path)
      next
    }

    df.m <- load_multiple_files(folder.path = path,
                                iamc = T,
                                pattern = "infilled") %>%
      mutate(ca.version = get_date_string_path_climate_data(p))

    infilled <- infilled %>% bind_rows(df.m)
  }
}


# Load metadata ----

## Warming ----
metadata.warming <- NULL
for (p in PATH.CLIMATE.DATA){
  dirs = list.dirs(path = p, recursive = FALSE)

  if (length(dirs) == 0) {
    message("No subdirectories found.")
    metadata.warming <- metadata.warming %>%
      bind_rows(
        load_multiple_files(folder.path = p,
                            iamc = F,
                            pandas.multiindex = T,
                            id.cols=c("climate_model", "model", "region", "scenario", "unit", "variable"),
                            mi.cols=c("metric", "quantile"),
                            pattern = "warming-quantiles") %>%
          mutate(value=as.numeric(value)) |> 
          add_scenariomip_info_columns() %>%
          mutate(ca.version = get_date_string_path_climate_data(p))
      )
  } else {
    message("Subdirectories found: ", paste(basename(dirs), collapse = ", "))
    for (d in dirs){
      metadata.warming <- metadata.warming %>%
        bind_rows(
          load_multiple_files(folder.path = d,
                              iamc = F,
                              pandas.multiindex = T,
                              id.cols=c("climate_model", "model", "region", "scenario", "unit", "variable"),
                              mi.cols=c("metric", "quantile"),
                              pattern = "warming-quantiles") %>%
          mutate(value=as.numeric(value)) |>
            add_scenariomip_info_columns() %>%
            mutate(ca.version = get_date_string_path_climate_data(p))
        )
    }
  }

}

## Exceedance Probabilities ----
metadata.exceedance <- NULL
for (p in PATH.CLIMATE.DATA.EXCEEDANCE){

  if (VERSION_RELEASE_SCENARIOMIP == "v0.1") {
    message("Loading exceedance probabilities from v0.1 path: ", p)
    metadata.exceedance <- metadata.exceedance %>% bind_rows(
      load_multiple_files(folder.path = p,
                        iamc = F,
                        pattern = "exceedance_probabilities",
                        iamc.wide.to.long = F) |> 
      rename(value=`0`) |> 
      add_scenariomip_info_columns() |> 
      mutate(ca.version = get_date_string_path_climate_data(p))
  )
  } else {
    message("Loading exceedance probabilities from v0.2 path: ", p)
    metadata.exceedance <- metadata.exceedance %>% bind_rows(
      load_multiple_files(folder.path = p,
                        iamc = F,
                        pattern = "exceedance_probabilities",
                        iamc.wide.to.long = F) |> 
      # rename(value=`0`) |> 
      add_scenariomip_info_columns() |> 
      mutate(ca.version = get_date_string_path_climate_data(p))
  )
  }

}


# Formatting data ----

## Functions and constants ----
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
format_emissions_variables <- function(df){
  df |> 
    # add prefix
    mutate(variable = paste0(INFILLING_VARIABLE_PREFIX, variable)) |>
    # align unit with common-definitions
    mutate_cond(unit == "kt HFC4310/yr", unit = "kt HFC4310mee/yr")  %>%
    return()
}
format_climate_variables <- function(df){
  df |> 
    # ensure model is full model name
    mutate(model = full.model.name) |> 
    
    # add prefix
    mutate(variable = paste0(CLIMATE_VARIABLE_PREFIX, variable)) |>
    # merge percentile into variable name (currently working for p33, p50, p67)
    mutate_cond(percentile %in% c("p33"), variable = paste0(variable, "|", substr(percentile, 2, nchar(percentile)), "rd Percentile")) |>
    mutate_cond(percentile %in% c("p67"), variable = paste0(variable, "|", substr(percentile, 2, nchar(percentile)), "th Percentile")) |>
    mutate_cond(percentile == "p50", variable = paste0(variable, "|", "Median")) |>
    # merge climate model into variable name
    mutate(variable = paste0(variable, " [", climate_model, "]")) %>%
    
    # unit
    mutate_cond(unit == "K", unit = "°C") %>%
    return()
}


## Infilled emissions --------------------------------------------------------
infilled.formatted <- infilled |> 
  format_emissions_variables() |> 
  select(model,scenario,region,variable,unit,year,value,
    ca.version)


## Climate variables --------------------------------------------------------
climate.formatted <- climate.timeseries.gsat |> 
  bind_rows(climate.timeseries.erf) |> 
  format_climate_variables() |> 
  select(model,scenario,region,variable,unit,year,value,
    ca.version)
# View(climate.formatted)


# Formatting metadata ----

## Functions and constants ----
format_warming_metadata <- function(df){
  df |> 
    # ensure model is full model name
    mutate(model = full.model.name) |> 
    
    # rename variable, including prefix, and percentile into variable name (currently working for p33, p50, p67)
    # - peak warming
    mutate_cond(((metric == "max") & (variable == "Surface Temperature (GSAT)") & (quantile == 0.33)), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Peak Warming|", as.character(round(quantile*100, 2)), "rd Percentile")) |>
    mutate_cond(((metric == "max") & (variable == "Surface Temperature (GSAT)") & (quantile == 0.50)), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Peak Warming|", "Median")) |>
    mutate_cond(((metric == "max") & (variable == "Surface Temperature (GSAT)") & (quantile == 0.67)), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Peak Warming|", as.character(round(quantile*100, 2)), "th Percentile")) |> 
    # - warming in 2100
    mutate_cond(((metric == "2100") & (variable == "Surface Temperature (GSAT)") & (quantile == 0.33)), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Warming in 2100|", as.character(round(quantile*100, 2)), "rd Percentile")) |>
    mutate_cond(((metric == "2100") & (variable == "Surface Temperature (GSAT)") & (quantile == 0.50)), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Warming in 2100|", "Median")) |>
    mutate_cond(((metric == "2100") & (variable == "Surface Temperature (GSAT)") & (quantile == 0.67)), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Warming in 2100|", as.character(round(quantile*100, 2)), "th Percentile")) |> 
    # add climate model in variable name
    mutate(variable = paste0(variable, " [", climate_model, ", °C]")) |>  
    
    # unit
    mutate_cond(unit == "K", unit = "°C") %>%
    return()
}
format_exceedance_metadata <- function(df){
  df |> 
    # ensure model is full model name
    mutate(model = full.model.name) |> 
    
    # rename variable, including prefix, and percentile into variable name (currently working for p33, p50, p67)
    # - peak warming
    mutate_cond((variable == "Exceedance probability"), 
                variable = paste0(CLIMATE_VARIABLE_PREFIX, "Exceedance Probability ", format(round(threshold, digits=1), nsmall=1), "°C")) |>
    # add climate model in variable name
    mutate(variable = paste0(variable, " [", climate_model, ", %]")) %>% 
    
    return()
}


## Warming indicators ----
metadata.warming.formatted <- metadata.warming |> 
  format_warming_metadata() |> 
  select(model,scenario,variable,unit,value,ca.version) |> 
  select(-unit) |> # drop unit for metadata
  pivot_wider(names_from = variable, values_from = value)

# View(metadata.warming.formatted)

## Exceedance probabilities ----
metadata.exceedance.formatted <- metadata.exceedance |> 
  format_exceedance_metadata() |> 
  select(model,scenario,variable,unit,value,ca.version) |>  
  select(-unit) |> # drop unit for metadata
  pivot_wider(names_from = variable, values_from = value)


# Combine and write out ----

## Combine ----
### Data ----
combined.formatted <- infilled.formatted |> 
  bind_rows(climate.formatted)
### Metadata ----
metadata.combined.formatted <- metadata.warming.formatted |> 
  left_join(metadata.exceedance.formatted, by = c("model", "scenario", "ca.version"))

##  Rename scenarios and select scenarios ----
### Data ----
if (VERSION_RELEASE_SCENARIOMIP == "v0.1") {
  combined.formatted.filtered <- combined.formatted |> 
    fix_scenario_names() |> 
    filter(!is.na(new_scenario_name)) |> 
    filter(
      new_scenario_name != "Medium-to-Low - SSP2 (Marker)"
    )
} else {
  combined.formatted.filtered <- combined.formatted |> 
    fix_scenario_names() |> 
    filter(!is.na(new_scenario_name))
}


combined.formatted.filtered |> 
  distinct(model,scenario,new_scenario_name)

### Metadata ----
metadata.combined.formatted.filtered <- metadata.combined.formatted |> 
  fix_scenario_names() |> 
  filter(!is.na(new_scenario_name)) # |> 
  # only for v0.1 ML was not ready yet.
  # filter(
  #   new_scenario_name != "Medium-to-Low - SSP2 (Marker)" # not included in v0.1
  # )

metadata.combined.formatted.filtered |> 
  distinct(model,scenario,new_scenario_name)



## FINAL ADJUTSMENTS FORMATTING ----
final_formatting_fixes <- function(df, type="meta"){
  df <- df |> 
    mutate(scenario = new_scenario_name) |> select(-new_scenario_name) |> 
    rename(`Version Climate Assessment` = ca.version) 
  if(type=="data"){
    df <- df |> 
      select(-`Version Climate Assessment`) |>
      arrange(model,scenario,region,variable,unit,year) |> 
      pivot_wider(values_from=value, names_from=year)
  }

  return(df)
}

### Data ----
combined.formatted.filtered.iamc <- combined.formatted.filtered |> 
  final_formatting_fixes(type="data")

### Metadata ----
metadata.combined.formatted.filtered.iamc <- metadata.combined.formatted.filtered |> 
  final_formatting_fixes(type="meta")

### README ----
# copy_readme_sheet <- function(source_file, target_file) {
#   # Load source workbook
#   source_wb <- loadWorkbook(source_file)

#   if (!"README" %in% names(source_wb)) {
#     stop("Sheet 'README' not found in source workbook.")
#   }

#   # Read README from source
#   readme <- readWorkbook(
#     source_wb,
#     sheet = "README",
#     colNames = FALSE
#   )

#   # Read all sheets from target
#   target_sheets <- excel_sheets(target_file)

#   other_sheets <- lapply(
#     setdiff(target_sheets, "README"),
#     \(s) readWorkbook(target_file, sheet = s)
#   )
#   names(other_sheets) <- setdiff(target_sheets, "README")

#   # Rebuild workbook with README first
#   write.xlsx(
#     x = c(list(README = readme), other_sheets),
#     file = target_file,
#     overwrite = TRUE
#   )
# }
# Try:
# library(openxlsx) # newer version
# copy_readme_sheet <- function(source_file, target_file) {
#   # Load workbooks
#   source_wb <- loadWorkbook(source_file)
#   target_wb <- loadWorkbook(target_file)

#   if (!"README" %in% names(source_wb)) {
#     stop("Sheet 'README' not found in source workbook.")
#   }

#   # Remove existing README if present
#   if ("README" %in% names(target_wb)) {
#     removeWorksheet(target_wb, "README")
#   }

#   # Copy sheet
#   addWorksheet(target_wb, "README", position = 1)
#   copyWorksheet(
#     fromWorkbook = source_wb,
#     fromSheet = "README",
#     toWorkbook   = target_wb,
#     toSheet      = "README"
#   )

#   saveWorkbook(target_wb, target_file, overwrite = TRUE)
# }



## Save ----
dir.create(PUBLICATION_FOLDER, recursive = TRUE)
writexl::write_xlsx(
  x = list(
    data = combined.formatted.filtered.iamc,
    meta = metadata.combined.formatted.filtered.iamc
  ),
  path = FINAL_DATA_FILE 
)
# copy_readme_sheet(
#   source_file = README_FILE_PATH,
#   target_file = FINAL_DATA_FILE
# )

# Checks ----
library("testthat")
testthat::test_that(
  "No duplicates in data",{

  expect_equal(
    combined.formatted.filtered.iamc |> nrow(),
    combined.formatted.filtered.iamc |> distinct(model,scenario,region,variable,unit) |> nrow(),
    # tolerance = 1e-10,
    label = "Duplicates in data"
  )
}
)
testthat::test_that(
  "No duplicates in meta",{

  expect_equal(
    metadata.combined.formatted.filtered.iamc |> nrow(),
    metadata.combined.formatted.filtered.iamc |> distinct(model,scenario) |> nrow(),
    # tolerance = 1e-10,
    label = "Duplicates in meta"
  )
}
)



# Compare to previously published version (manual) ----
# v0.1
v_previous <- read_excel(here("data", "ca", 
            paste0("ScenarioMIP_emissions_marker_scenarios_", VERSION_RELEASE_SCENARIOMIP_PREVIOUS,".xlsx")), 
            sheet = "data") |> 
  iamc_wide_to_long() |> rename(value_old = value)
v_previous

# # v0.2
# v02 <- read_excel(here("data", "ca", 
#             "ScenarioMIP_emissions_marker_scenarios_v0.2.xlsx"), 
#             sheet = "data") |> 
#   iamc_wide_to_long()
# v02


new <- combined.formatted.filtered.iamc

## All ---- 
comparison <- new |> iamc_wide_to_long() |> rename(value_new = value) |> 
  left_join(v_previous, by = c("model", "scenario", "region", "variable", "unit", "year")) |> 
  mutate(difference = value_new - value_old) |> 
  mutate(facet_label = paste0(variable, "\n(", unit, ")"))
comparison

p.diff.all <- ggplot(comparison, aes(x=year)) +
  facet_grid(facet_label~scenario, scales="free_y") +
  
  geom_line(aes(y=value_new, linetype=VERSION_RELEASE_SCENARIOMIP, linewidth=VERSION_RELEASE_SCENARIOMIP, color=VERSION_RELEASE_SCENARIOMIP)) +
  geom_line(aes(y=value_old, linetype=VERSION_RELEASE_SCENARIOMIP_PREVIOUS, linewidth=VERSION_RELEASE_SCENARIOMIP_PREVIOUS, color=VERSION_RELEASE_SCENARIOMIP_PREVIOUS)) +
  
  scale_linetype_manual(breaks=c(VERSION_RELEASE_SCENARIOMIP_PREVIOUS,VERSION_RELEASE_SCENARIOMIP), values=c("dashed", "solid")) + 
  scale_linewidth_manual(breaks=c(VERSION_RELEASE_SCENARIOMIP_PREVIOUS,VERSION_RELEASE_SCENARIOMIP), values=c(1.1, 0.8)) +
  scale_color_manual(breaks=c(VERSION_RELEASE_SCENARIOMIP_PREVIOUS,VERSION_RELEASE_SCENARIOMIP), values=c("grey", "dodgerblue")) +
  labs(title=paste0("Comparison of ", VERSION_RELEASE_SCENARIOMIP, " vs ", VERSION_RELEASE_SCENARIOMIP_PREVIOUS, " values", x=NULL)) +
  # theme_jsk() + mark_history(sy=2025) + 
  theme(strip.text.y = element_text(angle = 0))
# p.diff.all

save_ggplot(
  p = p.diff.all,
  f = file.path(PUBLICATION_FOLDER, "fig_compared_to_previous"),
  h = 2000,
  w = 600,
  format = "png",
  limitsize= FALSE
)

# add diff
p.diff.all.diff <- ggplot(comparison, aes(x = year)) +
  facet_grid(facet_label ~ scenario, scales = "free_y") +
  geom_rect(
    data = data.frame(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "green",
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_rect(
    data = data.frame(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "red",
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = difference)) +
  labs(
    title = paste0("Comparison of ", VERSION_RELEASE_SCENARIOMIP, " vs ", VERSION_RELEASE_SCENARIOMIP_PREVIOUS, " values"),
    x = NULL,
    y = "New - old"
  ) +
  theme(strip.text.y = element_text(angle = 0))
# p.diff.all.diff

save_ggplot(
  p = p.diff.all.diff,
  f = file.path(PUBLICATION_FOLDER, "fig_compared_to_previous_diff"),
  h = 2000,
  w = 600,
  format = "png",
  limitsize= FALSE
)

## Key variables ---- 
comparison.key <- comparison |> filter(
  variable %in% c(
    "Climate Assessment|Surface Temperature (GSAT)|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Harmonized and Infilled|Emissions|VOC",
    "Climate Assessment|Harmonized and Infilled|Emissions|Sulfur",
    "Climate Assessment|Harmonized and Infilled|Emissions|OC",
    "Climate Assessment|Harmonized and Infilled|Emissions|NOx",
    "Climate Assessment|Harmonized and Infilled|Emissions|NH3",
    "Climate Assessment|Harmonized and Infilled|Emissions|N2O",
    "Climate Assessment|Harmonized and Infilled|Emissions|Kyoto GHG AR6GWP100",
    "Climate Assessment|Harmonized and Infilled|Emissions|GHG AR6GWP100",
    "Climate Assessment|Harmonized and Infilled|Emissions|CO2|Energy and Industrial Processes",
    "Climate Assessment|Harmonized and Infilled|Emissions|CO2|AFOLU",
    "Climate Assessment|Harmonized and Infilled|Emissions|CO2",
    "Climate Assessment|Harmonized and Infilled|Emissions|CO",
    "Climate Assessment|Harmonized and Infilled|Emissions|CH4",
    "Climate Assessment|Harmonized and Infilled|Emissions|BC",
    "Climate Assessment|Harmonized and Infilled|Cumulative Emissions|CO2",
    "Climate Assessment|Effective Radiative Forcing|Ozone|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|N2O|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|Greenhouse Gases|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|F-Gases|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|CO2|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|CH4|Median [MAGICCv7.6.0a3]",
    "Climate Assessment|Effective Radiative Forcing|Aerosols|Median [MAGICCv7.6.0a3]"
  )
)

p.diff.key <- ggplot(comparison.key, aes(x=year)) +
  facet_grid(facet_label~scenario, scales="free_y") +
  
  geom_line(aes(y=value_new, linetype=VERSION_RELEASE_SCENARIOMIP, linewidth=VERSION_RELEASE_SCENARIOMIP, color=VERSION_RELEASE_SCENARIOMIP)) +
  geom_line(aes(y=value_old, linetype=VERSION_RELEASE_SCENARIOMIP_PREVIOUS, linewidth=VERSION_RELEASE_SCENARIOMIP_PREVIOUS, color=VERSION_RELEASE_SCENARIOMIP_PREVIOUS)) +
  
  scale_linetype_manual(breaks=c(VERSION_RELEASE_SCENARIOMIP_PREVIOUS,VERSION_RELEASE_SCENARIOMIP), values=c("dashed", "solid")) + 
  scale_linewidth_manual(breaks=c(VERSION_RELEASE_SCENARIOMIP_PREVIOUS,VERSION_RELEASE_SCENARIOMIP), values=c(1.1, 0.8)) +
  scale_color_manual(breaks=c(VERSION_RELEASE_SCENARIOMIP_PREVIOUS,VERSION_RELEASE_SCENARIOMIP), values=c("grey", "dodgerblue")) +
  labs(title=paste0("Comparison of ", VERSION_RELEASE_SCENARIOMIP, " vs ", VERSION_RELEASE_SCENARIOMIP_PREVIOUS, " values", x=NULL)) +
  # theme_jsk() + mark_history(sy=2025) + 
  theme(strip.text.y = element_text(angle = 0))


save_ggplot(
  p = p.diff.key,
  f = file.path(PUBLICATION_FOLDER, "fig_compared_to_previous_keyvars"),
  h = 1100,
  w = 600,
  format = "png",
  limitsize= FALSE
)

# add diff
p.diff.key.diff <- ggplot(comparison.key, aes(x = year)) +
  facet_grid(facet_label ~ scenario, scales = "free_y") +
  geom_rect(
    data = data.frame(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "green",
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_rect(
    data = data.frame(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "red",
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_line(aes(y = difference)) +
  labs(
    title = paste0("Comparison of ", VERSION_RELEASE_SCENARIOMIP, " vs ", VERSION_RELEASE_SCENARIOMIP_PREVIOUS, " values"),
    x = NULL,
    y = "New - old"
  ) +
  theme(strip.text.y = element_text(angle = 0))


save_ggplot(
  p = p.diff.key.diff,
  f = file.path(PUBLICATION_FOLDER, "fig_compared_to_previous_diff_keyvars"),
  h = 1100,
  w = 600,
  format = "png",
  limitsize= FALSE
)
