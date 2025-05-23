# tbd.

#' see: marker criteria doc - https://docs.google.com/document/d/1fvSvrvkqWBafIiMWw267twZcxTdbiDHa/edit
#' see: Zeb's notes - https://docs.google.com/document/d/1yf6wmasy4-c8WffoeootX1ejWH5nMKhrnsxby0z-t2Q/edit?tab=t.mzf9wzit3in
#' ...
#' TODO:
#' - net-zero function: 0.2Gt crossing limit (instead of zero); check IPCC
#' - add in priorities for each team
#'
#' TODO (later):
#' - ultimately what matter is the harmonized data (currently using native reported); when we're a bit further in the process []
#'
#'

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

# PATH ----
MARKER.ANALYSIS.FOLDER <- here("data", "marker selection")

# FILES ----

EMISSIONS.FILE <- "scenarios_scenariomip_emissions_2025-05-06.csv"

MAGICC.FOLDER <- "20250510_magicc_db_csv"
MAGICC.FILE <- "20250510_magicc_percentiles.csv"

PRIORITIES.FILE <- "20250523_MarkerPriorities.xlsx"


# THRESHOLDS ----



# utils ------------------------------------------------------------------------

## Flatten pandas multiindex ----
# flatten_multiindex_csv <- function(file_path, id_cols=c("climate_model", "model", "region", "scenario", "unit", "variable"), mi_cols=c("metric", "quantile")) {
#   # Read raw file without column names
#   raw <- read_csv(file_path,
#                   #"C:/Users/kikstra/Documents/GitHub/scenariomip/data/marker selection/climate/warming-quantiles_AIM 3.0.csv",
#                   col_names = FALSE, skip_empty_rows = FALSE)
#
#   # Step 1: Drop first row if it only contains "value"
#   if (all(raw[1, ] == "value", na.rm = TRUE)) {
#     raw <- raw[-1, ]
#   }
#
#   # Step 2: Identify rows with text in column A and B
#   a_text_rows <- which(!is.na(raw[[1]]) & raw[[1]] != "")
#   b_text_rows <- which(!is.na(raw[[1]]) & is.na(raw[[2]]))
#
#   # The last such row defines the column names (starting at B)
#   colname_row <- max(b_text_rows)
#
#   # Build actual column names
#   left_part <- id_cols
#   metric_info <- raw[seq_len(colname_row - 1), 1, drop = TRUE] |>
#     unlist(use.names = FALSE)
#   metric_info <- metric_info[metric_info != ""]
#   right_part <- mi_cols
#   full_names <- c(left_part, right_part) #, as.character(raw[colname_row, -(1:length(left_part))]))
#
#   # Step 3: Extract data starting from colname_row + 1
#   data <- raw[(colname_row + 1):nrow(raw), ]
#
#
#   # HERE IT BREAKS
#
#   # Assign column names
#   colnames(data) <- full_names
#
#   # Step 4: Pivot to long format
#   data_long <- data |>
#     pivot_longer(
#       cols = -(1:(length(left_part) + length(right_part))),
#       names_to = "year",
#       values_to = "value"
#     ) |>
#     mutate(
#       year = as.integer(year),
#       across(everything(), ~ ifelse(. == "", NA, .))
#     ) |>
#     drop_na(value)
#
#   return(data_long)
# }

flatten_multiindex_csv_new <- function(file_path,
                                   id_cols = NULL, #c("climate_model", "model", "scenario"),
                                   mi_cols = NULL, #c("metric"),
                                   collapse_string="__") {
  # Load raw data
  raw <- read_csv(file_path,
                  col_names = FALSE, skip_empty_rows = FALSE)

  # raw <- read_csv(#file_path,
  #   "C:/Users/kikstra/Documents/GitHub/scenariomip/data/marker selection/climate/warming-quantiles_AIM 3.0.csv",
  #   col_names = FALSE, skip_empty_rows = FALSE)
  # id_cols <- c("climate_model", "model", "region", "scenario", "unit", "variable")
  # mi_cols <- c("metric", "quantile")
  #
  # raw <- read_csv(#file_path,
  #   "C:/Users/kikstra/Documents/GitHub/scenariomip/data/marker selection/climate/categories_AIM 3.0.csv",
  #   col_names = FALSE, skip_empty_rows = FALSE)
  # id_cols <- c("climate_model", "model", "scenario")
  # mi_cols <- c("metric")

  print(id_cols)
  print(mi_cols)


  # Drop first row if it only contains "value"
  if (all(raw[1, ] == "value", na.rm = TRUE)) {
    raw <- raw[-1, ]
  }

  # Identify structure
  b_text_rows <- which(!is.na(raw[[1]]) & is.na(raw[[2]]))
  colname_row <- max(b_text_rows)

  # Pull out how many metadata rows (i.e. rows contributing to each column name)
  header_rows <- raw[1:colname_row, ]

  # Identify where data starts
  data <- raw[(colname_row + 2):nrow(raw), ] # adjusted
  # data <- raw[(colname_row + 1):nrow(raw), ] # original
  colnames(data)[1:length(id_cols)] <- id_cols

  # Build full column names by collapsing metadata rows for each column
  col_blocks <- (length(id_cols) + 1):ncol(data)

  new_colnames <- map_chr(col_blocks, function(i) {
    header_values <- header_rows[[i]]
    header_values <- header_values[!is.na(header_values) & header_values != ""]
    paste(header_values, collapse = collapse_string)
  })

  # Assign column names to data
  colnames(data) <- c(id_cols, new_colnames)

  # Pivot to long format
  if(length(mi_cols)==1){
    data_long <- data |>
      pivot_longer(
        cols = all_of(new_colnames),
        names_to = as.character(mi_cols),
        values_to = "value"
      ) %>%
      drop_na(value)
  } else {
    data_long <- data |>
      pivot_longer(
        cols = all_of(new_colnames),
        names_to = "measurement",
        values_to = "value"
      ) |>
      separate(
        col = measurement,
        into = mi_cols,#c("metric", "quantile", "year"),
        sep = collapse_string,
        convert = TRUE
      ) |>
      # mutate(
      #   source_file = basename(file_path)
      # ) |>
      drop_na(value)
  }

  return(data_long)
}
## Calculate percentiles ----
compute_percentiles <- function(data, id_cols = c("climate_model", "model", "region", "scenario", "unit", "variable"),
                                years = "all",
                                probs = c(0.33, 0.5, 0.67)) {
  # Identify numeric year-like columns
  all_years <- suppressWarnings(
    names(data)[sapply(names(data), function(x) !is.na(as.numeric(x)))]
  )

  # Select subset of years
  if (!identical(years, "all")) {
    years <- as.character(years)
    year_cols <- intersect(years, all_years)
    if (length(year_cols) == 0) {
      stop("None of the specified years are valid column names in the dataset.")
    }
  } else {
    year_cols <- all_years
  }

  # Named quantile functions (e.g. list(p25 = ~ quantile(x, 0.25)))
  percentile_labels <- paste0("p", probs * 100)
  quantile_funs <- set_names(probs, percentile_labels) |>
    purrr::map(~ \(x) quantile(x, .x, na.rm = TRUE))

  # Group by multiple ID columns
  data <- data |>
    dplyr::group_by(across(all_of(id_cols))) |>
    dplyr::summarise(
      dplyr::across(
        all_of(year_cols),
        quantile_funs,
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols = -all_of(id_cols),
      names_to = c("year", "percentile"),
      names_sep = "_",
      names_transform = list(year = as.integer)
    )

  return(data)
}

## Load multiple files ----
# install.packages("fs")
library("fs") # for using Unix-style globs in dir_ls like `"*.csv"`
# install.packages("glue")
library("glue") # for pasting like `glue("Loaded {length(all_files)} files.")`
load_multiple_files <- function(folder.path,
                                iamc=TRUE,
                                pattern=NULL,
                                filetype="csv",
                                upper.to.lower=F,
                                pandas.multiindex=F,
                                id.cols=NULL,
                                mi.cols=NULL,
                                magicc.percentiles.calculation=F,
                                ...){

  # Get files matching the extension
  all_files <- dir_ls(path = folder.path,
                      glob = paste0("*.", filetype))

  # Optionally filter by pattern
  if (!is.null(pattern)) {
    all_files <- all_files[str_detect(path_file(all_files), fixed(pattern))]
  }

  # Read and bind based on file type
  if (iamc==FALSE){
    if (pandas.multiindex==FALSE){
      if(magicc.percentiles.calculation==FALSE){
        df <- switch(
          filetype,
          "csv" = map_dfr(all_files, vroom), # bind using purr::map_dfr
          "xlsx" = map_dfr(all_files, read_excel),# bind using purr::map_dfr
          stop(glue::glue("Unsupported file type: {filetype}"))
        )
      } else {
        df <- NULL
        for (f in all_files){

          df.f <- vroom(f) %>%
            drop_na(run_id) %>% # drop IAM data like emissions etc.
            # filter(variable%nin%c(
            #   # drop some unnecessary variables
            #   "Atmospheric Concentrations|CH4",
            #   "Atmospheric Concentrations|CO2",
            #   "Atmospheric Concentrations|N2O",
            #   "CO2_CURRENT_NPP",
            #   "Surface Air Ocean Blended Temperature Change",
            #   "Effective Radiative Forcing|Aerosols|Direct Effect",
            #   "Effective Radiative Forcing|Aerosols|Indirect Effect",
            #   "Effective Radiative Forcing|Ozone",
            #   "Effective Radiative Forcing|Solar",
            #   "Effective Radiative Forcing|Stratospheric Ozone",
            #   "Effective Radiative Forcing|Tropospheric Ozone",
            #   "Effective Radiative Forcing|Volcanic",
            #   "Heat Uptake",
            #   "Heat Uptake|Ocean"
            # )) %>%
            filter(climate_model=="MAGICCv7.6.0a3") %>%
            filter(variable%in%c(
              "Effective Radiative Forcing|Aerosols",
              "Effective Radiative Forcing|CO2",
              "Effective Radiative Forcing|Greenhouse Gases",
              "Surface Air Temperature Change", # raw GST variable in MAGICC
              "Surface Temperature (GSAT)" # assessed temps after rescaling history
            )) %>%
            compute_percentiles(years = as.character(2015:2100))

          df <- df %>%
            bind_rows(
              df.f
            )
        }
      }

    } else {

      df <- switch(
        filetype,
        "csv" = map_dfr(all_files, ~flatten_multiindex_csv_new(file_path = .x, id_cols = id.cols, mi_cols = mi.cols)), # bind using purr::map_dfr
        stop(glue::glue("Unsupported file type: {filetype}"))
      )
    }

  } else {
    df <- switch(
      filetype,
      "csv" = map_dfr(all_files, load_csv_iamc), # bind using purr::map_dfr
      "xlsx" = map_dfr(all_files, load_excel_iamc),# bind using purr::map_dfr
      stop(glue::glue("Unsupported file type: {filetype}"))
    )
    df <- df %>% iamc_wide_to_long(upper.to.lower = upper.to.lower)
  }



  # Optional: print summary
  print(glue::glue("Loaded {length(all_files)} files. Total rows: {nrow(df)}"))

  return(df)
}

### ScenarioMIP specific ----
add_scenariomip_info_columns <- function(df){
  return(
    df %>%
      add_scenariomip_targets_to_IAM_scenarios() %>%
      add_ssp_basis_to_IAM_scenarios() %>%
      simplify_model_names(keep.full.model.name = T)
  )
}


# New Harmonization workflow data ----------------------------------------------

## Load data -------------------------------------------------------------------

### Preferences ----------------------------------------------------------------
preferences <- read_excel(path = file.path(MARKER.ANALYSIS.FOLDER, "preferences",
                                           PRIORITIES.FILE),
                          sheet = "data") %>%
  pivot_longer(
    cols = -model,
    names_to = "target",
    values_to = "priority"
  ) %>%
  simplify_model_names(keep.full.model.name = F)

### IAM data -------------------------------------------------------------------

#### Emissions -----------------------------------------------------------------
emissions <- load_csv_iamc(file.path(MARKER.ANALYSIS.FOLDER, "emissions",
                                EMISSIONS.FILE),
                           mode = "fast") %>%
  filter(Region=="World") %>%
  filter_starts_with(column.name = "Variable",
                     "Emissions") %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  add_scenariomip_info_columns()

emissions.regional <- NA

#### Emissions (harmonized) ----------------------------------------------------
# Download from: https://iiasahub.sharepoint.com/sites/eceprog/Shared%20Documents/Forms/AllItems.aspx?FolderCTID=0x012000AA9481BF7BE9264E85B14105F7F082FF&id=%2Fsites%2Feceprog%2FShared%20Documents%2FProjects%2FCMIP7%2FIAM%20Data%20Processing%2FScenarioMIP%20Emulator%20workflow%2FApril%2011%20submission%2Fscm%2Doutput%2F0005%5F0002%5F0002%5F0002%5F0002%5F0002%5F0003%5F0002%5F0002%5F0002%5F0002%5F95b5f2c9fb62e32a4d08fe2ffc5b4a6ff246ad2d%5F0003%5F0003%5F0002

#### Primary Energy Biomass ----------------------------------------------------
# pe.biomass <- load_csv_iamc(file.path(MARKER.ANALYSIS.FOLDER, "emissions",
#                                      "scenarios_scenariomip_allmodels_2025-05-06.csv"), # not currently downloaded
#                            mode = "fast") %>%
#   filter(Region=="World",
#          Variable=="Primary Energy|Biomass") %>%
#   iamc_wide_to_long(upper.to.lower = T) %>%
#   add_scenariomip_info_columns()

### Climate --------------------------------------------------------------------

PATH.CLIMATE.DATA <- file.path(MARKER.ANALYSIS.FOLDER, "climate")

#### Warming indications -------------------------------------------------------
warming <- load_multiple_files(folder.path = PATH.CLIMATE.DATA,
                          iamc = F,
                          pandas.multiindex = T,
                          id.cols=c("climate_model", "model", "region", "scenario", "unit", "variable"),
                          mi.cols=c("metric", "quantile"),
                          pattern = "warming-quantiles") %>%
  add_scenariomip_info_columns()

#### Categories ----------------------------------------------------------------
categories <- load_multiple_files(folder.path = PATH.CLIMATE.DATA,
                               iamc = F,
                               pandas.multiindex = T,
                               id.cols=c("climate_model", "model", "scenario"),
                               mi.cols=c("metric"),
                               pattern = "categories") %>%
  add_scenariomip_info_columns()

#### ALL DATA (timeseries) -----------------------------------------------------

##### First-time only: process raw data ----------------------------------------
###### Load all data and calculate percentiles ---------------------------------
climate.timeseries <- load_multiple_files(
  folder.path = file.path(PATH.CLIMATE.DATA, MAGICC.FOLDER),
  iamc = F, # because it has a climate model column, and a run_id column
  magicc.percentiles.calculation = T
)

###### Save percentiles --------------------------------------------------------
write_delim(
  x = climate.timeseries,
  file = file.path(PATH.CLIMATE.DATA, MAGICC.FILE),
  delim = ","
)

##### Load percentiles ---------------------------------------------------------
climate.timeseries <- read_csv(
  file.path(PATH.CLIMATE.DATA, MAGICC.FILE)
) %>%
  add_scenariomip_info_columns()


climate.timeseries %>% filter(target=="VLHO",
                              climate_model=="MAGICCv7.6.0a3",
                              model=="MESSAGE",
                              variable=="Surface Temperature (GSAT)",
                              percentile=="p50") %>%
  plot_standard_line_one_region()





###### New/Overriding data -----------------------------------------------------

####### What to maintain from data above ----
emissions <- emissions %>% filter(model%nin%c("AIM", "WITCH"))
climate.timeseries <- climate.timeseries %>% filter(
  model%nin%c("AIM", "WITCH"),
  variable!="Surface Air Temperature Change"
)
warming <- warming %>% filter(model%nin%c("AIM", "WITCH"))
categories <- categories %>% filter(model%nin%c("AIM", "WITCH"))

####### GSAT additional from zeb for run above -----
climate.timeseries.gsat <- tibble()
for (
  m in climate.timeseries %>% pull(model) %>% unique()
){
  climate.timeseries.gsat <- climate.timeseries.gsat %>%
    bind_rows(
      load_multiple_files(
        folder.path = file.path(PATH.CLIMATE.DATA, MAGICC.FOLDER, "additional_data", m),
        pattern = "assessed-warming",
        iamc = F # because it has a climate model column, and a run_id column
      ) %>%
        pivot_longer(cols = `2000`:`2100`,
                     names_to = "year",
                     values_to = "value") %>%
        mutate(year=as.numeric(year)) %>%
        filter(quantile%in%c(0.33,0.5,0.67)) %>%
        mutate(percentile = paste0("p",round(x = quantile*100, digits = 0))) %>%
        select(-quantile) %>%
        add_scenariomip_info_columns()

    )
}


####### AIM and WITCH rerun -----
emissions.witch.aim <- load_multiple_files(
  folder.path = file.path(PATH.CLIMATE.DATA,
                          "20250521_AIM_WITCH-rerun",
                          "global-emissions"),
  iamc = T, upper.to.lower = T,
  filetype = "xlsx",
) %>%
  add_scenariomip_info_columns()

climate.timeseries.witch.aim <- load_multiple_files(
  folder.path = file.path(PATH.CLIMATE.DATA,
                          "20250521_AIM_WITCH-rerun",
                          "output_csvs"),
  iamc = F,
  magicc.percentiles.calculation = T
) %>% add_scenariomip_info_columns()

# compare
compare.climate.timeseries <- climate.timeseries %>%
  filter(model%in%c("AIM", "WITCH")) %>% mutate(version = "early May") %>%
  bind_rows(
    climate.timeseries.witch.aim %>% mutate(version = "mid May")
  )

###### Replace older data with New/Overriding data -----------------------------
climate.timeseries <- climate.timeseries %>%
  bind_rows(climate.timeseries.witch.aim) %>% # add new witch and aim
  bind_rows(climate.timeseries.gsat) %>%  # add GSAT for original runs, too
  distinct() # prevent duplication
emissions <- emissions %>%
  bind_rows(emissions.witch.aim) %>% # add new witch and aim
  distinct() # prevent duplication





## Load filters ----------------------------------------------------------------

scenario.list <- climate.timeseries %>% distinct(model,scenario)
scenario.list
nrow(scenario.list)

#' NOTE: THINGS DONE TO KEEP A FEW MORE OPTIONS [enabling team's priority 1 target markers]:
#' 1. EOC GHGs up to 8Gt away from NZ allowed to keep MESSAGE L SSP2 in
#' 2. Max. net-negative CO2 to -10e3 instead of -7e3 to keep REMIND VLLO in
#' 3. VLLO peak temp (p33<1.62) -> should aim to go down towards as close as 1.5 as possible
#'
#' ...
#' ...

filter_criteria <- function(df.emissions,
                            df.warming,
                            df.categories,
                            df.climate.timeseries,
                            allowance.strictness="strict"){

  if (allowance.strictness=="strict"){

    allowance.ghg <- 2e3 # MtCO2eq allowance (should eventually go to zero?)
    allowance.co2 <- 2e3 # MtCO2 allowance (should eventually go to zero?)

    allowance.nz.co2 <- 1.5e3 # should eventually go to 200 MtCO2 to be considered net-zero CO2

    allowance.temp <- 0.075 # in K; to other function?


  }

  # apply filters

  ### H ----
  #' H
  #' Description
  #' - plausible highest emissions trajectory
  #' Quantification
  #' - no quantification (emissions here are not defensible as a criterion)
  h <- df.emissions %>% filter(target=="H") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria = 1, # no emissions or climate related filters for H, as this is a research question
      .groups = "drop"
    )

  ### M ----
  #' M
  #' Description
  #' - flat line, with a certain temperature outcome
  #' Quantification
  #' - 40 < GHG < 70 (at any point in time)
  #'
  m.ghg <- df.emissions %>% filter(year<=2100,variable=="Emissions|Kyoto Gases",target=="M") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg = if (any(value <= 40e3)|any(value >= 70e3)) 0 else 1,
      .groups = "drop"
    )
  m <- m.ghg %>%
    mutate(criteria = pmin(criteria.emissions.ghg,
                           na.rm = T))

  ### ML ----
  #' ML
  #' Quantification
  #' - no negative GHG before 2100
  #' - near-zero CO2 in 2100 (+-2GtCO2)
  #' - Temperature should start to stabilise
  #' - current policies until 2040 (NOT ASSESSED) - medium group will assess; will need some allowance, too
  #'
  ml.ghg <- df.emissions %>% filter(year<=2100,variable=="Emissions|Kyoto Gases",target=="ML") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg = if (any(value <= -allowance.ghg)) 0 else 1,
      .groups = "drop"
    )
  ml.co2 <- df.emissions %>% filter(year==2100,variable=="Emissions|CO2",target=="ML") %>%
    group_by(model,scenario) %>%
    summarise(
      #' Notes:
      #' - +-2 GtCO2 is maybe too lenient, but OK for now.
      criteria.emissions.co2 = if ((abs(value) < allowance.co2)) 1 else 0,
      .groups = "drop"
    )
  ml.temp <- df.climate.timeseries %>% filter(target=="ML",
                                          climate_model=="MAGICCv7.6.0a3",
                                          variable=="Surface Air Temperature Change",
                                          percentile=="p50",
                                          year%in%c(2090,2100)) %>%
    iamc_long_to_wide() %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.temp.stabilisation = if ((abs(`2100`-`2090`) < 0.075)) 1 else 0,
      .groups = "drop"
    )
  ml <- ml.co2 %>% left_join(ml.ghg) %>% left_join(ml.temp) %>%
    mutate(criteria = pmin(criteria.emissions.co2,
                           criteria.emissions.ghg,
                           criteria.temp.stabilisation,
                           na.rm = T))

    ### L ----
  #' L
  #' - no net-zero GHG before 2080 (too early)
  #' - close to net-zero GHG in 2100 (less than +-5GtCO2eq) # NOTE CURRENTLY IMPLEMENTED AS 8 Gt, THIS SHOULD GO DOWN
  #' - net-zero CO2 around 2070 (2065-2075)
  #' - higher than VLLO: 2100 p50 temp 1.65C or higher (no allowance)
  #' - likely below 2C: 2100 p67 temp 2C or low (with small allowance; 0.05K)
  #'
  #' Not assessed:
  #' - needs to be C3 (NOT ASSESSED DIRECTLY - too tight right now to do directly)
  #'
  #' Notes:
  #' - is likely 2C scenario; thus C3, net-zero GHG
  #' Add:
  #'
  #'
  #'
  l.ghg.nz.time <- df.emissions %>% filter(variable=="Emissions|Kyoto Gases",target=="L") %>%
    estimate_net_zero(var = "Emissions|Kyoto Gases",keep_only_first_crossing_year = T,
                      threshold_value = allowance.nz.co2) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg.nz.time = if ((net_zero_year <= 2080)) 0 else 1,
      .groups = "drop"
    )
  l.ghg.nz.time <- df.emissions %>% filter(variable=="Emissions|Kyoto Gases",target=="L") %>%
    distinct(model,scenario) %>%
    left_join(l.ghg.nz.time) %>%
    mutate_cond(is.na(criteria.emissions.ghg.nz.time),
                criteria.emissions.ghg.nz.time=1) # no net-zero at all before 2100 is not before 2080
  l.ghg.eoc.nz <- df.emissions %>% filter(variable=="Emissions|Kyoto Gases",target=="L",year==2100) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg.nz.eoc = if ((abs(value) <= 8e3)) 1 else 0,
      .groups = "drop"
    )
  l.co2.nz <- df.emissions %>% filter(variable=="Emissions|CO2",target=="L") %>%
    estimate_net_zero(var = "Emissions|CO2",keep_only_first_crossing_year = T,
                      threshold_value = allowance.nz.co2) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.co2.nz = if ((net_zero_year >= 2065)&(net_zero_year <= 2075)) 1 else 0,
      .groups = "drop"
    )
  l.eoc.p50 <- df.warming %>% filter(quantile==0.5,metric=="2100",target=="L") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.climate.eoc.p50 = if ((value >= 1.65)) 1 else 0,
      .groups = "drop"
    )
  l.eoc.p67 <- df.warming %>% filter(quantile==0.67,metric=="max",target=="L") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.climate.eoc.p67 = if ((value <= 2 + 0.05)) 1 else 0,
      .groups = "drop"
    )

  l <- l.ghg.nz.time %>% left_join(l.ghg.eoc.nz) %>% left_join(l.co2.nz) %>% left_join(l.eoc.p50) %>% left_join(l.eoc.p67) %>%
    mutate(criteria = pmin(criteria.emissions.ghg.nz.time,
                           criteria.emissions.ghg.nz.eoc,
                           criteria.emissions.co2.nz,
                           criteria.climate.eoc.p50,
                           criteria.climate.eoc.p67,
                           na.rm = T))

  ### VLHO ----
  #' VLHO
  #' Quantifications:
  #' - peak p33 temp minimum > 1.64C (0.15 higher than optimal VLLO peak, which we assume should be around 1.5, noting that diff between p33 and p50 peaks is about 0.14K)
  #' - peak p50 temp minimum > 1.75C (~1.6 of desired VLLO peak + desired diff of at least 0.15K)
  #' - GHG strongly negative in 2100 (<-15GtCO2eq ~ -13 effectively)
  #' - NZ GHG between 2060 and 2080
  #'
  vlho.peak.p33 <- df.warming %>% filter(quantile==0.33,metric=="max",target=="VLHO") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.climate.peak.p33 = if ((value >= 1.5 + 0.14)) 1 else 0,
      .groups = "drop"
    )
  vlho.peak.p50 <- df.warming %>% filter(quantile==0.50,metric=="max",target=="VLHO") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.climate.peak.p50 = if ((value >= 1.6 + 0.15)) 1 else 0,
      .groups = "drop"
    )
  vlho.ghg.neg <- df.emissions %>% filter(year==2100,variable=="Emissions|Kyoto Gases",target=="VLHO") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg.neg = if (value < -13e3 + allowance.ghg) 1 else 0,
      .groups = "drop"
    )
  vlho.ghg.nz <- df.emissions %>% filter(variable=="Emissions|Kyoto Gases",target=="VLHO") %>%
    estimate_net_zero(var = "Emissions|Kyoto Gases",keep_only_first_crossing_year = T,
                      threshold_value = allowance.nz.co2) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg.nz = if ((net_zero_year >= 2060) & (net_zero_year <= 2080)) 1 else 0,
      .groups = "drop"
    )

  vlho <- vlho.peak.p33 %>% left_join(vlho.peak.p50) %>% left_join(vlho.ghg.neg) %>% left_join(vlho.ghg.nz) %>%
    mutate(criteria = pmin(criteria.climate.peak.p33,
                           criteria.emissions.ghg.neg,
                           criteria.emissions.ghg.nz,
                           na.rm = T))
  #' info: how much are 0.33 and 0.55 different?
  #' in VLLO and VLHO they are different by about 0.11-0.18K
  #' - VLLO: 0.135 mean
  #' - VLHO: 0.147 mean
  #' so, we use:
  #' => 0.14K
  # df.warming %>% filter(quantile%in%c(0.33,0.50),metric=="max",target%in%c("VLLO","VLHO")) %>%
  #   pivot_wider(names_from = quantile,
  #               values_from=value) %>%
  #   mutate(diff=as.numeric(`0.5`)-as.numeric(`0.33`)) %>%
  #   reframe(
  #     diff.mean = mean(diff),
  #     .by = c("target")
  #   )


  ### VLLO ----
  #' VLLO
  #' Quantifications
  #' - low peak temp (p33<1.62) # should aim to go down towards as close as 1.5 as possible
  #' - CO2 net-zero latest 2055
  #' - GHG net-zero around 2070
  #' - residual emissions: CO2 not too negative (at most -7Gt) at any point in time [not more negative than -7 CO2 -- ENGAGE had net-negative 5-10, 5 means 90% reductions and 10% offset]
  #' - GHG still net-zero (not too negative) in 2100 (NOT ASSESSED)
  #' - primary energy|biomass lower than 100 EJ (NOT ASSESSED)
  #' - demand-side: Final Energy lower than 450EJ/yr? (NOT ASSESSED)
  #'
  #' Not assessed:
  #' - C1 category (NOT ASSESSED DIRECTLY - too tight right now to do directly)
  #'
  #' check/add
  #' - gross emissions (residual emissions): not higher than 7 GtCO2 after 2060
  #'
  #' Question:
  #' - is Land negative emissions acceptable if land sustainability criteria are met? Probably not for a marker because it raises questions on residual emissions.
  #'
  vllo.peak <- df.warming %>% filter(quantile==0.33,metric=="max",target=="VLLO") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.climate.peak = if (value <= 1.62) 1 else 0,
      .groups = "drop"
    )
  vllo.co2.nz <- df.emissions %>% filter(variable=="Emissions|CO2",target=="VLLO") %>%
    estimate_net_zero(var = "Emissions|CO2", keep_only_first_crossing_year = T,
                      threshold_value = allowance.nz.co2) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.co2.nz = if ((net_zero_year <= 2055)) 1 else 0,
      .groups = "drop"
    )
  vllo.ghg.nz <- df.emissions %>% filter(variable=="Emissions|Kyoto Gases",target=="VLLO") %>%
    estimate_net_zero(var = "Emissions|Kyoto Gases", keep_only_first_crossing_year = T,
                      threshold_value = allowance.nz.co2) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.ghg.nz = if ((net_zero_year >= 2055) & (net_zero_year <= 2080)) 1 else 0,
      .groups = "drop"
    )
  # vllo.co2.nz <- df.emissions %>% filter(year<=2065,variable=="Emissions|CO2",target=="VLLO") %>%
  #   group_by(model,scenario) %>%
  #   summarise(
  #     criteria.emissions.co2.nz = if (any(value < 0 + allowance.co2)) 1 else 0,
  #     .groups = "drop"
  #   )
  vllo.co2.negative <- df.emissions %>% filter(year<=2100,variable=="Emissions|CO2",target=="VLLO") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.emissions.co2.negative = if (any(value < -10e3)) 0 else 1, # NOTE: SHOULD ACTUALLY BE -7e3 !!!
      .groups = "drop"
    )
  vllo <- vllo.peak %>% left_join(vllo.co2.nz) %>% left_join(vllo.ghg.nz) %>% left_join(vllo.co2.negative) %>%
    mutate(criteria = pmin(criteria.climate.peak,
                           criteria.emissions.co2.nz,
                           criteria.emissions.ghg.nz,
                           criteria.emissions.co2.negative,
                           na.rm = T))

  # combine targets
  df.scen.list <- h %>% bind_rows(m) %>% bind_rows(ml) %>% bind_rows(l) %>% bind_rows(vlho) %>% bind_rows(vllo) %>%
    select(model,scenario,criteria,everything()) %>%
    add_scenariomip_targets_to_IAM_scenarios() %>%
    add_ssp_basis_to_IAM_scenarios()

  return(df.scen.list)
}

filter_criteria_only_climate <- function(df.climate.timeseries){

  # apply filters

  ### H ----
  #' H
  #' Description
  #' - plausible highest emissions trajectory
  #' Quantification
  #' - no quantification (emissions here are not defensible as a criterion)
  h <- df.climate.timeseries %>% filter(target=="H") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria = 1, # no emissions or climate related filters for H, as this is a research question
      .groups = "drop"
    )

  ### M ----
  #' M
  #' Description
  #' - flat line, with a certain temperature outcome
  #' Quantification
  #' - 40 < GHG < 70 (at any point in time)
  #'
  m <- df.climate.timeseries %>% filter(target=="M") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria = 1, # no emissions or climate related filters for H, as this is a research question
      .groups = "drop"
    )

  ### ML ----
  #' ML
  #' Quantification
  #' - Temperature should start to stabilise
  #'
  ml.temp.stab <- df.climate.timeseries %>% filter(target=="ML",
                                              climate_model=="MAGICCv7.6.0a3",
                                              variable=="Surface Temperature (GSAT)",
                                              percentile=="p50",
                                              year%in%c(2090,2100)) %>%
    iamc_long_to_wide() %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.temp.stabilisation = if ((abs(`2100`-`2090`) < 0.075)) 1 else (
        if ((abs(`2100`-`2090`) < 0.1)) 2 else 3
      ),
      .groups = "drop"
    )
  ml.temp.paris <- df.climate.timeseries %>% filter(target=="ML",
                                                   climate_model=="MAGICCv7.6.0a3",
                                                   variable=="Surface Temperature (GSAT)",
                                                   percentile=="p67") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.fail.paris = if (!any(value > 2)) 1 else 3,
      .groups = "drop"
    )


  ml <- ml.temp.stab %>% left_join(ml.temp.paris) %>%
    mutate(criteria = pmax(criteria.temp.stabilisation,
                           criteria.fail.paris,
                           na.rm = T))

  ### L ----
  #' L
  #' - likely below 2C: 2100 p67 temp 2C or low (with small allowance; 0.05K)
  #'
  l.temp.paris <- df.climate.timeseries %>% filter(target=="L",
                                                     climate_model=="MAGICCv7.6.0a3",
                                                     variable=="Surface Temperature (GSAT)",
                                                     percentile=="p67") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.meet.paris = if (!any(value > 2)) 1 else (
        if (!any(value > 2.1)) 2 else 3
      ),
      .groups = "drop"
    )

  l <- l.temp.paris %>%
    mutate(criteria = pmax(criteria.meet.paris,
                           na.rm = T))

  ### VLHO ----
  #' VLHO
  #' Quantifications:
  #' - peak p33 temp minimum > 1.64C (0.15 higher than optimal VLLO peak, which we assume should be around 1.5, noting that diff between p33 and p50 peaks is about 0.14K)
  #' - peak p50 temp minimum > 1.75C (~1.6 of desired VLLO peak + desired diff of at least 0.15K)
  #'
  vlho.2100.p50 <- df.climate.timeseries %>% filter(target=="VLHO",
                                                    climate_model=="MAGICCv7.6.0a3",
                                                    variable=="Surface Temperature (GSAT)",
                                                    percentile=="p50") %>%
    filter(year==2100) %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.2100 = if (!any(value > 1.5)) 1 else (
        if (!any(value > 1.55)) 2 else 3
      ),
      .groups = "drop"
    )
  vlho.peak.p50 <- df.climate.timeseries %>% filter(target=="VLHO",
                                                    climate_model=="MAGICCv7.6.0a3",
                                                    variable=="Surface Temperature (GSAT)",
                                                    percentile=="p50") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.high.peak = if (any(value > 1.8)) 1 else (
        if (any(value > 1.75)) 2 else 3
      ),
      .groups = "drop"
    )

  vlho <- vlho.2100.p50 %>% left_join(vlho.peak.p50) %>%
    mutate(criteria = pmax(criteria.2100,
                           criteria.high.peak,
                           na.rm = T))



  ### VLLO ----
  #' VLLO
  #' Quantifications
  #' - low peak temp (p33<1.62) # should aim to go down towards as close as 1.5 as possible
  #'
  vllo.temp.paris.p50 <- df.climate.timeseries %>% filter(target=="VLLO",
                                                     climate_model=="MAGICCv7.6.0a3",
                                                     variable=="Surface Temperature (GSAT)",
                                                     percentile=="p50") %>%
    group_by(model,scenario) %>%
    summarise(
      criteria.p50 = if (!any(value > 1.65)) 1 else (
        if (!any(value > 1.75)) 2 else 3
      ),
      .groups = "drop"
    )
  vllo <- vllo.temp.paris.p50 %>%
    mutate(criteria = pmax(criteria.p50,
                           na.rm = T))

  # combine targets
  df.scen.list <- h %>% bind_rows(m) %>% bind_rows(ml) %>% bind_rows(l) %>% bind_rows(vlho) %>% bind_rows(vllo) %>%
    select(model,scenario,criteria,everything()) %>%
    add_scenariomip_targets_to_IAM_scenarios() %>%
    add_ssp_basis_to_IAM_scenarios()

  return(df.scen.list)
}

full.scenario.list <- emissions %>% distinct(model,scenario) %>%
  arrange(model,scenario)

write_delim(x = full.scenario.list %>% add_scenariomip_info_columns(),
            file = file.path(MARKER.ANALYSIS.FOLDER, "output", "all_scenarios.csv"),
            delim=",")

# Apply filters ----

## Climate and emissions (v20250519) ----
crit <- filter_criteria(df.emissions=emissions,
                        df.warming=warming,
                        df.categories=categories,
                        df.climate.timeseries=climate.timeseries)
crit <- full.scenario.list %>% left_join(crit)

## Save filter info
write_delim(x = crit,
            file = file.path(MARKER.ANALYSIS.FOLDER, "output", "crit.csv"),
            delim=",")

## Climate only (v20250523) ----
crit.climate <- filter_criteria_only_climate(df.climate.timeseries=climate.timeseries)
crit.climate <- full.scenario.list %>% left_join(crit.climate)

## Save filter info
write_delim(x = crit.climate,
            file = file.path(MARKER.ANALYSIS.FOLDER, "output", "crit_climate.csv"),
            delim=",")
# Visualise filtering outcomes ----
for (c in crit %>% select(starts_with("criteria")) %>% colnames() ){
  p.crit <- ggplot(crit, aes(x = interaction(scenario), y = model, fill = .data[[c]])) +
    facet_wrap(~target, scales="free", ncol=2) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "red", high = "green", na.value = "grey80") +
    labs(
      title = ifelse(c=="criteria", "Passing all emissions and climate rules", c),
      x = "Scenario",
      y = "Model"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_ggplot(
    p = p.crit,
    f = file.path(MARKER.ANALYSIS.FOLDER, "output", paste0("criteria_heatmap_", clean_string(c) ) ),
    h = 300,
    w = 350,
    format = "png", bg = 'white',
    unit = "mm"
  )
}





### Temperature ----------------------------------------------------------------


### Scenario-Set criteria ------------------------------------------------------

#' ML & M: same SSP (or ML:SSP2 & M:SSP3)
#' VLHO & VLLO: peak_VLHO - peak_VLLO >= 0.2
#' VLHO & VLLO: cumu_CO2_untilNZ_VLHO - cumu_CO2_untilNZ_VLLO >= 400GtCO2
#' ...: ...
#' ...: ...
#' ...: ...
#'


# Scenario Explorer data -------------------------------------------------------
# ...


# Find marker set combinations -------------------------------------------------
passing.scens <- crit %>% filter(criteria==1) %>%
  left_join(preferences) %>%
  select(model,scenario,target,ssp,priority)
# also drop out all options that have priority 5 or 6
passing.scens <- passing.scens %>%
  filter(priority<5)
# also drop out the alternatives
passing.scens <- passing.scens %>%
  filter(!grepl(x=scenario,pattern="_"))

passing.scens %>% group_by(target) %>% count()
passing.scens %>% group_by(model,target) %>% count()
passing.scens.wide.count <- passing.scens %>% group_by(model,target) %>% count() %>%
  pivot_wider(names_from = target,
              values_from = n)
passing.scens.wide.count
write_delim(x = passing.scens.wide.count,
            file = file.path(MARKER.ANALYSIS.FOLDER, "output", "passing_scens_count.csv"),
            delim=",")

# # Helper to build one valid 6-element set
# build_valid_set <- function(data, set_size = 6, max_attempts = 10000) {
#   attempts <- 0
#   results <- list()
#
#   while (attempts < max_attempts && length(results) < 1) {
#     attempts <- attempts + 1
#     candidate <- data[sample(nrow(data)), ]
#
#     set <- list()
#     used_targets <- character()
#     used_models <- character()
#
#     for (i in seq_len(nrow(candidate))) {
#       row <- candidate[i, ]
#       if (row$target %in% used_targets) next
#       if (row$model %in% used_models) next
#       set[[length(set) + 1]] <- row
#       used_targets <- c(used_targets, row$target)
#       used_models <- c(used_models, row$model)
#       if (length(set) == set_size) break
#     }
#
#     if (length(set) == set_size) {
#       results[[length(results) + 1]] <- bind_rows(set)
#     }
#   }
#
#   return(bind_rows(results, .id = "set_id"))
# }


## Greedy option ----
# Function to generate one valid set (stop at the first possible set)
get_valid_set <- function(pool, set_size = 6) {
  selected <- pool[0, ]
  for (i in seq_len(nrow(passing.scens))) {
    row <- passing.scens[i, ]
    if (row$target %in% selected$target) next
    if (row$model %in% selected$model) next
    selected <- bind_rows(selected, row)
    if (nrow(selected) == 6) return(selected)
  }
  return(NULL)
}

# Function to generate multiple disjoint sets
generate_sets <- function(data, set_size = 6, max_sets = Inf) {

  results <- list()

  repeat {
    data <- data[sample(nrow(data)), ] # sample anew to shuffle the order
    pool <- data
    set <- get_valid_set(pool, set_size)
    if (is.null(set)) break
    results[[length(results) + 1]] <- set
    # Remove used rows from pool
    pool <- anti_join(pool, set, by = c("model", "target", "scenario", "ssp"))
    if (length(results) >= max_sets) break
  }

  # Bind results and assign set IDs
  bind_rows(results, .id = "set_id")
}

# Run it
valid_sets <- generate_sets(passing.scens, set_size = 6)
valid_sets


## Full combinatorics ----
# install.packages("combinat")
library(combinat)

create_all_options <- function(df, set.size=6){
  # Get all combinations of 6 rows
  combs <- combn(nrow(df %>% select(model,scenario,target)), set.size, simplify = FALSE)
  length(combs) # takes a few seconds if combs is <10000 (expample nrow(passing.scens)=22 came with combs=74613)

  # Keep only those where:
  # - targets are unique
  # - models are unique
  valid_sets <- keep(combs, function(idx) {
    set <- df[idx, ]
    n_distinct(set$target) == set.size &&
      n_distinct(set$model) == set.size
  })

  valid_sets

  all.valid.ms.combinations <- tibble()
  ms.passing.scens <- df %>% distinct(model,scenario)
  for (i in 1:length(valid_sets)){
    set <- valid_sets[[i]]

    set.ms <- tibble()
    for (r in set){
      set.ms <- set.ms %>%
        bind_rows(
          ms.passing.scens %>% filter(row_number()==r) %>%
            mutate(option=i)
        )
    }


    all.valid.ms.combinations <- all.valid.ms.combinations %>%
      bind_rows(
        set.ms
      )
  }
  all.valid.ms.combinations <- all.valid.ms.combinations %>%
    add_scenariomip_targets_to_IAM_scenarios() %>%
    add_ssp_basis_to_IAM_scenarios()

  return(all.valid.ms.combinations)
}

set_options <- create_all_options(df=passing.scens)

set_options %>% distinct(model,target) %>% group_by(model) %>% count()
set_options %>% distinct(model,scenario) %>% group_by(model) %>% count()

set_options.priorities <- set_options %>%
  left_join(preferences) %>%
  reframe(
    priority = sum(priority),
    .by = c("option")
  ) %>%
  arrange(priority)
set_options.priorities


## Model specific notes ----
#' why is remind dropping out on VLLO?
#' - high-ish peak; drop methane harder 2050 or earlier [like AIM]
#' - high negative co2 in 2100
crit %>% filter(model=="REMIND",target=="VLLO") %>% View()
emissions %>% filter(model=="REMIND",target=="VLLO",year==2050,variable=="Emissions|CH4")
emissions %>% filter(model=="REMIND",target=="VLLO",year==2100,variable=="Emissions|CO2")

# Plots ------------------------------------------------------------------------

## Colours ----
TARGETS <- c(
  # short letter naming
  "H"
  ,"HL" # (*)
  ,"M"
  ,"ML"
  ,"L"
  ,"VLHO"
  ,"VLLO"
)
TARGET.COLOURS <- c(
  '#800000', # H
  '#ff0000', # HL (*)
  '#c87820', # M
  '#d3a640', # ML
  '#098740', # L
  '#0080d0', # VLHO
  '#100060' # VLLO
)
names(TARGET.COLOURS) <- TARGETS

## Pick a marker set ----

### Functions ----
keep_only_markers <- function(df,markers,v="v1"){
  return(
    df %>% left_join(markers %>% filter(version==.env$v)) %>%
      filter(!is.na(marker))
  )
}
### Different sets ----

#### v1 ----
marker.set.v1_jk <- warming %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="AIM")&(scenario=="SSP1 - Very Low Emissions")), # choose SSP1
              marker="VLLO") %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP2 - Low Overshoot")), # REMIND only has SSP2
              marker="VLHO") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Low Emissions")), # MESSAGE prefers SSP2 (over SSP1)
              marker="L") %>%
  mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Medium-Low Emissions")), # choose SSP2 (over SSP1)
              marker="ML") %>%
  mutate_cond(((model=="WITCH")&(scenario=="SSP2 - Medium Emissions")), # WITCH only has SSP2
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP5 - High Emissions")), # choose SSP3
              marker="H") %>%
  mutate(version="v1_jk")

#### v2 ----
marker.set.v2_jk <- warming %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP1 - Very Low Emissions")), # SWAPPED
              marker="VLLO") %>%
  mutate_cond(((model=="AIM")&(scenario=="SSP2 - Low Overshoot")), # SWAPPED
              marker="VLHO") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Low Emissions")), # MESSAGE prefers SSP2 (over SSP1)
              marker="L") %>%
  mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Medium-Low Emissions")), # choose SSP2 (over SSP1)
              marker="ML") %>%
  mutate_cond(((model=="WITCH")&(scenario=="SSP2 - Medium Emissions")), # WITCH only has SSP2
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP5 - High Emissions")), # choose SSP3
              marker="H") %>%
  mutate(version="v2_jk")

#### v1_k1 ----
marker.set.v1_k1 <- warming %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP1 - Very Low Emissions")),
              marker="VLLO") %>%
  mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Low Overshoot")),
              marker="VLHO") %>%
  mutate_cond(((model=="AIM")&(scenario=="SSP2 - Low Emissions")),
              marker="L") %>%
  mutate_cond(((model=="COFFEE")&(scenario=="SSP2 - Medium-Low Emissions")),
              marker="ML") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Medium Emissions")),
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP5 - High Emissions")),
              marker="H") %>%
  mutate(version="v1_k1")

#### v2_k1 ----
marker.set.v2_k1 <- warming %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP1 - Very Low Emissions")),
              marker="VLLO") %>%
  mutate_cond(((model=="AIM")&(scenario=="SSP2 - Low Overshoot")),
              marker="VLHO") %>%
  mutate_cond(((model=="COFFEE")&(scenario=="SSP2 - Low Emissions")),
              marker="L") %>%
  mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Medium-Low Emissions")),
              marker="ML") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Medium Emissions")),
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP5 - High Emissions")), # choose SSP3
              marker="H") %>%
  mutate(version="v2_k1")


#### 23.05.2025: 6 options Keywan for meeting  -----------------------------------
marker.sets.v20250523_k1.starter <- climate.timeseries %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP1 - Very Low Emissions")),
              marker="VLLO") %>%
  # mutate_cond(((model=="AIM")&(scenario=="SSP2 - Low Overshoot")),
  #             marker="VLHO") %>%
  # mutate_cond(((model=="COFFEE")&(scenario=="SSP2 - Low Emissions")),
  #             marker="L") %>%
  # mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Medium-Low Emissions")),
  #             marker="ML") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Medium Emissions")),
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP3 - High Emissions")), # choose SSP3
              marker="H") %>%
  mutate(version="v20250523_fixed")

vlho.l.ml.options <- crossing(
  scenario = c("SSP2 - Low Overshoot", "SSP2 - Low Emissions", "SSP2 - Medium-Low Emissions"), # assuming all SSP2
  model = c("AIM", "COFFEE", "IMAGE")
  ) %>% add_scenariomip_info_columns()
v20250523_k1.subset.options <- create_all_options(df=vlho.l.ml.options, set.size = 3)

marker.sets.v20250523_k1 <- marker.sets.v20250523_k1.starter
for (i in v20250523_k1.subset.options %>% pull(option) %>% unique()){
  subset <- v20250523_k1.subset.options %>% filter(option==i)

  m.vlho <- subset %>% filter(target=="VLHO") %>% pull(model)
  s.vlho <- subset %>% filter(target=="VLHO") %>% pull(scenario)
  m.l <- subset %>% filter(target=="L") %>% pull(model)
  s.l <- subset %>% filter(target=="L") %>% pull(scenario)
  m.ml <- subset %>% filter(target=="ML") %>% pull(model)
  s.ml <- subset %>% filter(target=="ML") %>% pull(scenario)

  marker.sets.v20250523_k1 <- marker.sets.v20250523_k1 %>%
    bind_rows(
      marker.sets.v20250523_k1.starter %>%
        mutate_cond(((model==m.vlho)&(scenario==s.vlho)),
                    marker="VLHO") %>%
        mutate_cond(((model==m.l)&(scenario==s.l)),
                    marker="L") %>%
        mutate_cond(((model==m.ml)&(scenario==s.ml)),
                    marker="ML") %>%
        mutate(version=paste0("v20250523_",i))
    )
}
marker.sets.v20250523_k1 %>% distinct(version)
nrow(marker.sets.v20250523_k1)

#### Combine ----
marker.sets <- marker.set.v1_jk %>%
  bind_rows(marker.set.v2_jk) %>%
  bind_rows(marker.set.v1_k1) %>%
  bind_rows(marker.set.v2_k1)

#### Dispreference indices ----
return_dispreferences_of_marker_sets <- function(marker.df, prefs = preferences){
  return(
    marker.df %>% left_join(prefs %>% rename(marker=target)) %>%
      drop_na(marker) %>% mutate(n=1) %>%
      reframe(
        n = sum(n),
        dispreference = sum(priority),
        .by = c("version")
      )
  )
}
marker.sets %>% return_dispreferences_of_marker_sets()


## Produce plots ----
### Define plots ----
produce_marker_set_plots <- function(emissions,
                                     climate.timeseries,
                                     v.marker,
                                     marker.sets=marker.sets,
                                     markers.to.show="all",
                                     p.format="png",
                                     override.H.HL=FALSE,
                                     legend.ncol.per.indicator=2){

  climate.vars <- climate.timeseries %>% pull(variable) %>% unique()
  emissions.vars <- c(
    "Emissions|Kyoto Gases",
    "Emissions|CO2",
    "Emissions|CH4",
    "Emissions|Sulfur"
  )

  prefix.figure <- paste0(v.marker, "_", markers.to.show, "_")
  caption.figure <- paste0("Marker version: ", v.marker)

  clim <- climate.timeseries %>%
    keep_only_markers(markers = marker.sets, v = v.marker)
  em <- emissions %>%
    keep_only_markers(markers = marker.sets, v = v.marker)



  if (markers.to.show!="all"){
    if (markers.to.show=="low"){
      clim <- clim %>% filter(marker%in%c("VLLO", "VLHO", "L"))
      em <- em %>% filter(marker%in%c("VLLO", "VLHO", "L"))
    } else if (markers.to.show=="H_HL"){
      if (override.H.HL){
        print("Using override") # not implemented
        if (override.H.HL){
          clim <- climate.timeseries %>%
            filter(target=="H") %>%
            mutate(marker="H") %>%
            filter(model%in%c("GCAM", "WITCH")) %>%
            mutate_cond(model=="WITCH", marker="HL")
          em <- emissions %>%
            filter(target=="H") %>%
            mutate(marker="H") %>%
            filter(model%in%c("GCAM", "WITCH")) %>%
            mutate_cond(model=="WITCH", marker="HL")
        }
      } else(
        stop("Not implemented")
      )

    } else if (markers.to.show=="M_ML"){
      clim <- clim %>% filter(marker%in%c("M", "ML"))
      em <- em %>% filter(marker%in%c("M", "ML"))
    }
  }

  # Set up plotting
  set_up_plotting_style_scenarios <- function(df){
    df <- df %>% mutate(scenario = interaction(marker, model, sep = " / "))
    scenario_levels <<- unique(with(df, interaction(marker, model, sep = " / ")))
    scenario_colours <<- setNames(
      TARGET.COLOURS[as.character(df$marker[match(scenario_levels, with(df, interaction(marker, model, sep = " / ")))])],
      scenario_levels
    )
    scenario_linetypes <<- setNames(
      rep(c("solid", "dashed", "dotted", "dotdash"), length.out = length(scenario_levels)),
      scenario_levels
    )
    return(df)
  }




  for (c in climate.vars){
    c.data <- clim %>%
      filter(variable==c) %>%
      pivot_wider(names_from = percentile,
                  values_from = value)

    c.data <- set_up_plotting_style_scenarios(c.data)

    c.var <- c.data %>% pull(variable) %>% unique()
    c.unit <- c.data %>% pull(unit) %>% unique()
    c.m.v <- c.data %>% pull(version) %>% unique()

    p.c <- ggplot(c.data %>%
                    filter(year>=2010),
                  aes(x=year)) +
      geom_ribbon(aes(ymin=p33,
                      ymax=p67,
                      group=scenario,
                      fill=scenario),
                  alpha=0.3) +
      geom_line(aes(y=p50,
                    group=scenario,
                    colour=scenario,
                    linetype=scenario)) +
      theme_jsk() +
      mark_history(sy=2025)+
      # scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
      # scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
      scale_color_manual(values = scenario_colours) +
      scale_fill_manual(values = scenario_colours) +
      scale_linetype_manual(values = scenario_linetypes) +
      labs(
        title = c.var,
        y = c.unit,
        caption = caption.figure,
        colour = "Scenario",
        fill = "Scenario",
        linetype = "Scenario"
      ) +
      legend_column_wise(ncol = legend.ncol.per.indicator)


    save_ggplot(
      p = p.c,
      f = file.path(MARKER.ANALYSIS.FOLDER, "output", paste0(prefix.figure, "climate_", clean_string(c) ) ),
      h = 150,
      w = 150,
      format = p.format, bg = 'white',
      unit = "mm"
    )
  }

  for (e in emissions.vars){
    em.data <- em %>% filter(variable==e)

    em.data <- set_up_plotting_style_scenarios(em.data)

    em.var <- em.data %>% pull(variable) %>% unique()
    em.unit <- em.data %>% pull(unit) %>% unique()

    p.em <- ggplot(em.data %>% filter(year>=2010),aes(x=year,y=value)) +
      geom_line(aes(colour=scenario,linetype=scenario)) +
      theme_jsk() +
      mark_history(sy=2025)+
      # scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
      scale_color_manual(values = scenario_colours) +
      # scale_fill_manual(values = scenario_colours) +
      scale_linetype_manual(values = scenario_linetypes) +
      labs(
        title = em.var,
        y = em.unit,
        caption = caption.figure,
        colour = "Scenario",
        # fill = "Scenario",
        linetype = "Scenario"
      ) +
      legend_column_wise(ncol = legend.ncol.per.indicator)

    save_ggplot(
      p = p.em,
      f = file.path(MARKER.ANALYSIS.FOLDER, "output", paste0(prefix.figure, "emissions_", clean_string(e) ) ),
      h = 150,
      w = 150,
      format = p.format, bg = 'white',
      unit = "mm"
    )
  }


}


### Run plots (v20250519) ----
for (v.marker in (marker.sets %>% pull(version) %>% unique())){
  produce_marker_set_plots(emissions = emissions,
                           climate.timeseries = climate.timeseries,
                           v = v.marker,
                           marker.sets = marker.sets,
                           markers.to.show = "all")
  produce_marker_set_plots(emissions = emissions,
                           climate.timeseries = climate.timeseries,
                           v = v.marker,
                           marker.sets = marker.sets,
                           markers.to.show = "low")
  # produce_marker_set_plots(emissions = emissions,
  #                          climate.timeseries = climate.timeseries,
  #                          v = v.marker,
  #                          marker.sets = marker.sets,
  #                          markers.to.show = "H_HL", override.H.HL = TRUE)
  produce_marker_set_plots(emissions = emissions,
                           climate.timeseries = climate.timeseries,
                           v = v.marker,
                           marker.sets = marker.sets,
                           markers.to.show = "M_ML")
}


#### Additional (H/HL) ----

p.hhl <- ggplot(emissions %>%
                  filter(variable=="Emissions|Kyoto Gases",
                         target=="H",
                         model%in%c(
                           "GCAM",
                           "WITCH"
                         )),
                aes(x=year,y=value,
                    colour=scenario,
                    linetype=model,
                    group=interaction(model,scenario,variable,region))) +
  facet_wrap(~variable) +
  geom_line() +
  theme_jsk() +
  legend_column_wise()

p.hhl




### Run plots (v20250523) ----
for (v.marker in (marker.sets.v20250523_k1 %>% pull(version) %>% unique())){
  print(v.marker)
  produce_marker_set_plots(emissions = emissions,
                           climate.timeseries = climate.timeseries,
                           v = v.marker,
                           marker.sets = marker.sets.v20250523_k1,
                           markers.to.show = "all")
  produce_marker_set_plots(emissions = emissions,
                           climate.timeseries = climate.timeseries,
                           v = v.marker,
                           marker.sets = marker.sets.v20250523_k1,
                           markers.to.show = "low")
  # produce_marker_set_plots(emissions = emissions,
  #                          climate.timeseries = climate.timeseries,
  #                          v = v.marker,
  #                          marker.sets = marker.sets,
  #                          markers.to.show = "H_HL", override.H.HL = TRUE)
  produce_marker_set_plots(emissions = emissions,
                           climate.timeseries = climate.timeseries,
                           v = v.marker,
                           marker.sets = marker.sets.v20250523_k1,
                           markers.to.show = "M_ML")
}
#### Dispreferences ----
marker.sets.v20250523_k1 %>% return_dispreferences_of_marker_sets()
v20250523_k1.subset.options %>% rename(marker=target,version=option) %>% return_dispreferences_of_marker_sets()










## Regional emissions ----
# tbd.


# Visualizing criteria ----

## Per Target ----
# ...
emissions


## Per Model (& target) ----
# ...

# Harmonized emissions ----


## Load ----
models.in.emissions <- emissions %>% pull(model) %>% unique()

harmonized.regional <- NULL
harmonized.global <- NULL
emissions.startyear <- 2010
# for (m in models.in.emissions){
for (m in c("MESSAGE")){
  harmonized.regional <- harmonized.regional %>% bind_rows(
    vroom(file.path(MARKER.ANALYSIS.FOLDER,
                    "emissions",
                    "harmonized_annika",
                    m,
                    paste0("check_harmonisation_regions_",m,".csv"))) %>%
      pivot_longer(
        cols = all_of("1950"):all_of("2100"),
        names_to = "year",
        values_to = "value"
      ) %>%
      drop_na(value) %>%
      mutate(year = as.numeric(year)) %>%
      filter(year>=emissions.startyear)
  )
}
harmonized.regional



## Regional & Total effects ----


## Comparing (a) cumulative effect and (b) trend reversal ----



#' Notes from Carl:
#' - REMIND for
#'
#' ISIMIP:
#' * M and VLLO
#' ...
#' ...
#' ..
#' ..


# IAMC abstract figures ----

## 1. harmonization example? ----

f.harm.data <- harmonized.regional %>% filter(
  scenario == "SSP2 - Low Emissions",
  region == "MESSAGEix-GLOBIOM-GAINS 2.1-R12|China",
  (grepl(variable, pattern="Industr", fixed=T) | grepl(variable, pattern="Energy", fixed=T) ), # sector
) %>%
  add_sector_and_species_columns() %>%
  filter(
    species %in% c("CO2", "CH4", "Sulfur")
  )
history <- vroom(here("data",
                      "data_vetting",
                      "hist",
                      "iamc_regions_cmip7_history_0021_0020.csv")) %>%
  filter(
    region == "MESSAGEix-GLOBIOM-GAINS 2.1-R12|China",
    (grepl(variable, pattern="Industr", fixed=T) | grepl(variable, pattern="Energy", fixed=T) ), # sector
    ) %>%
  iamc_wide_to_long() %>%
  add_sector_and_species_columns() %>%
  filter(
    species %in% c("CO2", "CH4", "Sulfur")
  )



p.harm <- ggplot(f.harm.data %>%
                   filter(year>=2023) %>%
                   mutate_cond(stage=="harmonised", stage="Harmonized emissions") %>%
                   mutate_cond(stage=="pre-processed", stage="Pre-harmonization"),
                 aes(x=year)) +
  facet_grid(unit~sector, scales="free") +
  mark_history(sy=2023) +
  geom_line(aes(y=value,
                linetype=stage),
                colour="dodgerblue") +
  geom_line(data=history %>%
              filter(year>1985) %>%
              mutate(stage="Historical"),
            aes(y=value,
                linetype=stage),
            colour="black") +
  theme_jsk() +
  labs(
    title = "Example of regional emissions harmonization",
    subtitle = "China",
    y = NULL,
    colour = "",
    fill = "",
    linetype = ""
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    # legend.position = "none"
  )
p.harm

save_ggplot(
  p = p.harm,
  f = file.path(MARKER.ANALYSIS.FOLDER, "output", "iamc_abstract", "harmonization"),
  h = 150,
  w = 150,
  bg = 'white',
  unit = "mm"
)

## 2. gridding example? ----
install.packages("ncdf4")
install.packages("terra")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggspatial")
library(ncdf4)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

file_path <-
  here(
    "..","concordia",
       "results",
       "config_cmip7_v0_testing",
       "CH4-em-anthro_input4MIPs_emissions_RESCUE_IIASA-PIK-MESSAGEix-GLOBIOM-GAINS-2.1-M-R12-SSP2---Low-Overshoot_gn_201501-210012.nc"
       )
variable_name <- "CH4_em_anthro_sector=4_1"  # Replace with the variable you want to plot

# Read data using terra
raster_stack <- terra::rast(file_path)
names(raster_stack)  # Inspect to get correct name if needed

# Extract and cap variable ------------------------------------------------
data_raster <- raster_stack[[variable_name]]

data_df <- as.data.frame(data_raster, xy = TRUE, na.rm = TRUE)
colnames(data_df)[3] <- "value"

cap <- quantile(data_df$value, 0.99, na.rm = TRUE)

data_df <- data_df |>
  mutate(value_capped = pmin(value, cap)) %>%
  mutate_cond(value_capped==0, value_capped=NA)

# Convert raster data to sf points and reproject --------------------------
data_sf <- st_as_sf(data_df, coords = c("x", "y"), crs = crs(data_raster))
data_robin <- st_transform(data_sf, crs = "+proj=robin")

# Get land borders --------------------------------------------------------
# Load and project land borders -------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(crs = "+proj=robin")

# Plot --------------------------------------------------------------------
p.grid <- ggplot() +
  geom_sf(
    data = data_robin,
    aes(color = value_capped),
    size = 0.01
  ) +
  geom_sf(
    data = world,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = paste(variable_name, "(≤99th percentile)")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.2),
    # panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 8),
    # legend.position = "bottom"
    legend.position = "none"
  ) +
  labs(
    title = "Example of a spatial distribution of emissions",
    subtitle = "CO2 emissions, Transportation",
    x = NULL,
    y = NULL
  )

# p.grid

save_ggplot(
  p = p.grid,
  f = file.path(MARKER.ANALYSIS.FOLDER, "output", "iamc_abstract", "grid"),
  h = 150,
  w = 200,
  bg = 'white', #format = "png",
  unit = "mm"
)

# ggplot() +
#   geom_sf(
#     data = data_robin,
#     aes(color = value_capped),
#     size = 0.01  # Small dot size for visual density
#   ) +
#   scale_color_viridis_c(
#     option = "plasma",
#     name = paste(variable_name, "(≤99th percentile)")
#   ) +
#   coord_sf(crs = "+proj=robin") +
#   theme_minimal() +
#   theme(
#     panel.grid.major = element_line(color = "gray80", size = 0.2),
#     panel.background = element_rect(fill = "white"),
#     axis.text = element_text(size = 8),
#     legend.position = "bottom"
#   ) +
#   labs(
#     title = "Projected Raster Data (Robinson Projection)",
#     x = NULL,
#     y = NULL
#   )

## 3. climate? ----

marker.set.iamc <- warming %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="AIM")&(scenario=="SSP1 - Very Low Emissions")), # choose SSP1
              marker="VLLO") %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP2 - Low Overshoot")), # REMIND only has SSP2
              marker="VLHO") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Low Emissions")), # MESSAGE prefers SSP2 (over SSP1)
              marker="L") %>%
  mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Medium-Low Emissions")), # choose SSP2 (over SSP1)
              marker="ML") %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP2 - Medium Emissions")), # WITCH only has SSP2
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP5 - High Emissions")), # choose SSP3
              marker="H") %>%
  mutate(version="iamc")


f.clim <- climate.timeseries.updated %>%
  keep_only_markers(markers = marker.set.iamc, v = "iamc")
f.em <- emissions %>%
  keep_only_markers(markers = marker.set.iamc, v = "iamc")

### Temps (all) ----
c.data <- f.clim %>%
  filter(variable=="Surface Air Temperature Change") %>%
  mutate(variable="Temperature Change (GSAT)") %>%
  pivot_wider(names_from = percentile,
              values_from = value)

# c.data <- set_up_plotting_style_scenarios(c.data)

c.var <- c.data %>% pull(variable) %>% unique()
c.unit <- c.data %>% pull(unit) %>% unique()
c.m.v <- c.data %>% pull(version) %>% unique()

p.temps.all <- ggplot(c.data %>%
                filter(year>=2010),
              aes(x=year)) +
  mark_history(sy=2025) +
  geom_ribbon(aes(ymin=p33,
                  ymax=p67,
                  group=scenario,
                  fill=target),
              alpha=0.3) +
  geom_line(aes(y=p50,
                group=scenario,
                colour=target)) +
  theme_jsk() +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  # scale_color_manual(values = scenario_colours) +
  # scale_fill_manual(values = scenario_colours) +
  # scale_linetype_manual(values = scenario_linetypes) +
  labs(
    title = c.var,
    y = c.unit,
    # caption = "Preliminary model results.",
    colour = "Scenario",
    fill = "Scenario",
    linetype = "Scenario"
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    legend.position = c(0.1, 0.9),       # top-left corner
    legend.justification = c(0, 1)   # align top-left of legend box to that corner
  )
p.temps.all

### Temps (Low) ----
p.temps.low <- ggplot(c.data %>% filter(target%in%c("VLLO","VLHO","L")) %>%
                        filter(year>=2010),
                      aes(x=year)) +
  mark_history(sy=2025) +
  geom_ribbon(aes(ymin=p33,
                  ymax=p67,
                  group=scenario,
                  fill=target),
              alpha=0.3) +
  geom_line(aes(y=p50,
                group=scenario,
                colour=target)) +
  theme_jsk() +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  # scale_color_manual(values = scenario_colours) +
  # scale_fill_manual(values = scenario_colours) +
  # scale_linetype_manual(values = scenario_linetypes) +
  labs(
    title = c.var,
    y = c.unit,
    # caption = "Preliminary model results.",
    colour = "Scenario",
    fill = "Scenario",
    linetype = "Scenario"
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    legend.position = "none"
  )
# p.temps.low

### Temps (High) ----
p.temps.high <- ggplot(c.data %>% filter(target%nin%c("VLLO","VLHO","L")) %>%
                        filter(year>=2010),
                      aes(x=year)) +
  mark_history(sy=2025) +
  geom_ribbon(aes(ymin=p33,
                  ymax=p67,
                  group=scenario,
                  fill=target),
              alpha=0.3) +
  geom_line(aes(y=p50,
                group=scenario,
                colour=target)) +
  theme_jsk() +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  # scale_color_manual(values = scenario_colours) +
  # scale_fill_manual(values = scenario_colours) +
  # scale_linetype_manual(values = scenario_linetypes) +
  labs(
    title = c.var,
    y = c.unit,
    # caption = "Preliminary model results.",
    colour = "Scenario",
    fill = "Scenario",
    linetype = "Scenario"
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    legend.position = "none"
  )
p.temps.high

### ERF GHG ----
c.data <- f.clim %>%
  filter(variable=="Effective Radiative Forcing|Greenhouse Gases") %>%
  mutate(variable="ERF|GHGs") %>%
  pivot_wider(names_from = percentile,
              values_from = value)

c.var <- c.data %>% pull(variable) %>% unique()
c.unit <- c.data %>% pull(unit) %>% unique()
c.m.v <- c.data %>% pull(version) %>% unique()

p.erf.GHG <- ggplot(c.data %>%
                        filter(year>=2010),
                      aes(x=year)) +
  mark_history(sy=2025) +
  geom_ribbon(aes(ymin=p33,
                  ymax=p67,
                  group=scenario,
                  fill=target),
              alpha=0.3) +
  geom_line(aes(y=p50,
                group=scenario,
                colour=target)) +
  theme_jsk() +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  labs(
    title = c.var,
    y = c.unit,
    colour = "Scenario",
    fill = "Scenario",
    linetype = "Scenario"
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    legend.position = "none"
  )
# p.erf.GHG

### ERF CO2 ----
c.data <- f.clim %>%
  filter(variable=="Effective Radiative Forcing|CO2") %>%
  mutate(variable="ERF|CO2") %>%
  pivot_wider(names_from = percentile,
              values_from = value)

c.var <- c.data %>% pull(variable) %>% unique()
c.unit <- c.data %>% pull(unit) %>% unique()
c.m.v <- c.data %>% pull(version) %>% unique()

p.erf.CO2 <- ggplot(c.data %>%
                      filter(year>=2010),
                    aes(x=year)) +
  mark_history(sy=2025) +
  geom_ribbon(aes(ymin=p33,
                  ymax=p67,
                  group=scenario,
                  fill=target),
              alpha=0.3) +
  geom_line(aes(y=p50,
                group=scenario,
                colour=target)) +
  theme_jsk() +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  labs(
    title = c.var,
    y = c.unit,
    colour = "Scenario",
    fill = "Scenario",
    linetype = "Scenario"
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    legend.position = "none"
  )
# p.erf.CO2

### ERF Aerosols ----
c.data <- f.clim %>%
  filter(variable=="Effective Radiative Forcing|Aerosols") %>%
  mutate(variable="ERF|Aerosols") %>%
  pivot_wider(names_from = percentile,
              values_from = value)

c.var <- c.data %>% pull(variable) %>% unique()
c.unit <- c.data %>% pull(unit) %>% unique()
c.m.v <- c.data %>% pull(version) %>% unique()

p.erf.aerosols <- ggplot(c.data %>%
                      filter(year>=2010),
                    aes(x=year)) +
  mark_history(sy=2025) +
  geom_ribbon(aes(ymin=p33,
                  ymax=p67,
                  group=scenario,
                  fill=target),
              alpha=0.3) +
  geom_line(aes(y=p50,
                group=scenario,
                colour=target)) +
  theme_jsk() +
  scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
  labs(
    title = c.var,
    y = c.unit,
    colour = "Scenario",
    fill = "Scenario",
    linetype = "Scenario"
  ) +
  legend_column_wise(ncol = 2) +
  theme(
    legend.position = "none"
  )
# p.erf.aerosols


### combined plot ----
p.climate <- (
  p.temps.all + p.temps.low + p.temps.high +
  p.erf.GHG + p.erf.CO2 + p.erf.aerosols#, p.concentration.co2/p.ocean.heat.uptake
) + plot_layout(design = c(
"AAB
AAC
DEF"))
# p.climate

save_ggplot(
  p = p.climate,
  f = file.path(MARKER.ANALYSIS.FOLDER, "output", "iamc_abstract", "climate"),
  h = 250,
  w = 250,
  bg = 'white',
  unit = "mm"
)


# Other ----
write_delim(
  vroom(here("data","marker selection", "output", "scens_online.csv")) %>%
    add_scenariomip_info_columns()
)
