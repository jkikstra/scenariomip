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
              "Surface Air Temperature Change"
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

### Preferenecs ----------------------------------------------------------------
preferences <- read_excel(path = file.path(MARKER.ANALYSIS.FOLDER, "preferences",
                                           "20251605_MarkerPriorities.xlsx"),
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
                                "scenarios_scenariomip_emissions_2025-05-06.csv"),
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
  folder.path = file.path(PATH.CLIMATE.DATA, "20250510_magicc_db_csv"),
  iamc = F,
  magicc.percentiles.calculation = T # because it has a climate model column, and a run_id column
)

###### Save percentiles --------------------------------------------------------
write_delim(
  x = climate.timeseries,
  file = file.path(PATH.CLIMATE.DATA, "20250510_magicc_percentiles.csv"),
  delim = ","
)

##### Load percentiles ---------------------------------------------------------
climate.timeseries <- read_csv(
  file.path(PATH.CLIMATE.DATA, "20250510_magicc_percentiles.csv")
) %>%
  add_scenariomip_info_columns()


climate.timeseries %>% filter(target=="M",
                              climate_model=="MAGICCv7.6.0a3",
                              variable=="Surface Air Temperature Change",
                              percentile=="p50") %>%
  plot_standard_line_one_region()



## Load filters ----------------------------------------------------------------

scenario.list <- warming %>% distinct(model,scenario)


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
  #' - Temperature should start to stabilise (NOT ASSESSED YET)
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

full.scenario.list <- emissions %>% distinct(model,scenario) %>%
  arrange(model,scenario)

write_delim(x = full.scenario.list,
            file = file.path(MARKER.ANALYSIS.FOLDER, "output", "all_scenarios.csv"),
            delim=",")

# Apply filters ----
crit <- filter_criteria(df.emissions=emissions,
                        df.warming=warming,
                        df.categories=categories,
                        df.climate.timeseries=climate.timeseries)
crit <- full.scenario.list %>% left_join(crit)

## Save filter info
write_delim(x = crit,
            file = file.path(MARKER.ANALYSIS.FOLDER, "output", "crit.csv"),
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

create_all_options <- function(df){
  # Get all combinations of 6 rows
  combs <- combn(nrow(passing.scens %>% select(model,scenario,target)), 6, simplify = FALSE)
  length(combs) # takes a few seconds if combs is <10000 (expample nrow(passing.scens)=22 came with combs=74613)

  # Keep only those where:
  # - targets are unique
  # - models are unique
  valid_sets <- keep(combs, function(idx) {
    set <- passing.scens[idx, ]
    n_distinct(set$target) == 6 &&
      n_distinct(set$model) == 6
  })

  valid_sets

  all.valid.ms.combinations <- tibble()
  ms.passing.scens <- passing.scens %>% distinct(model,scenario)
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
  ,"M"
  ,"ML"
  ,"L"
  ,"VLHO"
  ,"VLLO"
)
TARGET.COLOURS <- c(
  '#800000', # H
  # '#ff0000', # high-overshoot (N/A)
  '#c87820', # M
  '#d3a640', # ML
  '#098740', # L
  '#0080d0', # VLHO
  '#100060' # VLLO
)
names(TARGET.COLOURS) <- TARGETS

## Pick a marker set ----

### Functions ----
keep_only_markers <- function(df,markers=marker.set){
  return(
    df %>% left_join(markers) %>%
      filter(!is.na(marker))
  )
}
### Different sets ----

#### v1 ----
marker.set <- warming %>% distinct(model,scenario) %>%
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
  mutate_cond(((model=="GCAM")&(scenario=="SSP3 - High Emissions")), # choose SSP3
              marker="H") %>%
  mutate(version="v1")

#### v2 ----
# ...

## Climate ----

climate.vars <- climate.timeseries %>% pull(variable) %>% unique()
for (c in climate.vars){
  c.data <- climate.timeseries %>%
    keep_only_markers() %>%
    filter(variable==c) %>%
    pivot_wider(names_from = percentile,
                values_from = value)
  c.var <- c.data %>% pull(variable) %>% unique()
  c.unit <- c.data %>% pull(unit) %>% unique()

  p.c <- ggplot(c.data %>%
                  filter(year>=2010),
                aes(x=year)) +
    geom_ribbon(aes(ymin=p33,
                    ymax=p67,
                    fill=marker),
                alpha=0.3) +
    geom_line(aes(y=p50,
                  colour=marker,
                  linetype=model)) +
    theme_jsk() +
    mark_history(sy=2025)+
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    scale_fill_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    labs(
      title = c.var,
      y = c.unit
    ) +
    legend_column_wise()

  save_ggplot(
    p = p.c,
    f = file.path(MARKER.ANALYSIS.FOLDER, "output", paste0("climate_timeseries_", clean_string(c) ) ),
    h = 150,
    w = 150,
    format = "png", bg = 'white',
    unit = "mm"
  )
}



## Global emissions ----
#' GHG
#' CO2
#' CH4
#' Sulfur
emissions.vars <- c(
  "Emissions|Kyoto Gases",
  "Emissions|CO2",
  "Emissions|CH4",
  "Emissions|Sulfur"
)
for (e in emissions.vars){
  em.data <- emissions %>% filter(variable==e) %>%
    keep_only_markers()
  em.var <- em.data %>% pull(variable) %>% unique()
  em.unit <- em.data %>% pull(unit) %>% unique()

  p.em <- ggplot(em.data %>% filter(year>=2010),aes(x=year,y=value)) +
    geom_line(aes(colour=marker,linetype=model)) +
    theme_jsk() +
    mark_history(sy=2025)+
    scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
    labs(
      title = em.var,
      y = em.unit
    )

  save_ggplot(
    p = p.em,
    f = file.path(MARKER.ANALYSIS.FOLDER, "output", paste0("emissions_timeseries_", clean_string(e) ) ),
    h = 150,
    w = 150,
    format = "png", bg = 'white',
    unit = "mm"
  )
}


## Regional emissions ----
# tbd.
