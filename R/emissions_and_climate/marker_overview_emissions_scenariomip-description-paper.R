#' Code for marker analysis ScenarioMIP
#' Developed by Jarmo Kikstra
#' Newly created as a leaner version of "emissions_marker_alignment_analysis"


# Load packages ----
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

# .====. ----
# PATH ----
# Using one climate assessment versions; this is the default
# DATA.EHH <- here("data", "ca", "20250918")
# Using multiple climate assessment versions; not yet fully tested to work with the visualisations/data below.
DATA.EHH.FOLDERS <- c(
  here("data", "ca", "20250904"),
  here("data", "ca", "20250918"),
  # here("data", "ca", "20250924")
  here("data", "ca", "20250925")
)
PATH.CLIMATE.DATA <- file.path(DATA.EHH.FOLDERS, "climate-assessment")
get_date_string_path_climate_data <- function(p){
  parts <- strsplit(p, "/")[[1]]
  date_str <- parts[length(parts) - 1]

  return(date_str)
}
MARKER.SET.FOLDER <- here("data", "marker_set", "20250922")
MARKER.ANALYSIS.FOLDER <- file.path(DATA.EHH.FOLDERS, "plots")


# .====. ----
# Utils ----
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

# .====. ----

# Load data ----

## Climate --------------------------------------------------------------------


### Warming indications -------------------------------------------------------
warming <- NULL
for (p in PATH.CLIMATE.DATA){

  warming <- warming %>%
    bind_rows(
      load_multiple_files(folder.path = p,
                          iamc = F,
                          pandas.multiindex = T,
                          id.cols=c("climate_model", "model", "region", "scenario", "unit", "variable"),
                          mi.cols=c("metric", "quantile"),
                          pattern = "warming-quantiles") %>%
        add_scenariomip_info_columns() %>%
        mutate(ca.version = get_date_string_path_climate_data(p))
    )
}


### Categories ----------------------------------------------------------------
categories <- NULL
for (p in PATH.CLIMATE.DATA){

  categories <- categories %>%
    bind_rows(
      load_multiple_files(folder.path = p,
                          iamc = F,
                          pandas.multiindex = T,
                          id.cols=c("climate_model", "model", "scenario"),
                          mi.cols=c("metric"),
                          pattern = "categories") %>%
        add_scenariomip_info_columns() %>%
        mutate(ca.version = get_date_string_path_climate_data(p))
    )

}


### GSAT (assessed) -----------------------------------------------------------
climate.timeseries.gsat <- NULL
for (p in PATH.CLIMATE.DATA){

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

}

### ERF -----------------------------------------------------------

climate.timeseries.erf <- NULL
for (p in PATH.CLIMATE.DATA){

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
}

### Combine -------------------------------------------------------------------
climate.timeseries <- climate.timeseries.gsat %>%
  bind_rows(climate.timeseries.erf)

climate.timeseries %>% distinct(model)



## Harmonized ---------------------------------------------------------------
# Download from: https://iiasahub.sharepoint.com/sites/eceprog/Shared%20Documents/Forms/AllItems.aspx?FolderCTID=0x012000AA9481BF7BE9264E85B14105F7F082FF&id=%2Fsites%2Feceprog%2FShared%20Documents%2FProjects%2FCMIP7%2FIAM%20Data%20Processing%2FScenarioMIP%20Emulator%20workflow%2FApril%2011%20submission%2Fscm%2Doutput%2F0005%5F0002%5F0002%5F0002%5F0002%5F0002%5F0003%5F0002%5F0002%5F0002%5F0002%5F95b5f2c9fb62e32a4d08fe2ffc5b4a6ff246ad2d%5F0003%5F0003%5F0002


### Pre-processed (SCM) -----------------------------------------------------
pre.processed <- tibble()
for (p in PATH.CLIMATE.DATA){
  for (m in climate.timeseries %>% pull(model) %>% unique() ){
    path <- file.path(p, "..", "emissions", m)
    if (!dir.exists(path)) {
      message("Skipping missing path: ", path)
      next
    }

    df.m <- load_multiple_files(folder.path = path,
                                iamc = T,
                                pattern = "pre-processed-scms") %>%
      mutate(ca.version = get_date_string_path_climate_data(p))

    pre.processed <- pre.processed %>% bind_rows(df.m)
  }
}

### Harmonized (SCM) --------------------------------------------------------
harmonized <- tibble()
for (p in PATH.CLIMATE.DATA){
  for (m in climate.timeseries %>% pull(model) %>% unique() ){
    path <- file.path(p, "..", "emissions", m)
    if (!dir.exists(path)) {
      message("Skipping missing path: ", path)
      next
    }

    df.m <- load_multiple_files(folder.path = path,
                                iamc = T,
                                pattern = "harmonised-scms") %>%
      mutate(ca.version = get_date_string_path_climate_data(p))

    harmonized <- harmonized %>% bind_rows(df.m)
  }
}

### Infilled (SCM) -----------------------------------------------------
infilled <- tibble()
for (p in PATH.CLIMATE.DATA){
  for (m in climate.timeseries %>% pull(model) %>% unique() ){
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


### Totals (pre-processed vs harmonised; compared) -----------------------------------------------
COMPARE.HARMONIZATION.SPECIES <- c("Emissions|CO2|Energy and Industrial Processes",
                                   "Emissions|CO2|AFOLU",
                                   "Emissions|CO",
                                   "Emissions|CH4",
                                   "Emissions|Sulfur",
                                   "Emissions|BC",
                                   "Emissions|OC"
)
harmonization.compare <- pre.processed %>% mutate(stage = "pre-processed") %>%
  bind_rows(harmonized %>% mutate(stage = "harmonized")) %>%
  filter(
    variable %in% COMPARE.HARMONIZATION.SPECIES
  )

#### Calculate Cumulative CO2 ----
start_year <- 2023
harmonization.compare.cumulative.co2 <- harmonization.compare %>% filter(
  variable %in% c("Emissions|CO2|Energy and Industrial Processes",
                  "Emissions|CO2|AFOLU")
) %>% reframe(
  variable = "Emissions|CO2",
  value = sum(value),
  .by = c("model", "scenario", "region", "unit", "year", "stage", "ca.version")
) %>%
  filter(variable == "Emissions|CO2",
         year >= start_year) %>%
  arrange(model, scenario, region, variable, year) %>%
  group_by(model, scenario, region, variable, unit, stage, ca.version) %>%
  mutate(
    year_prev = lag(year),
    value_prev = lag(value),
    # Trapezoid area between each pair of years
    trapezoid = ((value + value_prev) / 2) * (year - year_prev)
  ) %>%
  # Replace NA (first year has no previous) with 0
  mutate(trapezoid = ifelse(is.na(trapezoid), 0, trapezoid)) %>%
  # Now cumulative sum per group
  mutate(cumulative_emissions = cumsum(trapezoid)) %>%
  ungroup() %>%
  # bring back to original format
  select(-value, -year_prev, -value_prev, -trapezoid) %>% rename(value=cumulative_emissions) %>%
  mutate(variable = paste0("Cumulative ", variable)) %>%
  # update units
  mutate(
    unit = "GtCO2",
    value = value / 1e3
  )
harmonization.compare.cumulative.co2

harmonization.compare.cumulative.co2.split <- harmonization.compare %>% filter(
  variable %in% c("Emissions|CO2|Energy and Industrial Processes",
                  "Emissions|CO2|AFOLU")
) %>%
  filter(year >= start_year) %>%
  arrange(model, scenario, region, variable, year) %>%
  group_by(model, scenario, region, variable, unit, stage, ca.version) %>%
  mutate(
    year_prev = lag(year),
    value_prev = lag(value),
    # Trapezoid area between each pair of years
    trapezoid = ((value + value_prev) / 2) * (year - year_prev)
  ) %>%
  # Replace NA (first year has no previous) with 0
  mutate(trapezoid = ifelse(is.na(trapezoid), 0, trapezoid)) %>%
  # Now cumulative sum per group
  mutate(cumulative_emissions = cumsum(trapezoid)) %>%
  ungroup() %>%
  # bring back to original format
  select(-value, -year_prev, -value_prev, -trapezoid) %>% rename(value=cumulative_emissions) %>%
  mutate(variable = paste0("Cumulative ", variable)) %>%
  # update units
  mutate(
    unit = "GtCO2",
    value = value / 1e3
  )
harmonization.compare.cumulative.co2.split
# compare.harmonization <- emissions %>% fi


### Combine harmonization cumulative and annual data -------------------------
harm.data <- harmonization.compare.cumulative.co2.split %>% bind_rows(harmonization.compare) %>%
  add_scenariomip_info_columns()

harm.data <- harm.data %>% filter(
  !((model=="MESSAGE") & (year<2020) & (stage=="pre-processed"))
)

## Native (raw / unharmonised) reported IAM data ----

### Full data ----
marker.data <- load_multiple_files(iamc = T,
                                   filetype = "csv",
                                   folder.path = MARKER.SET.FOLDER,
                                   iamc.wide.to.long = F)

#### Emissions ----
emissions <- marker.data %>%
  filter(Region=="World") %>%
  filter_starts_with(column.name = "Variable",
                     "Emissions") %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  add_scenariomip_info_columns()

##### Cumulative CO2 -----------------------------------------------------------
start_year <- 2020

cumulative.co2 <- emissions %>%
  filter(variable == "Emissions|CO2",
         year >= start_year) %>%
  arrange(model, scenario, region, variable, year) %>%
  group_by(model, scenario, region, variable, unit, target, ssp, full.model.name) %>%
  mutate(
    year_prev = lag(year),
    value_prev = lag(value),
    # Trapezoid area between each pair of years
    trapezoid = ((value + value_prev) / 2) * (year - year_prev)
  ) %>%
  # Replace NA (first year has no previous) with 0
  mutate(trapezoid = ifelse(is.na(trapezoid), 0, trapezoid)) %>%
  # Now cumulative sum per group
  mutate(cumulative_emissions = cumsum(trapezoid)) %>%
  ungroup() %>%
  # bring back to original format
  select(-value, -year_prev, -value_prev, -trapezoid) %>% rename(value=cumulative_emissions) %>%
  mutate(variable = paste0("Cumulative ", variable)) %>%
  # update units
  mutate(
    unit = "GtCO2",
    value = value / 1e3
  )
cumulative.co2

#### CDR ----
cdr <- marker.data %>%
  filter(Region=="World") %>%
  filter_starts_with(column.name = "Variable",
                     "Carbon") %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  add_scenariomip_info_columns()



# .====. ----
# Plotting choices ----

## Colours ----
TARGETS <- c(
  # short letter naming
  "H"
  ,"HL"
  ,"M"
  ,"ML"
  ,"L"
  ,"L_a"
  ,"VLHO"
  ,"VLHO_a"
  ,"VLHO_b"
  ,"VLHO_c"
  ,"VLHO_d"
  ,"VLLO"
)
TARGET.COLOURS <- c(
  '#800000', # H
  '#ff0000', # HL (*)
  '#c87820', # M
  '#d3a640', # ML
  '#098740', # L
  '#098740', # L
  '#0080d0', # VLHO
  '#0080d0', # VLHO
  '#0080d0', # VLHO
  '#0080d0', # VLHO
  '#0080d0', # VLHO
  '#100060' # VLLO
)
names(TARGET.COLOURS) <- TARGETS

## Functions ----
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
  # scenario_shapes <<- setNames(
  #   rep(c(1,2,...), length.out = length(scenario_levels)),
  #   scenario_levels
  # )
  return(df)
}


# .====. ----
# Pick a marker set ----

## Functions ----

### Filtering scenarios ----
keep_only_markers <- function(df,markers,v="v1"){
  return(
    df %>% left_join(markers %>% filter(version==.env$v)) %>%
      filter(!is.na(marker))
  )
}

## [Pick marker set] ----
v.marker <- "v20250922"
marker.set.final <- climate.timeseries %>% distinct(model,scenario) %>%
  mutate(marker=NA) %>%
  mutate_cond(((model=="REMIND")&(scenario=="SSP1 - Very Low Emissions")),
              marker="VLLO") %>%
  mutate_cond(((model=="AIM")&(scenario=="SSP2 - Low Overshoot_a")), # many options, Shinichiro communicated his preference over email on 21.09.2025
              marker="VLHO") %>%
  mutate_cond(((model=="MESSAGE")&(scenario=="SSP2 - Low Emissions_e")), # communicated as being the current preferred option by Oliver (Slack September 19)
              marker="L") %>%
  mutate_cond(((model=="COFFEE")&(scenario=="SSP2 - Medium-Low Emissions")),
              marker="ML") %>%
  mutate_cond(((model=="IMAGE")&(scenario=="SSP2 - Medium Emissions")),
              marker="M") %>%
  mutate_cond(((model=="GCAM")&(scenario=="SSP3 - High Emissions")),
              marker="H") %>%
  mutate_cond(((model=="WITCH")&(scenario=="SSP5 - Medium-Low Emissions_a")),
              marker="HL") %>%
  mutate(version=v.marker)


# .====. ----
# Plotting functions ----

produce_marker_set_plots <- function(emissions,
                                     climate.timeseries,
                                     v.marker,
                                     harmonization=NULL,
                                     marker.sets=marker.sets,
                                     markers.to.show="all",
                                     p.format="png",
                                     override.H.HL=FALSE,
                                     legend.ncol.per.indicator=2,
                                     low.scens = c("VLLO", "VLHO", "L")){

  climate.vars <- climate.timeseries %>% pull(variable) %>% unique()
  climate.vars <- c(
    "Surface Temperature (GSAT)",
    "Effective Radiative Forcing|Aerosols",
    "Effective Radiative Forcing|CO2",
    "Effective Radiative Forcing|Greenhouse Gases"
  )
  emissions.vars <- c(
    "Emissions|Kyoto Gases",
    "Cumulative Emissions|CO2",
    "Emissions|CO2",
    "Emissions|CH4",
    "Emissions|Sulfur"
  )
  h.vars <- c(
    "Cumulative Emissions|CO2|Energy and Industrial Processes",
    "Cumulative Emissions|CO2|AFOLU",
    "Emissions|CO2|Energy and Industrial Processes",
    "Emissions|CO2|AFOLU",
    "Emissions|Sulfur",
    "Emissions|CO",
    "Emissions|CH4",
    "Emissions|BC",
    "Emissions|OC"
  )

  prefix.figure <- paste0(v.marker, "_", markers.to.show, "_")
  caption.figure <- paste0("Marker version: ", v.marker)

  clim <- climate.timeseries %>%
    keep_only_markers(markers = marker.sets, v = v.marker)
  em <- emissions %>%
    keep_only_markers(markers = marker.sets, v = v.marker)
  if (!is.null(harmonization)){
    h <- harmonization %>%
      filter(variable%in%h.vars) %>%
      keep_only_markers(markers = marker.sets, v = v.marker)
  }





  if (markers.to.show!="all"){
    if (markers.to.show=="low"){
      LOW.SCENS <- low.scens

      clim <- clim %>% filter(marker%in%LOW.SCENS)
      em <- em %>% filter(marker%in%LOW.SCENS)
      if (!is.null(harmonization)){
        h <- h %>% filter(marker%in%LOW.SCENS)
      }
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


  #### Plots: climate ----
  for (c in climate.vars){
    c.data <- clim %>%
      filter(variable==c) %>%
      pivot_wider(names_from = percentile,
                  values_from = value)

    c.data <- set_up_plotting_style_scenarios(c.data)

    c.var <- c.data %>% pull(variable) %>% unique()
    c.unit <- c.data %>% pull(unit) %>% unique()
    c.m.v <- c.data %>% pull(version) %>% unique()

    # browser()

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
      f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "climate_", clean_string(c) ) ),
      h = 150,
      w = 150,
      format = p.format, bg = 'white',
      unit = "mm"
    )
  }

  #### Plots: emissions ----
  for (e in emissions.vars){
    em.data <- em %>% filter(variable==e)

    em.data <- set_up_plotting_style_scenarios(em.data)

    em.var <- em.data %>% pull(variable) %>% unique()
    em.unit <- em.data %>% pull(unit) %>% unique()

    p.em <- ggplot(em.data %>% filter(year>=2010),aes(x=year,y=value)) +
      geom_line(aes(colour=scenario,
                    # linetype=scenario
      )) +
      theme_jsk() +
      mark_history(sy=2025)+
      # scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
      scale_color_manual(values = scenario_colours) +
      # scale_fill_manual(values = scenario_colours) +
      # scale_linetype_manual(values = scenario_linetypes) +
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
      f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "emissions_", clean_string(e) ) ),
      h = 150,
      w = 150,
      format = p.format, bg = 'white',
      unit = "mm"
    )
  }

  #### Plots: harmonization ----

  # For each separate species
  if (!is.null(harmonization)){
    emissions.harmonized.vars <- h %>% variable_unique()
    for (e in emissions.harmonized.vars){
      em.data <- h %>% filter(variable==e,
                              stage=="harmonized")

      em.data <- set_up_plotting_style_scenarios(em.data)

      em.var <- em.data %>% pull(variable) %>% unique()
      em.unit <- em.data %>% pull(unit) %>% unique()

      p.em <- ggplot(em.data %>% filter(year>=2010),aes(x=year,y=value)) +
        geom_line(aes(colour=scenario,
                      # linetype=scenario
        )) +
        theme_jsk() +
        mark_history(sy=2025)+
        # scale_color_manual(values = TARGET.COLOURS, breaks = names(TARGET.COLOURS)) +
        scale_color_manual(values = scenario_colours) +
        # scale_fill_manual(values = scenario_colours) +
        # scale_linetype_manual(values = scenario_linetypes) +
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
        f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "emissions_harmonized_", clean_string(e) ) ),
        h = 150,
        w = 150,
        format = p.format, bg = 'white',
        unit = "mm"
      )
    }
  }


  # Multiple species
  if (!is.null(harmonization)){

    h <- set_up_plotting_style_scenarios(h)

    if (markers.to.show=="low"){

      p.harm <- ggplot(h,
                       aes(x=year,y=value,linetype=stage,
                           group=interaction(model,scenario,variable,region,unit,stage))) +
        facet_wrap(.~variable, scales = "free_y", ncol=h %>% pull(variable) %>% unique() %>% length()) +
        mark_history(sy=2025) +
        geom_line(aes(colour=scenario), linewidth = 1.3) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0,hjust = 0)
        ) +
        scale_color_manual(values = scenario_colours) +
        # scale_fill_manual(values = scenario_colours) +
        # scale_linetype_manual(values = scenario_linetypes) +
        labs(
          title = "Harmonization effect (for input to MAGICC)",
          # y = em.unit,
          caption = caption.figure,
          colour = "Scenario",
          # fill = "Scenario",
          linetype = "Scenario"
        ) +
        legend_column_wise(ncol = legend.ncol.per.indicator)

      save_ggplot(
        p = p.harm,
        f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "harmonization_timeseries" ) ),
        h = 125,
        w = 350,
        format = p.format, bg = 'white',
        unit = "mm"
      )

      p.harm.ch4.loworder <- ggplot(h %>% filter(
        variable == "Emissions|CH4",
        stage=="harmonized",
        marker %in% c("VLLO", "VLHO", "L")
      ),
      aes(x=year,y=value,linetype=stage,
          group=interaction(model,scenario,variable,region,unit,stage))) +
        facet_wrap(.~variable, scales = "free_y", ncol=h %>% pull(variable) %>% unique() %>% length()) +
        mark_history(sy=2025) +
        geom_line(aes(colour=scenario), linewidth = 1.3) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0,hjust = 0)
        ) +
        scale_color_manual(values = scenario_colours) +
        # scale_fill_manual(values = scenario_colours) +
        # scale_linetype_manual(values = scenario_linetypes) +
        labs(
          title = "Harmonized CH4 emissions",
          y = "Mt CH4/yr",
          caption = caption.figure,
          colour = "Scenario",
          # fill = "Scenario",
          linetype = "Scenario"
        ) +
        legend_column_wise(ncol = legend.ncol.per.indicator)

      save_ggplot(
        p = p.harm.ch4.loworder,
        f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "harmonization_timeseries_ch4_order" ) ),
        h = 150,
        w = 150,
        format = p.format, bg = 'white',
        unit = "mm"
      )

      p.harm.cumu <- ggplot(h %>% filter_includes("Cumulative Emissions|CO2|Energy and Industrial Processes"),
                            aes(x=year,y=value,linetype=stage,
                                group=interaction(model,scenario,variable,region,unit,stage))) +
        facet_wrap(.~variable, scales = "free_y", ncol=h %>% pull(variable) %>% unique() %>% length()) +
        mark_history(sy=2025) +
        geom_line(aes(colour=scenario), linewidth = 1.3) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0,hjust = 0)
        ) +
        scale_color_manual(values = scenario_colours) +
        # scale_fill_manual(values = scenario_colours) +
        # scale_linetype_manual(values = scenario_linetypes) +
        labs(
          title = "Harmonization effect (for input to MAGICC)",
          # y = em.unit,
          caption = caption.figure,
          colour = "Scenario",
          # fill = "Scenario",
          linetype = "Scenario"
        ) +
        legend_column_wise(ncol = legend.ncol.per.indicator)

      save_ggplot(
        p = p.harm.cumu,
        f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "harmonization_timeseries_cumulative_EIP" ) ),
        h = 150,
        w = 150,
        format = p.format, bg = 'white',
        unit = "mm"
      )

      # p.harm.cumu <- ggplot(h %>% filter_includes("Cumulative Emissions|CO2|AFOLU"),
      #                       aes(x=year,y=value,linetype=stage,
      #                           group=interaction(model,scenario,variable,region,unit,stage))) +
      #   facet_wrap(.~variable, scales = "free_y", ncol=h %>% pull(variable) %>% unique() %>% length()) +
      #   mark_history(sy=2025) +
      #   geom_line(aes(colour=scenario), linewidth = 1.3) +
      #   theme_jsk() +
      #   theme(
      #     strip.text.y = element_text(angle = 0,hjust = 0)
      #   ) +
      #   scale_color_manual(values = scenario_colours) +
      #   # scale_fill_manual(values = scenario_colours) +
      #   # scale_linetype_manual(values = scenario_linetypes) +
      #   labs(
      #     title = "Harmonization effect (for input to MAGICC)",
      #     # y = em.unit,
      #     caption = caption.figure,
      #     colour = "Scenario",
      #     # fill = "Scenario",
      #     linetype = "Scenario"
      #   ) +
      #   legend_column_wise(ncol = legend.ncol.per.indicator)
      #
      # save_ggplot(
      #   p = p.harm.cumu,
      #   f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "harmonization_timeseries_cumulative_AFOLU" ) ),
      #   h = 150,
      #   w = 150,
      #   format = p.format, bg = 'white',
      #   unit = "mm"
      # )

      # p.harm.no.image <- ggplot(h %>% filter(model!="IMAGE"),
      #                       aes(x=year,y=value,linetype=stage,
      #                           group=interaction(model,scenario,variable,region,unit,stage))) +
      #   facet_wrap(.~variable, scales = "free_y", ncol=h %>% pull(variable) %>% unique() %>% length()) +
      #   mark_history(sy=2025) +
      #   geom_line(aes(colour=scenario), linewidth = 1.3) +
      #   theme_jsk() +
      #   theme(
      #     strip.text.y = element_text(angle = 0,hjust = 0)
      #   ) +
      #   scale_color_manual(values = scenario_colours) +
      #   # scale_fill_manual(values = scenario_colours) +
      #   # scale_linetype_manual(values = scenario_linetypes) +
      #   labs(
      #     title = "Harmonization effect (for input to MAGICC)",
      #     # y = em.unit,
      #     caption = caption.figure,
      #     colour = "Scenario",
      #     # fill = "Scenario",
      #     linetype = "Scenario"
      #   ) +
      #   legend_column_wise(ncol = legend.ncol.per.indicator)
      #
      # save_ggplot(
      #   p = p.harm.no.image,
      #   f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "harmonization_timeseries_no_IMAGE" ) ),
      #   h = 125,
      #   w = 350,
      #   format = p.format, bg = 'white',
      #   unit = "mm"
      # )

    }

    if (markers.to.show=="all"){

      p.harm <- ggplot(h,
                       aes(x=year,y=value,linetype=stage,
                           group=interaction(model,scenario,variable,region,unit,stage))) +
        facet_grid(variable~marker, scales = "free_y") +
        mark_history(sy=2025) +
        geom_line(aes(colour=scenario), linewidth = 1.3) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0,hjust = 0)
        ) +
        scale_color_manual(values = scenario_colours) +
        # scale_fill_manual(values = scenario_colours) +
        # scale_linetype_manual(values = scenario_linetypes) +
        labs(
          title = "Harmonization effect (for input to MAGICC)",
          # y = em.unit,
          caption = caption.figure,
          colour = "Scenario",
          # fill = "Scenario",
          linetype = "Scenario"
        ) +
        legend_column_wise(ncol = legend.ncol.per.indicator)

      save_ggplot(
        p = p.harm,
        f = file.path(MARKER.ANALYSIS.FOLDER, paste0(prefix.figure, "harmonization_timeseries" ) ),
        h = 175,
        w = 350,
        format = p.format, bg = 'white',
        unit = "mm"
      )

    }

  }

}



# .====. ----
# [Run] main plotting figures ----

## All scenarios ----
produce_marker_set_plots(emissions = emissions %>% bind_rows(cumulative.co2) %>% filter(
  !((model=="MESSAGE") & (year<2020))
),
climate.timeseries = climate.timeseries,
v = v.marker,
harmonization = harm.data,
marker.sets = marker.set.final,
markers.to.show = "all")

## Only low scenarios ----
produce_marker_set_plots(emissions = emissions %>% bind_rows(cumulative.co2) %>% filter(
  !((model=="MESSAGE") & (year<2020))
),
climate.timeseries = climate.timeseries,
v = v.marker,
harmonization = harm.data,
marker.sets = marker.set.final,
low.scens = c("VLLO", "VLHO", "L"),
markers.to.show = "low")


# .====. ----
# [Save] data underpinning main plotting figures ----
write_delim(
  x = emissions %>% bind_rows(cumulative.co2) %>% filter(
    !((model=="MESSAGE") & (year<2020))
  ),
  file = file.path(MARKER.ANALYSIS.FOLDER, "natively_reported_data.csv"),
  delim = ","
)
write_delim(
  x = climate.timeseries,
  file = file.path(MARKER.ANALYSIS.FOLDER, "climate_outcomes.csv"),
  delim = ","
)
write_delim(
  x = harm.data,
  file = file.path(MARKER.ANALYSIS.FOLDER, "harmonised_emissions.csv"),
  delim = ","
)


# :::::: ----
# :::::: ----
# Side quest into AR6 climate data ----
AR6_CLIMATE_DATA <- vroom("C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Scenario data/Scenario Databases/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv")

erf.aer.p50 <- AR6_CLIMATE_DATA %>%
  filter(grepl(Variable, pattern = "Effective Radiative Forcing", fixed=T)) %>%
  filter(grepl(Variable, pattern = "Aerosols", fixed=T)) %>%
  filter(grepl(Variable, pattern = "50.0th Percentile", fixed=T)) %>%
  filter(!grepl(Variable, pattern = "irect", fixed=T))

erf.aer.p50 %>% mutate(erf_2040_vs_2015 = `2040` - `2015`) %>%
  mutate(emulator=ifelse(grepl(Variable, pattern="MAGICC", fixed=T), "MAGICC", "FaIR")) %>%
  ggplot(
    data = .,
    aes(x = erf_2040_vs_2015)
  ) +
  # facet_grid(~emulator) +
  geom_histogram(aes(colour=emulator), alpha=0,position = position_identity()) +
  # theme_jsk() +
  scale_x_continuous(name = "Change in ERF|Aerosols (p50) in 2040 rel. to 2015") +
  scale_y_continuous(name = "Scenario count")


# :::::: ----
# :::::: ----
# Side quest into OC from forest burning ----
oc_forest_burning_russia <- marker.data %>%
  filter_starts_with(column.name = "Variable",
                     "Emissions") %>% filter(
                       Variable=="Emissions|OC|AFOLU|Land|Fires|Forest Burning",
                       Region=="Reforming Economies (R10)"
                     ) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  add_scenariomip_info_columns()

p.oc_forest_burning_russia <- ggplot(oc_forest_burning_russia %>% filter(year>=2020),
                                     mapping=aes(x=year, y=value, colour=target)) +
  facet_grid(~model) +
  geom_line(aes(group=interaction(model,scenario)), linewidth=1.3) +
  labs(title = "Emissions|OC|AFOLU|Land|Fires|Forest Burning",
       subtitle = "Reforming Economies (R10)") +
  theme_jsk() + mark_history(sy = 2023)
p.oc_forest_burning_russia

oc_forest_burning_africa <- marker.data %>%
  filter_starts_with(column.name = "Variable",
                     "Emissions") %>% filter(
                       Variable=="Emissions|OC|AFOLU|Land|Fires|Forest Burning",
                       Region=="Africa (R10)"
                     ) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  add_scenariomip_info_columns()

p.oc_forest_burning_africa <- ggplot(oc_forest_burning_africa %>% filter(year>=2020),
                                     mapping=aes(x=year, y=value, colour=target)) +
  facet_grid(~model) +
  geom_line(aes(group=interaction(model,scenario)), linewidth=1.3) +
  labs(title = "Emissions|OC|AFOLU|Land|Fires|Forest Burning",
       subtitle = "Africa (R10)") +
  theme_jsk() + mark_history(sy = 2023)
p.oc_forest_burning_africa

afolu_africa <- marker.data %>%
  filter_starts_with(column.name = "Variable",
                     "Emissions") %>% filter(
                       grepl(Variable, pattern="Emissions|OC|AFOLU",fixed=T),
                       Region=="Africa (R10)"
                     ) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  add_scenariomip_info_columns()

p.afolu_africa <- ggplot(afolu_africa %>% filter(year>=2020),
                                     mapping=aes(x=year, y=value, colour=target)) +
  facet_grid(variable~model, scales="free_y") +
  geom_line(aes(group=interaction(model,scenario)), linewidth=1.3) +
  labs(title = "Emissions|OC|AFOLU|*",
       subtitle = "Africa (R10)") +
  theme_jsk() + mark_history(sy = 2023)+
  theme(
    strip.text.y = element_text(angle = 0,hjust = 0)
  )
p.afolu_africa


# :::::: ----
# :::::: ----
# Side quest check IMAGE changes in CO harmonization ----

for (c in c(
  "Effective Radiative Forcing|Aerosols",
  "Surface Temperature (GSAT)"
)){
  prefix.figure <- paste0("CO_analysis", "_", "IMAGE", "_")

  c.data <- climate.timeseries %>%
    filter(model=="IMAGE", scenario=="SSP2 - Medium Emissions") %>%
    filter(variable==c) %>%
    pivot_wider(names_from = percentile,
                values_from = value)

  c.data <- set_up_plotting_style_scenarios(c.data %>% mutate(marker=target))

  c.var <- c.data %>% pull(variable) %>% unique()
  c.unit <- c.data %>% pull(unit) %>% unique()
  c.m.v <- c.data %>% pull(ca.version) %>% unique()

  p.c <- ggplot(c.data %>%
                  filter(year>=2010),
                aes(x=year)) +
    facet_grid(~ca.version) +
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
      # caption = caption.figure,
      colour = "Scenario",
      fill = "Scenario",
      linetype = "Scenario"
    ) +
    legend_column_wise(ncol = 2)
  p.c


  save_ggplot(
    p = p.c,
    f = file.path(MARKER.ANALYSIS.FOLDER[2], paste0(prefix.figure, "climate_", clean_string(c) ) ),
    h = 150,
    w = 150,
    format = "pdf", bg = 'white',
    unit = "mm"
  )
}


# :::::: ----
# :::::: ----
# Side quest check REMIND VLLO submission differences ----
compare.data <- harm.data %>%
  filter(target=="VLLO",
         scenario=="SSP1 - Very Low Emissions",
         model=="REMIND")

p.compare.harm.data <- ggplot(
  compare.data,
  aes(x=year, y=value, linetype=ca.version, colour=ca.version)
) +
  facet_wrap(stage~variable, scales="free_y") +
  geom_line(aes(group=interaction(model,scenario,variable,ca.version)))
p.compare.harm.data

temp <- climate.timeseries.gsat %>%
  filter(target=="VLLO",
         scenario=="SSP1 - Very Low Emissions",
         model=="REMIND")

p.compare.temp <- ggplot(
  temp %>% filter(percentile=="p50"),
  aes(x=year, y=value, linetype=ca.version, colour=ca.version)
) +
  facet_wrap(percentile~variable, scales="free_y") +
  geom_line(aes(group=interaction(model,scenario,variable,ca.version))) +
  scale_y_continuous(breaks = c(1.1,1.2,1.3,1.4,1.5,1.6,1.7))
p.compare.temp
