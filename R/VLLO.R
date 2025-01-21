# Explore scenarioMIP very low with low overshoot - developed for ScenarioMIP, related to SSP update.
#' Produced by Jarmo Kikstra
#'
#'



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

# Scenario input data ----------------------------------------------------------
DATA_DOWNLOAD_DATE <- "20241122"

load_multiple_files <- function(ddd = DATA_DOWNLOAD_DATE) {
  # Get list of .csv files with the specified date in their name
  csv_files <- list.files(
    path = here::here("data"),
    pattern = paste0(ddd, ".*\\.csv$"),
    full.names = TRUE
  )

  if (length(csv_files) == 0) {
    stop("No CSV files found for the specified date: ", ddd)
  }

  # Load, process with iamc_wide_to_long, and bind all rows
  combined_data <- csv_files %>%
    purrr::map_dfr(~ iamc_wide_to_long(vroom::vroom(.x, col_types = vroom::cols())))

  return(combined_data)
}

data.allscens <- load_multiple_files(ddd = DATA_DOWNLOAD_DATE)
vllo.variants <- data.allscens %>% distinct(Scenario) %>%
  filter(grepl(Scenario,pattern="Very Low Emissions")) %>%
  pull(Scenario)
data.vllo <- data.allscens %>%
  filter(
    Scenario %in% vllo.variants
  ) %>%
  upper_to_lower() %>% # because I don't like typing capital letters
  filter(year<=2100) %>% # to avoid odd data issues (such as WITCH 0 values until 2150)
  simplify_model_names() # for ease of use and plotting

# Net-zero function ------------------------------------------------------------

#' Estimate the Year When a Value Crosses Zero
#'
#' This function estimates the year when a specified column (`value_col`) crosses zero
#' for each combination of grouping columns (`group_cols`). Two methods are available:
#' returning the first year before the reported crossing year ("before-reported-model-year")
#' or estimating the crossing year using linear interpolation ("linear-interpolation").
#'
#' @param df A data frame containing the data to analyze.
#' @param value_col A string specifying the column name containing the values to check for zero-crossing. Default is `"value"`.
#' @param year_col A string specifying the column name containing the years. Default is `"year"`.
#' @param group_cols A vector of strings specifying the columns that define groups, typically `"model"` and `"scenario"`.
#' @param mode A string specifying the estimation method. Options are:
#'   - `"before-reported-model-year"`: Returns the year before the zero-crossing.
#'   - `"linear-interpolation"`: Uses linear interpolation to estimate the exact year of crossing.
#'   Default is `"linear-interpolation"`.
#' @param threshold_value A numeric value specifying the threshold for which the crossing is investigated. Default is `0`.
#'
#' @return A data frame containing the group columns and the estimated crossing year.
#'   The output varies depending on the selected `mode`:
#'   - For `"before-reported-model-year"`, the output contains the first year before the crossing.
#'   - For `"linear-interpolation"`, the output contains interpolated years.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   model = c("A", "A", "A", "B", "B", "B"),
#'   scenario = c("X", "X", "X", "Y", "Y", "Y"),
#'   year = c(2020, 2021, 2022, 2020, 2021, 2022),
#'   value = c(-1, 0.5, 1.5, 2, -1, -2)
#' )
#'
#' # Estimate crossing year with default mode
#' estimate_crossing_year(df, mode = "before-reported-model-year")
#'
#' # Estimate crossing year with linear interpolation
#' estimate_crossing_year(df)
#'
#' @import dplyr
#' @export
estimate_crossing_year <- function(data, value_col = "value", year_col = "year", group_cols = c("model", "scenario"),
                                   mode = "linear-interpolation",
                                   threshold_value = 0) {
  # Ensure the columns exist
  stopifnot(all(c(value_col, year_col, group_cols) %in% colnames(data)))

  # Apply the threshold
  data <- data %>% mutate(value = value - threshold_value)

  # Convert character column names to symbols
  group_syms <- rlang::syms(group_cols)

  if (mode == "before-reported-model-year") {
    # Identify the first year before a crossing occurs
    crossing_years <- data %>%
      arrange(!!!group_syms, .data[[year_col]]) %>% # Use symbols for grouping
      group_by(!!!group_syms) %>%
      mutate(sign_change = sign(.data[[value_col]]) != lag(sign(.data[[value_col]]))) %>% # Detect sign change
      filter(!is.na(sign_change) & sign_change) %>% # Remove NA and keep rows with a sign change
      slice_head(n = 1) %>% # Get the first crossing for each group
      summarise(crossing_year = first(.data[[year_col]]), .groups = "drop") # Report the first year with a sign change

  } else if (mode == "linear-interpolation") {
    # Use linear interpolation to estimate the crossing year
    crossing_years <- data %>%
      arrange(!!!group_syms, .data[[year_col]]) %>% # Use symbols for grouping
      group_by(!!!group_syms) %>%
      mutate(
        sign_prev = lag(sign(.data[[value_col]])), # Previous sign
        year_prev = lag(.data[[year_col]]),       # Previous year
        value_prev = lag(.data[[value_col]])      # Previous value
      ) %>%
      filter(!is.na(sign_prev) & sign(.data[[value_col]]) != sign_prev) %>% # Keep sign changes
      reframe(
        crossing_year = year_prev + (.data[[year_col]] - year_prev) * abs(value_prev) /
          (abs(value_prev) + abs(.data[[value_col]])),
        !!!group_syms # Include group columns in the result
      )
  } else {
    stop("Invalid mode specified. Choose 'before-reported-model-year' or 'linear-interpolation'.")
  }

  return(crossing_years)
}

estimate_net_zero <- function(df, var="Emissions|CO2", region="World",
                              mode = "only-linear-interpolation",
                              rounding_year=TRUE,
                              ...){
  df <- df %>% filter(variable==var, region=="World")

  if (mode=="only-linear-interpolation"){
    out <- estimate_crossing_year(df, ...) %>% mutate(mode="linear-interpolation") %>%
      mutate(variable = var)
  } else if (mode=="only-before-reported-model-year"){
    out <- estimate_crossing_year(df, mode="before-reported-model-year", ...) %>% mutate(mode="before-reported-model-year") %>%
      mutate(variable = var)
  } else if (mode=="all-methods"){
    out <- estimate_crossing_year(df, ...) %>% mutate(mode="linear-interpolation") %>%
      bind_rows(estimate_crossing_year(df, mode = "before-reported-model-year", ...) %>% mutate(mode="before-reported-model-year")) %>%
      mutate(variable = var)
  }

  # clean up
  out <- out %>% rename(net_zero_year = crossing_year)
  if (rounding_year == TRUE){
    out <- out %>% mutate(net_zero_year = round(net_zero_year))
  }

  return(out)
}

# Data -------------------------------------------------------------------------
### Net-zero CO2 and GHG -------------------------------------------------------
nz.vars <- c("Emissions|CO2",
             "Emissions|Kyoto Gases")
nz <- NULL
for (i in nz.vars){
  nz <- nz %>%
    bind_rows(
      estimate_net_zero(data.vllo, var = i)
    )
}

### Net-zero CO2 and GHG -------------------------------------------------------
below.value.value <- 5000
below.value.vars <- c("Emissions|CO2",
                      "Emissions|Kyoto Gases")
below.value <- NULL
for (i in below.value.vars){
  below.value <- below.value %>%
    bind_rows(
      estimate_net_zero(data.vllo, var = i, threshold_value = below.value.value) %>%
        rename(below_threshold_year = net_zero_year)
    )
}

### Peak temperatures ----------------------------------------------------------
estimate_peakT_from_timeseries <- function(df, var = "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"){
  peak.df <- df %>%
    filter(variable==var) %>%
    reframe(
      peak_temperature = max(value),
      .by = c("model", "scenario", "variable", "unit")
    )

  return(peak.df)
}

peak.vars <- c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile",
               "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|67.0th Percentile",
               "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|90.0th Percentile")
peak <- NULL
for (i in peak.vars){
  peak <- peak %>%
    bind_rows(
      estimate_peakT_from_timeseries(data.vllo, var = i)
    )
}
peak

t2025 <- data.vllo %>% filter(variable%in%peak.vars) %>%
  filter(year==2025) %>%
  reframe(
    range = paste0(
      round(min(value),digits=2), " - ", round(max(value),digits=2)
    ),
    .by = c("variable")
  )


# Visualise --------------------------------------------------------------------

## ranges of net-zero and peak temperatures ------------------------------------
### net-zero -------------------------------------------------------------------
nz.ranges <- ggplot(nz, aes(x=net_zero_year, y=variable)) +
  geom_boxplot() +
  geom_point(aes(shape=model,colour=scenario)) +
  theme_jsk() + mark_history(sy = 2025) +
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.minor.x = element_line(colour = "lightgrey", linetype = "dotted"),
        panel.grid.major.y = element_blank()) +
  ylab(NULL) +
  scale_x_continuous(breaks=seq(2020,2100,10), limits = c(2020,2100)) +
  labs(title = "Net-zero years")
nz.ranges

### Below X Gt/yr years --------------------------------------------------------
below.ranges <- ggplot(below.value, aes(x=below_threshold_year, y=variable)) +
  geom_boxplot() +
  geom_point(aes(shape=model,colour=scenario)) +
  theme_jsk() + mark_history(sy = 2025) +
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.minor.x = element_line(colour = "lightgrey", linetype = "dotted"),
        panel.grid.major.y = element_blank()) +
  ylab(NULL) +
  scale_x_continuous(breaks=seq(2020,2100,10), limits = c(2020,2100)) +
  labs(title = paste0("Below ", as.character(below.value.value/1e3), "Gt/yr"))
below.ranges

### Peak temperature values ----------------------------------------------------
peak.temp.ranges <- ggplot(peak %>% iamc_variable_keep_two_levels(levels = c(-2,-1)),
                           aes(x=peak_temperature, y=variable)) +
  geom_vline(xintercept = 1.5, linetype = "dashed") +
  geom_vline(xintercept = 2, linetype = "dashed") +
  geom_boxplot() +
  geom_point(aes(shape=model,colour=scenario)) +
  theme_jsk() +
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.minor.x = element_line(colour = "lightgrey", linetype = "dotted"),
        panel.grid.major.y = element_blank()) +
  ylab(NULL) +
  scale_x_continuous(breaks=seq(1.4,2.3,0.1), limits = c(1.4,2.3)) +
  labs(title = "Peak temperatures",
       # caption = cat(
       #   t2025 %>%  %>% pull(temp.range)
       # )
       )
peak.temp.ranges

### Combine and save out -------------------------------------------------------

#### Data -------------------------------------------------------
df.vllo.ranges <- below.value %>% left_join(nz) %>%
  left_join(peak %>% select(-unit) %>%
              iamc_variable_keep_two_levels(c(-2,-1)) %>%
              left_join(
                t2025 %>% iamc_variable_keep_two_levels(c(-2,-1)) %>%
                  rename(temp_range_2025 = range)
              ) %>%
            rename(temperature_probability=variable))



write_delim(file = here("figures", paste0("scenariomip-",
                                          "VLLO"),
                        paste0("peak_and_netzero_ranges.csv")),
            x = df.vllo.ranges,
            delim = ","
)

#### Plot -------------------------------------------------------
p.vllo.ranges <- ((nz.ranges + theme(legend.position = "none")) +
  (below.ranges) +
  (peak.temp.ranges + theme(legend.position = "none"))) +
  plot_layout(design = "ABC") +
  plot_annotation(title = "VLLO",
                  caption = paste("Downloaded scenario data on: ", DATA_DOWNLOAD_DATE))
save_ggplot(
  p = p.vllo.ranges,
  h = 150,
  w = 500,
  f = here("figures", paste0("scenariomip-",
                             "VLLO"),
           paste0("peak_and_netzero_ranges"))
)

