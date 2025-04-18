library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("patchwork")
library("ggthemes")
library("ggsci")
library("testthat")
library("stringr")
library("ggthemes")
library("pals")
# library("DescTools")
library("acid")

here::i_am("scenariomip.Rproj")

source(here("R", "utils.R"))

# Notes ----
# The list of variables is here: https://docs.google.com/spreadsheets/d/1H65lrzcBBBoUYKc4rjt1sJdbdLTf7YMRM-n2k325FDU/edit?usp=sharing
# This list was originally used to inquire with the teams whether they can extend their reporting, so this also gives an impression of what would be available from the models.
#
# The presentation on the basis of Oliver's figures is here: IIASA_energy_food_demand_2025-02-04.pptx (https://iiasahub.sharepoint.com/:p:/r/sites/eceprog/Shared%20Documents/SharedSocioEconomicPathways2023/Scenario_Vetting/DenHaag_2025-02-04/IIASA_energy_food_demand_2025-02-04.pptx?d=wa6f5b0978ccf4a20aacbc51915287401&csf=1&web=1&e=DYIaQV)
#
# For creating a very large pile of figures, let's plot the following:
# - Energy and service variables directly over time (as Oliver did)
# - Energy and service values per capita over time
# - Energy and service values per capita vs GDP per capita
# - Final energy per unit of service (GJ/m2, or GJ/pkm) vs time and vs GDPpc
# - Final energy intensity per unit of GDP (vs time and vs GDPpc)
# - Useful energy (either reported by the models, or calculated like Christoph does here (https://drive.google.com/file/d/1FhWyqdhNsgoYCZf1_kXuvYjtqEEgEPN6/view?usp=sharing)
#
# --> ADD: share of electricity in final energy.
#
#                  Geographic dimension will be a process to find out what is useful without overwhelming everyone... initially global is a good start, then R5 should give some insights. Model native regions (or several model native regions fitting on a single plot) would be useful for the modelling teams themselves, but probably not for a paper.


# Focus & potential insights ----

# - all over the place, or not? (="ALIGNMENT" _historical_; of representation and historical alignment)
#   * figures from 2005 - 2030  (all direct variables)
# - similar patterns / responses? (="ALIGNMENT" of trends / scenario interpretations)
#   * figures 2010 - 2100 (all direct variables)
#     -> loop over all variables from `vars.all`
#     -> select all that make sense; for per capita.
#     -> check if other 'derived'/'calculated' variables need to be added
#       * e.g. normalised (relative to 2025)
#       * e.g. share of electricity in final energy (by sector)
# - per capita: development narratives (= per/cap service VS per/cap GDP; structures of the economy)
#     -> 2010 - 2100; per/cap service [y-axis] VS per/cap GDP [x-axis]
#         * where sensible; focus on total freight/passenger/road/LDV + Stocks|LDV; Stocks|EV .
#         * top-level items; iron, steel, aluminum, copper
# ...
# - variable reporting overview? (= helpful; maybe oliver already has it...)
#   N.B. missing is 'Trade' and 'Consumption' (to match 'Production') variables


# Layout:
# 1) style
#   --> linetype per ssp?
# 2) regional content
#   - give teams also overview on the regional level
#     -> R5 (not R10) & native region
# 3) HTML/Powerpoint/just loads of figures/...
#   - no strong preference. all three would work. png are easier to work with.
#     => just png first (because HTML would require a bit too much thinking/tweaking for now)
#     => future; HTML where you can filter (e.g. what variable to view, with tabs as regional resolution)


# Where to save the figures? ----

path.figures <- here("figures", "demand")
path.figures.data <- here("figures_data", "demand")

if (!dir.exists(path.figures.data)) { dir.create(path.figures.data, recursive = TRUE) }

path.figures.convergence <- here("figures", "demand_convergence")
path.figures.data.convergence <- here("figures_data", "demand_convergence")

if (!dir.exists(path.figures.data.convergence)) { dir.create(path.figures.data.convergence, recursive = TRUE) }

path.figures.convergence.indicators <- here("figures", "demand_convergence_indicators")
path.figures.data.convergence.indicators <- here("figures_data", "demand_convergence_indicators")

if (!dir.exists(path.figures.data.convergence.indicators)) { dir.create(path.figures.data.convergence.indicators, recursive = TRUE) }


# Visualisation choices ----
STARTYEAR <- 2025

plot.model.colors <- c(
  "AIM" = "#4D5CAD",
  "COFFEE" = "#69BA7F",
  "GCAM" = "#759EA8",
  "IMAGE" = "#868367",
  "MESSAGE" = "#892F71",
  "REMIND" = "#facb1e",
  "WITCH" = "#fb6a4a"
)

plot.ssp.linetypes <- c(
  "SSP1" = "3313",
  "SSP2" = "solid",
  "SSP3" = "3333",
  "SSP4" = "1111",
  "SSP5" = "3131"
)

plot.target.linetypes <- c(
  "VLLO" = "solid",
  "VLHO" = "3131",
  "L" = "3333",
  "ML" = "3313",
  "M" = "1111",
  "H" = "dotted"
)

# plot.ssp.shapes

plot.region.colours.r5 <- c(
  "red",
  "magenta",
  "green4",
  "chocolate4",
  "darkgrey",
  "black"
)

plot.region.colours.r10 <- c(
  "red", # Africa
  "orange", # Middle East
  "green", # India+
  "green4", # Rest of Asia
  "magenta", # Latin America
  "chocolate4", # Reforming Economies
  "blue", # China+
  "lightblue4", # Europe
  "darkturquoise", # Pacific OECD
  "darkviolet", # North America
  "grey", # Other
  "black" # World
)

plot.region.colours.gn.gs.chn <- c(
  "red", # Global South
  "green4", # China
  "blue", # Global North
  "darkgrey" # World
)


# Helper functions ----
# ...
# MESSAGE: keep the one with GAINS

# Some units ----
exa <- 1e18
giga <- 1e9
mega <- 1e6
billion <- 1e9
million <- 1e6
pop.unit <- million # assume population is in millions.


# Variable selection ----

## Survey list
vars.survey <- vroom(here("data", "demand", "Demand_Variable_Survey.csv"), skip = 7) %>%
  pull(Variable)

## Misc
vars.socioecon <- c("Population", "GDP|MER", "GDP|PPP")

vars.fe <- c(
  "Final Energy",
  "Final Energy|Industry",
  "Final Energy|Residential and Commercial",
  "Final Energy|Transportation",
  "Final Energy|Non-Energy Use",
  "Final Energy|Electricity",
  "Final Energy|Transportation|Electricity",
  "Final Energy|Industry|Electricity",
  "Final Energy|Residential and Commercial|Electricity"
)
# to calculate:
# share of electricity in final energy (per sector)

## Full list of variables
vars.all <- c(
  vars.survey,
  vars.socioecon,
  vars.fe
)


# Loading IAM data ----
# IAM_SCENARIOS_LOCATION <- here("data", "data_vetting", "scens")
IAM_SCENARIOS_LOCATION <- "C:/Users/zaini/OneDrive - IIASA/Documents/ScenarioMIP demand"

# IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-02-17.csv"
# IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-03-05.csv" # version 'demand_world_r5_total_directvariables_v20250307_a.zip'
IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-04-16.csv" # version presented in ScenarioMIP demand-side meeting on 13.03.2025

scenarios.alldata <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, IAM_SCENARIOS_FILE), mode = "fast")


# only keep relevant variables
scenarios <- scenarios.alldata %>%
  filter(Variable %in% vars.all)
rm(scenarios.alldata) # remove large dataframe from environment to free up RAM
gc()

# to long format
scenarios <- scenarios %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  filter(year <= 2100)

# add some extra helpful information
scenarios <- scenarios %>%
  add_scenariomip_targets_to_IAM_scenarios() %>%
  add_ssp_basis_to_IAM_scenarios() %>%
  simplify_model_names(keep.full.model.name = T) %>%
  remove_scenarios_with_issues() %>% # temporary fix (which should be reported to the modelling teams)
  remove_all_zero_values() # temporary fix (which should be reported to the modelling teams)

# save some useful lists
model.list <- scenarios %>%
  pull(model) %>%
  unique()
model.list.simple <- scenarios %>%
  distinct(model) %>%
  simplify_model_names() %>%
  pull(model) %>%
  unique()


# Loading other data ----
# ...

# ... ... ... ... ... ... ... ... ... ------------------------------------------
# DIRECT VARIABLES -------------------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------

# totals (all direct variables) ----

path.figures.activity.total <- file.path(path.figures, "activity total")
path.figures.data.activity.total <- file.path(path.figures.data, "activity total")

if (!dir.exists(path.figures.data.activity.total)) {
  dir.create(path.figures.data.activity.total, recursive = TRUE)
}

## Global ----
for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    region == "World",
    variable == v
  )

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

  if (nrow(plot.data) > 0) {
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()
    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_wrap(~target, ncol = 3) +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.activity.total, paste0("world_total_", clean_string(v)))
    )

    write_csv(plot.data, file.path(path.figures.data.activity.total, paste0("world_total_", clean_string(v), ".csv")))
  } else {
    print(paste0("No data reported for ", v))
  }
}
## R5 ----
for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable == v
  )

  # order data by scenarioMIP target scenario and by region
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
  plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


  if (nrow(plot.data) > 0) {
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()
    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(region ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.activity.total, paste0("r5_total_", clean_string(v)))
    )

    write_csv(plot.data, file.path(path.figures.data.activity.total, paste0("r5_total_", clean_string(v), ".csv")))
  } else {
    print(paste0("No data repoted for ", v))
  }
}


# ... ... ... ... ... ... ... ... ... ------------------------------------------
# DIRECT VARIABLES (Per Capita) ------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------

# Create subfolders where to save plots and data
path.figures.activity.percap <- file.path(path.figures, "activity per cap")
path.figures.data.activity.percap <- file.path(path.figures.data, "activity per cap")

if (!dir.exists(path.figures.data.activity.percap)) {
  dir.create(path.figures.data.activity.percap, recursive = TRUE)
}


to_per_capita_scenariomip <- function(df, y.u, p.u = pop.unit) {
  # formatting
  df <- df %>%
    rename(simple.model = model, model = full.model.name)

  # to per capita
  df <- df %>%
    to_per_capita() %>%
    mutate(value = value / p.u)

  # correct the units
  if (y.u == "EJ/yr") {
    df <- df %>%
      mutate(
        value = value * exa / giga,
        unit = "GJ/cap/yr"
      )
  } else if (y.u == "billion pkm/yr") {
    df <- df %>%
      mutate(
        value = value * billion,
        unit = "pkm/cap/yr"
      )
  } else if (y.u == "billion tkm/yr") {
    df <- df %>%
      mutate(
        value = value * billion,
        unit = "tkm/cap/yr"
      )
  } else if (y.u == "Mt/yr") {
    df <- df %>%
      mutate(
        value = value * mega,
        unit = "t/cap/yr"
      )
  } else if (y.u == "bn m2") { # change in common-definitions to 'billion m2'?
    df <- df %>%
      mutate(
        value = value * billion,
        unit = "m2/cap"
      )
  } else if (y.u == "million") {
    df <- df %>%
      mutate(
        value = value * million,
        unit = "unit/cap"
      )
  } else if (y.u == "°C-days") { # change in common-definitions to '°C-days/yr' -> average per capita already?
    df <- df %>%
      mutate(
        value = value,
        unit = "°C-days/cap/yr"
      )
  } else if (y.u == "billion USD_2010/yr") {
    df <- df %>%
      mutate(
        value = value * billion,
        unit = "USD_2010/cap/yr"
      )
  } else {
    print(paste0("The unit for variable ", v, " is not processed to per capita variables in this script."))
    df <- df %>%
      mutate(
        value = value,
        unit = paste0(unit, " [per capita]")
      )
  }

  # formatting
  df <- df %>%
    rename(model = simple.model, full.model.name = model)

  return(df)
}


## Global ----
for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    region == "World",
    variable %in% c(
      v,
      "Population"
    )
  )
  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }



    # plot
    if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
      print(paste0("Plotting ", v, " (per capita)"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()

      p <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_wrap(~target, ncol = 3) +
        mark_history() +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )
      p

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("world_percapita_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.activity.percap, paste0("world_percapita_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data repoted for ", v))
  }
}

## R5 ----
for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      v,
      "Population"
    )
  )



  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))

    # plot
    if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
      print(paste0("Plotting ", v, " (per capita)"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()
      p <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r5_percapita_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.activity.percap, paste0("r5_percapita_", clean_string(v), ".csv")))


      # Remove models with no data
      plot.data <- plot.data %>% drop_na(value)

      p.convergence <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.region.colours) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      p.convergence

      save_ggplot(
        p = p.convergence,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r5_percapita_conv_", clean_string(v)))
      )

      p.convergence.ssp <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ ssp, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = target
          )
        ) +
        scale_color_manual(values = plot.region.colours) +
        scale_linetype_manual(values = plot.target.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      p.convergence.ssp

      save_ggplot(
        p = p.convergence.ssp,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r5_percapita_conv_ssp_", clean_string(v)))
      )
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
}



## R10 ----

for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    (region == "World" | grepl(region, pattern = " (R10)", fixed = T)),
    variable %in% c(
      v,
      "Population"
    )
  )



  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Africa (R10)", "India+ (R10)", "Rest of Asia (R10)", "Latin America (R10)", "Middle East (R10)", "Reforming Economies (R10)", "China+ (R10)", "Europe (R10)", "Pacific OECD (R10)", "North America (R10)", "Other (R10)", "World"))

    # plot
    if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
      print(paste0("Plotting ", v, " (per capita)"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()

      ### a) Faceted by region

      p <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )
      p

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r10_percapita_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.activity.percap, paste0("r10_percapita_", clean_string(v), ".csv")))


      ### b) Convergence, by scenarioMIP scenario

      # Remove models with no data
      plot.data <- plot.data %>% drop_na(value)

      p.convergence <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.region.colours.r10) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      p.convergence

      save_ggplot(
        p = p.convergence,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r10_percapita_conv_", clean_string(v)))
      )

      ### c) Convergence, by SSP

      p.convergence.ssp <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ ssp, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = target
          )
        ) +
        scale_color_manual(values = plot.region.colours.r10) +
        scale_linetype_manual(values = plot.target.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      p.convergence.ssp

      save_ggplot(
        p = p.convergence.ssp,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r10_percapita_conv_ssp_", clean_string(v)))
      )
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of variables loop


## Convergence in R10 ----


# Let's start analyzing convergence in SSP scenarios

# I start from the plot.data df that was produced in R10 section

for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    (region == "World" | grepl(region, pattern = " (R10)", fixed = T)),
    variable %in% c(
      v,
      "Population"
    )
  )



  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

    # plot
    if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
      print(paste0("Plotting ", v, " (per capita) convergence"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()

      ### 1) Standard deviation ----

      conv.data <- plot.data %>%
        group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
        summarise(st.dev = sd(value)) %>%
        ungroup()


      p.sd <- ggplot(
        conv.data,
        aes(
          x = year, y = st.dev,
          group = interaction(full.model.name, scenario, variable, unit)
        )
      ) +
        facet_wrap(~ssp, ncol = 5) +
        mark_history() +
        geom_point(
          aes(
            colour = model,
            shape = target
          )
        ) +
        geom_line(aes(color = model)) +
        scale_color_manual(values = plot.model.colors) +
        theme_jsk() +
        labs(
          y = paste0("Std Deviation (", y.unit, ")"),
          x = NULL,
          title = paste("Convergence of", v),
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(colour = guide_legend(title = NULL))

      p.sd

      save_ggplot(
        p = p.sd,
        h = 130,
        w = 350,
        format = "png",
        f = file.path(path.figures.convergence.indicators, paste0("sd_percapita_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("sd_percapita_", clean_string(v), ".csv")))



      ### 2) Gini ----

      # Attach population data, to compute population-weighted gini
      pop.data <- scenarios %>% filter(
        (region == "World" | grepl(region, pattern = " (R10)", fixed = T)),
        variable == "Population"
      )

      conv.data.gini <- plot.data %>%
        left_join(pop.data, by = c("model", "scenario", "region", "year", "target", "ssp", "full.model.name")) %>%
        rename(
          variable = variable.x,
          unit = unit.x,
          value = value.x,
          population = value.y,
          pop.unit = unit.y
        ) %>%
        select(-variable.y)

      gini.pop.weighted.data <- conv.data.gini %>%
        drop_na(population) %>%
        group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
        reframe(gini.pop.weighted = weighted.gini(x = value, w = population)$bcwGini) %>%
        ungroup()

      # gini.data <- plot.data %>%
      #   group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
      #   reframe(gini = gini(value)$bcGini) %>%
      #   # N.B. We pick the bias-corrected Gini (bcGini), because the sample is very small (only 10 regions) and w/o bias correction the Gini estimate tends to be downward-biased (underestimation of inequality)
      #   ungroup()

      # Reorder ScenarioMIP scenarios
      gini.pop.weighted.data$target <- factor(gini.pop.weighted.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

      p.gini.w <- ggplot(
        gini.pop.weighted.data,
        aes(
          x = year, y = gini.pop.weighted,
          group = interaction(full.model.name, scenario, variable, unit)
        )
      ) +
        facet_wrap(~ssp, ncol = 5) +
        mark_history() +
        geom_point(
          aes(
            colour = model,
            shape = target
          )
        ) +
        geom_line(aes(color = model)) +
        scale_color_manual(values = plot.model.colors) +
        theme_jsk() +
        labs(
          y = paste0("Gini (pop weighted)"),
          x = NULL,
          title = paste("Convergence of", v),
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(colour = guide_legend(title = NULL))

      p.gini.w


      # p.gini <- ggplot(
      #   gini.data,
      #   aes(
      #     x = year, y = gini,
      #     group = interaction(full.model.name, scenario, variable, unit)
      #   )
      # ) +
      #   facet_wrap(~ssp) +
      #   mark_history() +
      #   geom_point(
      #     aes(
      #       colour = model,
      #       shape = target
      #     )
      #   ) +
      #   scale_color_manual(values = plot.model.colors) +
      #   theme_jsk() +
      #   labs(
      #     y = paste0("Gini"),
      #     x = NULL,
      #     title = paste("Convergence of", v),
      #     subtitle = "per capita",
      #     caption = paste0("File: ", IAM_SCENARIOS_FILE)
      #   ) +
      #   guides(colour = guide_legend(title = NULL))
      #
      # p.gini


      save_ggplot(
        p = p.gini.w,
        h = 130,
        w = 350,
        format = "png",
        f = file.path(path.figures.convergence.indicators, paste0("gini_percapita_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("gini_percapita_", clean_string(v), ".csv")))



      ### 3) Ratio ----

      # North America to Africa
      if (nrow(plot.data %>% filter(region == "North America (R10)")) > 0) {   # If data exists for "North America(R10)"

        ratio.nam.afr.data <- plot.data %>%
          filter(region %in% c("Africa (R10)", "North America (R10)")) %>%
          group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
          pivot_wider(names_from = region, values_from = value) %>%
          mutate(ratio = `North America (R10)` / `Africa (R10)`) %>%
          ungroup()

        # Reorder ScenarioMIP scenarios
        ratio.nam.afr.data$target <- factor(ratio.nam.afr.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

        p.ratio.nam.afr <- ggplot(
          ratio.nam.afr.data,
          aes(
            x = year, y = ratio,
            group = interaction(full.model.name, scenario, variable, unit)
          )
        ) +
          facet_wrap(~ssp, ncol=5) +
          mark_history() +
          geom_point(
            aes(
              colour = model,
              shape = target
            )
          ) +
          geom_line(aes(color = model)) +
          scale_color_manual(values = plot.model.colors) +
          theme_jsk() +
          labs(
            y = paste0("Ratio NAM/AFR"),
            x = NULL,
            title = paste("Convergence of", v),
            subtitle = "per capita",
            caption = paste0("File: ", IAM_SCENARIOS_FILE)
          ) +
          guides(colour = guide_legend(title = NULL))

        p.ratio.nam.afr


        save_ggplot(
          p = p.ratio.nam.afr,
          h = 130,
          w = 350,
          format = "png",
          f = file.path(path.figures.convergence.indicators, paste0("ratio_nam_afr_percapita_", clean_string(v)))
        )

        write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("ratio_nam_afr_percapita_", clean_string(v), ".csv")))


        # Pacific OECD to Africa

        ratio.pao.afr.data <- plot.data %>%
          filter(region %in% c("Africa (R10)", "Pacific OECD (R10)")) %>%
          group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
          pivot_wider(names_from = region, values_from = value) %>%
          mutate(ratio = `Pacific OECD (R10)` / `Africa (R10)`) %>%
          ungroup()

        # Reorder ScenarioMIP scenarios
        ratio.pao.afr.data$target <- factor(ratio.pao.afr.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

        p.ratio.pao.afr <- ggplot(
          ratio.pao.afr.data,
          aes(
            x = year, y = ratio,
            group = interaction(full.model.name, scenario, variable, unit)
          )
        ) +
          facet_wrap(~ssp, ncol = 5) +
          mark_history() +
          geom_point(
            aes(
              colour = model,
              shape = target
            )
          ) +
          geom_line(aes(color = model)) +
          scale_color_manual(values = plot.model.colors) +
          theme_jsk() +
          labs(
            y = paste0("Ratio PAO/AFR"),
            x = NULL,
            title = paste("Convergence of", v),
            subtitle = "per capita",
            caption = paste0("File: ", IAM_SCENARIOS_FILE)
          ) +
          guides(colour = guide_legend(title = NULL))

        p.ratio.pao.afr


        save_ggplot(
          p = p.ratio.pao.afr,
          h = 130,
          w = 350,
          format = "png",
          f = file.path(path.figures.convergence.indicators, paste0("ratio_pao_afr_percapita_", clean_string(v)))
        )

        write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("ratio_pao_afr_percapita_", clean_string(v), ".csv")))

      } else {
        print("No sufficient R10 data to calculate convergence as a ratio")
      }


      ### Three indicators together ----

      # Create individual indicator datasets with a common value column
      conv.data.ind <- conv.data %>%
        mutate(indicator = paste0("Std Dev (", y.unit, ")"), value = st.dev) %>%
        select(model, scenario, variable, unit, year, target, ssp, full.model.name, indicator, value)

      gini.data.ind <- gini.pop.weighted.data %>%
        mutate(indicator = "Gini (pop weighted)", value = gini.pop.weighted) %>%
        select(model, scenario, variable, unit, year, target, ssp, full.model.name, indicator, value)

      # Only create the ratio dataset if there is NAM data present.
      if (nrow(plot.data %>% filter(region == "North America (R10)")) > 0) {
        ratio.data.ind <- ratio.nam.afr.data %>%
          mutate(indicator = "Ratio NAM/AFR",
                 value = ratio) %>%
          select(model, scenario, variable, unit, year, target, ssp, full.model.name, indicator, value)

        combined.data <- bind_rows(conv.data.ind, gini.data.ind, ratio.data.ind)
      } else {
        combined.data <- bind_rows(conv.data.ind, gini.data.ind)
      }

      # Reorder indicators
      combined.data$indicator <- factor(combined.data$indicator, levels = c(paste0("Std Dev (", y.unit, ")"), "Gini (pop weighted)", "Ratio NAM/AFR"))

      # Now create a plot with three rows (indicators) and five columns (ssp)
      p.combined <- ggplot(combined.data,
                           aes(x = year, y = value,
                               group = interaction(model, scenario, full.model.name, indicator))) +
        facet_grid(indicator ~ ssp, scales = "free_y", switch = "y") +
        geom_line(aes(color = model)) +
        geom_point(aes(color = model, shape = target)) +
        scale_color_manual(values = plot.model.colors) +
        scale_y_continuous(position = "left") +  # ensures axis tick labels are on the left
        labs(
          x = "Year",
          y = NULL,
          title = paste("Convergence of", v),
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        theme_jsk() +
        guides(
          color = guide_legend(title = "Model"),
          shape = guide_legend(title = "Target")
        ) +
        theme(strip.placement = "outside",           # moves facet strips outside the panels
              strip.text.y.left = element_text(angle = 90))  # rotates the text as needed

      p.combined


      save_ggplot(
        p = p.combined,
        h = 180,
        w = 300,
        format = "png",
        f = file.path(path.figures.convergence.indicators, paste0("conv_indicators_combined_", clean_string(v)))
      )

    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of variables loop






## GN vs GS ----

create_globalnorth_globalsouth <- function(df, china.in.GN = FALSE) {
  # Select R10 regions
  df <- df %>%
    filter(region == "World" | grepl(region, pattern = " (R10)", fixed = T))

  global.south.regions <- c(
    "Africa (R10)",
    "Middle East (R10)",
    "India+ (R10)",
    "Rest of Asia (R10)",
    "Latin America (R10)"
  )
  global.north.regions <- c(
    "North America (R10)",
    "Europe (R10)",
    "Reforming Economies (R10)",
    "Pacific OECD (R10)",
    "Other (R10)"
  )

  if (china.in.GN == TRUE) {
    global.north.regions <- c(global.north.regions, "China+ (R10)")
  }


  df_aggr <- df %>%
    mutate(region = case_when(
      region %in% global.south.regions ~ "Global South",
      region %in% global.north.regions ~ "Global North",
      TRUE ~ region
    )) %>%
    group_by(model, scenario, full.model.name, variable, year, region, unit, target, ssp) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


  return(df_aggr)
}




for (v in vars.all) {
  plot.data <- scenarios %>% filter(variable %in% c(
    v,
    "Population"
  ))


  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    # Aggregate to GN and GS
    plot.data <- create_globalnorth_globalsouth(df = plot.data, china.in.GN = FALSE)

    # Calculate per capita
    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Global South", "China+ (R10)", "Global North", "World"))

    # plot
    if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
      print(paste0("Plotting ", v, " (per capita)"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()

      p <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )
      p

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("gn_gs_percapita_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.activity.percap, paste0("gn_gs_percapita_", clean_string(v), ".csv")))


      # Remove models with no data
      plot.data <- plot.data %>% drop_na(value)

      p.convergence <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.region.colours.gn.gs.chn) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      p.convergence

      save_ggplot(
        p = p.convergence,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("gn_gs_percapita_conv_", clean_string(v)))
      )

      p.convergence.ssp <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ ssp, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = target
          )
        ) +
        scale_color_manual(values = plot.region.colours.gn.gs.chn) +
        scale_linetype_manual(values = plot.target.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      p.convergence.ssp

      save_ggplot(
        p = p.convergence.ssp,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("gn_gs_percapita_conv_ssp_", clean_string(v)))
      )
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
}







## b) vs GDPpc ----
# Per GDP on x-axis [per/cap service over per/cap GDP]
# ... use `to_per_gdp()`
# example: pkm/$
gdp.unit <- billion

### Global ----

plot.data.gdp.percap <- scenarios %>%
  filter(
    region == "World",
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    region == "World",
    variable %in% c(
      v,
      "Population"
    )
  )


  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0 & v != "GDP|PPP") {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }

    y.unit.percapita <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


    # plot
    if (nrow(plot.data) > 0) {
      print(paste0("Plotting ", v, " per capita (over GDP/cap)"))

      p <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_wrap(~target, ncol = 3) +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit.percapita,
          x = gdp.percap.unit,
          title = v,
          subtitle = "per capita, vs GDP per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("world_percapita_overgdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.activity.percap, paste0("world_percapita_overgdp_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data repoted for ", v))
  }
} # end of loop over variables


### R5 ----
plot.data.gdp.percap <- scenarios %>%
  filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


for (v in vars.all) {
  plot.data <- scenarios %>% filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      v,
      "Population"
    )
  )

  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0 & v != "GDP|PPP") {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
    } else {
      print("Unit error.")
    }

    y.unit.percapita <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)


    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


    # plot
    if (nrow(plot.data) > 0) {
      print(paste0("Plotting ", v, " per capita (over GDP/cap)"))

      p <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit.percapita,
          x = gdp.percap.unit,
          title = v,
          subtitle = "per capita, vs GDP per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r5_percapita_overgdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.activity.percap, paste0("r5_percapita_overgdp_", clean_string(v), ".csv")))



      #### Convergence plots ----

      # Remove models with no data
      plot.data <- plot.data %>% drop_na(.data[[v]])


      p.convergence <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_grid(model ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.region.colours.r5) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit.percapita,
          x = NULL,
          title = v,
          subtitle = "per capita, vs GDP per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )


      save_ggplot(
        p = p.convergence,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r5_percapita_overgdp_conv_", clean_string(v)))
      )

      # Faceting by SSP
      p.convergence.ssp <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_grid(model ~ ssp, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = target
          )
        ) +
        scale_color_manual(values = plot.region.colours.r5) +
        scale_linetype_manual(values = plot.target.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit.percapita,
          x = NULL,
          title = v,
          subtitle = "per capita, vs GDP per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )


      save_ggplot(
        p = p.convergence.ssp,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.activity.percap, paste0("r5_percapita_overgdp_conv_ssp_", clean_string(v)))
      )
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of loop over variables





# ... ... ... ... ... ... ... ... ... ------------------------------------------
# CALCULATED/DERIVED VARIABLES -------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------

# Electrification ----

# Create subfolders where to save plots and data
path.figures.electrification <- file.path(path.figures, "electrification")
path.figures.data.electrification <- file.path(path.figures.data, "electrification")

if (!dir.exists(path.figures.data.electrification)) {
  dir.create(path.figures.data.electrification, recursive = TRUE)
}


calculate_electrification <- function(df) {
  df %>%
    filter(variable %in% c(
      "Final Energy",
      "Final Energy|Electricity",
      "Final Energy|Transportation",
      "Final Energy|Industry",
      "Final Energy|Residential and Commercial",
      "Final Energy|Transportation|Electricity",
      "Final Energy|Industry|Electricity",
      "Final Energy|Residential and Commercial|Electricity"
    )) %>%
    mutate(total.or.electricity = ifelse(
      grepl(variable, pattern = "Electricity", fixed = T),
      "Electricity",
      "Total"
    )) %>%
    remove_variable_lastlevel_match("Electricity") %>%
    pivot_wider(names_from = total.or.electricity, values_from = value) %>%
    mutate(`Electrification rate` = Electricity / Total * 100) %>%
    mutate_cond(variable == "Final Energy", variable = "Final Energy|Total") %>%
    remove_variable_firstlevel_match("Final Energy") %>%
    mutate(
      variable = paste0("Electrification of Final Energy (", variable, ")"),
      unit = "%"
    ) %>%
    rename(value = `Electrification rate`) %>%
    return()
}

scenarios.electrification <- scenarios %>% calculate_electrification()

vars.electrification <- scenarios.electrification %>%
  pull(variable) %>%
  unique()

## a) vs time ----

### Global ----
for (v in vars.electrification) {
  plot.data <- scenarios.electrification %>% filter(
    region == "World",
    variable == v
  )

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


  if (nrow(plot.data) > 0) {
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()
    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_wrap(~target, ncol = 3) +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.electrification, paste0("world_electrification_", clean_string(v)))
    )

    write_csv(plot.data, file.path(path.figures.data.electrification, paste0("world_electrification_", clean_string(v), ".csv")))
  } else {
    print(paste0("No data repoted for ", v))
  }
}
### R5 ----
for (v in vars.electrification) {
  plot.data <- scenarios.electrification %>% filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable == v
  )

  # order data by region
  plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))

  if (nrow(plot.data) > 0) {
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()
    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(region ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = "vs GDP per capita",
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.electrification, paste0("r5_electrification_", clean_string(v)))
    )

    write_csv(plot.data, file.path(path.figures.data.electrification, paste0("r5_electrification_", clean_string(v), ".csv")))
  } else {
    print(paste0("No data repoted for ", v))
  }
}



## b) vs GDPpc ----
# Per GDP on x-axis [per/cap service over per/cap GDP]
# ... use `to_per_gdp()`
# example: pkm/$
gdp.unit <- billion

### Global ----

plot.data.gdp.percap <- scenarios %>%
  filter(
    region == "World",
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


for (v in c(
  "Energy Service|Residential|Floor Space",
  "Energy Service|Transportation|Freight",
  "Energy Service|Transportation|Passenger",
  "Stocks|Transportation|Light-Duty Vehicle",
  "Stocks|Transportation|Light-Duty Vehicle|Battery-Electric",
  vars.electrification
)) {
  plot.data <- scenarios %>%
    bind_rows(scenarios.electrification %>% select(-c(Total, Electricity))) %>%
    filter(
      region == "World",
      variable %in% c(
        v,
        "Population"
      )
    )


  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (v %nin% vars.electrification) {
      if (!is.na(y.unit)) {
        plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
      } else {
        print("Unit error.")
      }
    }
    y.unit.percapita <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


    # plot
    if (nrow(plot.data) > 0) {
      print(paste0("Plotting ", v, " (over GDP/cap)"))

      p <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_wrap(~target, ncol = 3) +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = gdp.percap.unit,
          title = v,
          subtitle = "vs GDP per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.electrification, paste0("world_overgdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.electrification, paste0("world_overgdp_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data repoted for ", v))
  }
} # end of loop over variables


### R5 ----
plot.data.gdp.percap <- scenarios %>%
  filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()

for (v in c(
  "Energy Service|Residential|Floor Space",
  "Energy Service|Transportation|Freight",
  "Energy Service|Transportation|Passenger",
  "Stocks|Transportation|Light-Duty Vehicle",
  "Stocks|Transportation|Light-Duty Vehicle|Battery-Electric",
  vars.electrification
)) {
  plot.data <- scenarios %>%
    bind_rows(scenarios.electrification %>% select(-c(Total, Electricity))) %>%
    filter(
      (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
      variable %in% c(
        v,
        "Population"
      )
    )

  # order data by scenarioMIP target scenario and by region
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
  plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


  # change to per capita units
  if (nrow(plot.data %>% filter(variable != "Population")) > 0) {
    y.unit <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    if (v %nin% vars.electrification) {
      if (!is.na(y.unit)) {
        plot.data <- to_per_capita_scenariomip(df = plot.data, y.u = y.unit)
      } else {
        print("Unit error.")
      }
    }
    y.unit.percapita <- plot.data %>%
      filter(variable != "Population") %>%
      pull(unit) %>%
      unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)



    # plot
    if (nrow(plot.data) > 0) {
      print(paste0("Plotting ", v, " (over GDP/cap)"))

      p <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit.percapita,
          x = gdp.percap.unit,
          title = v,
          subtitle = "vs GDP per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.electrification, paste0("r5_overgdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.electrification, paste0("r5_overgdp_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per capita data for ", v))
    }
  } else {
    print(paste0("No data repoted for ", v))
  }
} # end of loop over variables




# Normalised to 2025 ---
# ... use `normalise_iamc_long(starting.year = STARTYEAR)`





# Final energy per unit of service (GJ/m2, or GJ/pkm) [per/cap service over per/cap GDP]   ----

# Create subfolders where to save plots and data
path.figures.fe.per.service <- file.path(path.figures, "FE per service")
path.figures.data.fe.per.service <- file.path(path.figures.data, "FE per service")

if (!dir.exists(path.figures.data.fe.per.service)) {
  dir.create(path.figures.data.fe.per.service, recursive = TRUE)
}


to_per_unit_service_scenariomip <- function(df, var.service) {
  # formatting
  df <- df %>%
    rename(simple.model = model, model = full.model.name)

  # Extract current unit of service (e.g. "bn m2")
  service.unit <- df %>%
    filter(variable == var.service) %>%
    pull(unit) %>%
    unique()

  # Correct the unit of the service, e.g. bn m2 to m2
  if (service.unit == "bn m2") {
    df <- df %>%
      mutate(value = if_else(variable == var.service, value * billion, value)) %>%
      mutate(unit = if_else(variable == var.service, "m2", unit))
  } else if (service.unit == "billion pkm/yr") {
    df <- df %>%
      mutate(value = if_else(variable == var.service, value * billion, value)) %>%
      mutate(unit = if_else(variable == var.service, "pkm/yr", unit))
  } else if (service.unit == "billion tkm/yr") {
    df <- df %>%
      mutate(value = if_else(variable == var.service, value * billion, value)) %>%
      mutate(unit = if_else(variable == var.service, "tkm/yr", unit))
  } else if (service.unit == "Mt/yr") {
    df <- df %>%
      mutate(value = if_else(variable == var.service, value * mega, value)) %>%
      mutate(unit = if_else(variable == var.service, "t/yr", unit))
  }

  formatted.service.unit <- df %>%
    filter(variable == var.service) %>%
    pull(unit) %>%
    unique()

  df.service <- df %>%
    filter(variable == var.service) %>%
    rename(service.value = value) %>%
    select(model, scenario, region, year, service.value)

  # Calculate variable per unit of service
  df <- df %>%
    filter(variable != var.service) %>%
    left_join(df.service) %>%
    mutate(value = value / service.value) %>%
    mutate(unit = paste0(unit, "/", formatted.service.unit)) %>%
    select(-service.value)

  unit.per.service <- unique(df$unit)


  # correct the units
  if (unit.per.service == "EJ/yr/m2") {
    df <- df %>%
      mutate(
        value = value * exa / giga,
        unit = "GJ/yr/m2"
      )
  } else if (unit.per.service == "EJ/yr/pkm/yr") {
    df <- df %>%
      mutate(
        value = value * exa / mega,
        unit = "MJ/pkm"
      )
  } else if (unit.per.service == "EJ/yr/tkm/yr") {
    df <- df %>%
      mutate(
        value = value * exa / mega,
        unit = "MJ/tkm"
      )
  } else if (unit.per.service == "EJ/yr/t/yr") {
    df <- df %>%
      mutate(
        value = value * exa / giga,
        unit = "GJ/t"
      )
  } else {
    print(paste0("The unit for variable ", v, " is not processed to per capita variables in this script."))
    df <- df %>%
      mutate(
        value = value,
        unit = paste0(unit, " [per unit service]")
      )
  }

  # formatting
  df <- df %>%
    rename(model = simple.model, full.model.name = model)

  return(df)
}


#' Final Energy variables to be calculated per unit of service:
#' "Final Energy|Residential and Commercial",
#' "Final Energy|Residential and Commercial|Electricity",
#' "Final Energy|Transportation",
#' "Final Energy|Industry"
#'
#' Energy service variables:
#' Main ones:
#' "Energy Service|Residential and Commercial|Floor Space"
#' "Energy Service|Transportation|Passenger"
#' "Energy Service|Transportation|Freight"
#'
#' "Production|Iron and Steel" (= "Production|Iron and Steel|Iron" + "Production|Iron and Steel|Steel")
#' "Production|Non-Metallic Minerals|Cement"
#'
#' Others:
#' "Energy Service|Residential|Floor Space"
#' "Energy Service|Commercial|Floor Space"
#'
#' Others, probably not used:
#' "Energy Service|Residential|Cooling Degree Days"
#' "Energy Service|Residential|Heating Degree Days"
#' "Energy Service|Transportation|Freight"
#' "Energy Service|Transportation|Freight|Truck"
#' "Energy Service|Transportation|Freight|Truck|Battery-Electric"
#' "Energy Service|Transportation|Freight|Truck|Fuel-Cell-Electric"
#' "Energy Service|Transportation|Freight|Truck|Internal Combustion"
#' "Energy Service|Transportation|Freight|Truck|Plug-in Hybrid"
#' "Energy Service|Transportation|Passenger|Active Transport [Share]"
#' "Energy Service|Transportation|Passenger|Public Transport [Share]"
#' "Energy Service|Transportation|Passenger|Road"
#' "Energy Service|Transportation|Passenger|Road|Light-Duty Vehicle"




# Create a new dataframe with the summed values per group
iron_and_steel <- scenarios %>%
  group_by(model, scenario, region, year, target, ssp, full.model.name) %>%
  summarise(value = sum(
    value[variable %in% c(
      "Production|Iron and Steel|Iron",
      "Production|Iron and Steel|Steel"
    )],
    na.rm = TRUE
  )) %>%
  mutate(variable = "Production|Iron and Steel") %>%
  mutate(unit = "Mt/yr") %>%
  ungroup()

# Bind the new rows back to the original dataframe
scenarios.with.aggregations <- bind_rows(scenarios, iron_and_steel)

## choose variable to be calculated per unit of service: ----

v <- "Final Energy|Residential and Commercial"
v.service <- "Energy Service|Residential and Commercial|Floor Space"

#' Final Energy variables to be calculated per unit of service:
#' "Final Energy|Residential and Commercial",
#' "Final Energy|Residential and Commercial|Electricity",
#' "Final Energy|Transportation",
#' "Final Energy|Industry"
#'
#' Energy service variables:
#' Main ones:
#' "Energy Service|Residential and Commercial|Floor Space"
#' "Energy Service|Transportation|Passenger"
#' "Energy Service|Transportation|Freight"
#'
#' "Production|Iron and Steel" (= "Production|Iron and Steel|Iron" + "Production|Iron and Steel|Steel")
#' "Production|Non-Metallic Minerals|Cement"

## a) vs time ----

### Global ----
# for (v in vars.fe.per.service){

# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)

plot.data <- scenarios.with.aggregations %>% filter(
  region == "World",
  variable %in% c(
    v,
    v.service
  )
)

# change to per service units (m2 or pkm)
if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # only if you have data for variable v...

  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


  # plot
  if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # again, only if you have data for variable v...

    print(paste0("Plotting ", v, " per unit of ", v.service))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()
    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_wrap(~target, ncol = 3) +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("world_perservice_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.fe.per.service, paste0("world_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))
  } else {
    print(paste0("Something went wrong in calculating per unit of service data for ", v))
  }
} else {
  print(paste0("No data reported for ", v))
}

# } # end of loop over vars.fe.per.service



### R5 ----

# for (v in vars.fe.perservice){

# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)

plot.data <- scenarios.with.aggregations %>% filter(
  (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
  variable %in% c(
    v,
    v.service
  )
)

# change to per service units (m2 or pkm)
if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # only if you have data for variable v...

  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  # order data by scenarioMIP target scenario and by region
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
  plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


  # plot
  if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # again, only if you have data for variable v...

    print(paste0("Plotting ", v, " per unit of ", v.service))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()
    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(region ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r5_perservice_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.fe.per.service, paste0("r5_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))


    #### Convergence plots

    # Remove models with no data
    plot.data <- plot.data %>% drop_na(value)

    p.convergence <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(model ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = region,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.region.colours.r5) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )


    save_ggplot(
      p = p.convergence,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r5_perservice_conv_", clean_string(v)))
    )

    # Faceting by SSP
    p.convergence.ssp <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(model ~ ssp, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = region,
          linetype = target
        )
      ) +
      scale_color_manual(values = plot.region.colours.r5) +
      scale_linetype_manual(values = plot.target.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )


    save_ggplot(
      p = p.convergence.ssp,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r5_perservice_conv_ssp_", clean_string(v)))
    )
  } else {
    print(paste0("Something went wrong in calculating per capita data for ", v))
  }
} else {
  print(paste0("No data reported for ", v))
}

# } # end of loop over final energy variables



### R10 ----

# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)

plot.data <- scenarios.with.aggregations %>% filter(
  (region == "World" | grepl(region, pattern = " (R10)", fixed = T)),
  variable %in% c(
    v,
    v.service
  )
)

# change to per service units (m2 or pkm)
if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # only if you have data for variable v...

  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  # order data by scenarioMIP target scenario and by region
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
  plot.data$region <- factor(plot.data$region, levels = c("Africa (R10)", "India+ (R10)", "Rest of Asia (R10)", "Latin America (R10)", "Middle East (R10)", "Reforming Economies (R10)", "China+ (R10)", "Europe (R10)", "Pacific OECD (R10)", "North America (R10)", "Other (R10)", "World"))


  # plot
  if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # again, only if you have data for variable v...

    print(paste0("Plotting ", v, " per unit of ", v.service))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()

    ### Faceted by region

    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(region ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )
    p

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r10_perservice_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.fe.per.service, paste0("r10_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))


    #### Convergence, by ScenarioMIP scenario

    # Remove models with no data
    plot.data <- plot.data %>% drop_na(value)

    p.convergence <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(model ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = region,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.region.colours.r10) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )
    p.convergence

    save_ggplot(
      p = p.convergence,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r10_perservice_conv_", clean_string(v)))
    )


    ### Convergence, by SSP scenario

    p.convergence.ssp <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(model ~ ssp, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = region,
          linetype = target
        )
      ) +
      scale_color_manual(values = plot.region.colours.r10) +
      scale_linetype_manual(values = plot.target.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )
    p.convergence.ssp

    save_ggplot(
      p = p.convergence.ssp,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r10_perservice_conv_ssp_", clean_string(v)))
    )
  } else {
    print(paste0("Something went wrong in calculating per capita data for ", v))
  }
} else {
  print(paste0("No data reported for ", v))
}


## Convergence in R10 ----


# Let's start analyzing convergence in SSP scenarios

# I start from the plot.data df that was produced in R10 section

# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)

plot.data <- scenarios.with.aggregations %>% filter(
  (region == "World" | grepl(region, pattern = " (R10)", fixed = T)),
  variable %in% c(
    v,
    v.service
  )
)

# change to per service units (m2 or pkm)
if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # only if you have data for variable v...

  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


  # plot
  if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # again, only if you have data for variable v...

    print(paste0("Plotting ", v, " per unit of ", v.service))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()

    ### 1) Standard deviation ----

    conv.data <- plot.data %>%
      group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
      summarise(st.dev = sd(value)) %>%
      ungroup()


    p.sd <- ggplot(
      conv.data,
      aes(
        x = year, y = st.dev,
        group = interaction(full.model.name, scenario, variable, unit)
      )
    ) +
      facet_wrap(~ssp, ncol = 5) +
      mark_history() +
      geom_point(
        aes(
          colour = model,
          shape = target
        )
      ) +
      geom_line(aes(color = model)) +
      scale_color_manual(values = plot.model.colors) +
      theme_jsk() +
      labs(
        y = paste0("Std Deviation (", y.unit, ")"),
        x = NULL,
        title = paste("Convergence of", v),
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(colour = guide_legend(title = NULL))

    p.sd

    save_ggplot(
      p = p.sd,
      h = 130,
      w = 350,
      format = "png",
      f = file.path(path.figures.convergence.indicators, paste0("sd_perservice_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("sd_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))


    ### 2) Gini ----

    # Attach population data, to compute population-weighted gini
    pop.data <- scenarios %>% filter(
      (region == "World" | grepl(region, pattern = " (R10)", fixed = T)),
      variable == "Population"
    )

    conv.data.gini <- plot.data %>%
      left_join(pop.data, by = c("model", "scenario", "region", "year", "target", "ssp", "full.model.name")) %>%
      rename(
        variable = variable.x,
        unit = unit.x,
        value = value.x,
        population = value.y,
        pop.unit = unit.y
      ) %>%
      select(-variable.y)

    gini.pop.weighted.data <- conv.data.gini %>%
      drop_na(population, value) %>%
      group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
      reframe(gini.pop.weighted = weighted.gini(x = value, w = population)$bcwGini) %>%
      ungroup()

    # gini.data <- plot.data %>%
    #   group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
    #   reframe(gini = gini(value)$bcGini) %>%
    #   # N.B. We pick the bias-corrected Gini (bcGini), because the sample is very small (only 10 regions) and w/o bias correction the Gini estimate tends to be downward-biased (underestimation of inequality)
    #   ungroup()

    # Reorder ScenarioMIP scenarios
    gini.pop.weighted.data$target <- factor(gini.pop.weighted.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

    p.gini.w <- ggplot(
      gini.pop.weighted.data,
      aes(
        x = year, y = gini.pop.weighted,
        group = interaction(full.model.name, scenario, variable, unit)
      )
    ) +
      facet_wrap(~ssp, ncol = 5) +
      mark_history() +
      geom_point(
        aes(
          colour = model,
          shape = target
        )
      ) +
      geom_line(aes(color = model)) +
      scale_color_manual(values = plot.model.colors) +
      theme_jsk() +
      labs(
        y = paste0("Gini (pop weighted)"),
        x = NULL,
        title = paste("Convergence of", v),
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(colour = guide_legend(title = NULL))

    p.gini.w


    # p.gini <- ggplot(
    #   gini.data,
    #   aes(
    #     x = year, y = gini,
    #     group = interaction(full.model.name, scenario, variable, unit)
    #   )
    # ) +
    #   facet_wrap(~ssp) +
    #   mark_history() +
    #   geom_point(
    #     aes(
    #       colour = model,
    #       shape = target
    #     )
    #   ) +
    #   scale_color_manual(values = plot.model.colors) +
    #   theme_jsk() +
    #   labs(
    #     y = paste0("Gini"),
    #     x = NULL,
    #     title = paste("Convergence of", v),
    #     subtitle = "per capita",
    #     caption = paste0("File: ", IAM_SCENARIOS_FILE)
    #   ) +
    #   guides(colour = guide_legend(title = NULL))
    #
    # p.gini


    save_ggplot(
      p = p.gini.w,
      h = 130,
      w = 350,
      format = "png",
      f = file.path(path.figures.convergence.indicators, paste0("gini_perservice_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("gini_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))



    ### 3) Ratio ----

    # North America to Africa
    if (nrow(plot.data %>% filter(region == "North America (R10)")) > 0) {   # If data exists for "North America(R10)"

      ratio.nam.afr.data <- plot.data %>%
        filter(region %in% c("Africa (R10)", "North America (R10)")) %>%
        group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
        pivot_wider(names_from = region, values_from = value) %>%
        mutate(ratio = `North America (R10)` / `Africa (R10)`) %>%
        ungroup()

      # Reorder ScenarioMIP scenarios
      ratio.nam.afr.data$target <- factor(ratio.nam.afr.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

      p.ratio.nam.afr <- ggplot(
        ratio.nam.afr.data,
        aes(
          x = year, y = ratio,
          group = interaction(full.model.name, scenario, variable, unit)
        )
      ) +
        facet_wrap(~ssp, ncol=5) +
        mark_history() +
        geom_point(
          aes(
            colour = model,
            shape = target
          )
        ) +
        geom_line(aes(color = model)) +
        scale_color_manual(values = plot.model.colors) +
        theme_jsk() +
        labs(
          y = paste0("Ratio NAM/AFR"),
          x = NULL,
          title = paste("Convergence of", v),
          subtitle = paste0("per unit of service (", name.service, ")"),
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(colour = guide_legend(title = NULL))

      p.ratio.nam.afr


      save_ggplot(
        p = p.ratio.nam.afr,
        h = 130,
        w = 350,
        format = "png",
        f = file.path(path.figures.convergence.indicators, paste0("ratio_nam_afr_perservice_", clean_string(v), "_", clean_string(name.service)))
      )

      write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("ratio_nam_afr_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))


      # Pacific OECD to Africa

      ratio.pao.afr.data <- plot.data %>%
        filter(region %in% c("Africa (R10)", "Pacific OECD (R10)")) %>%
        group_by(model, scenario, variable, unit, year, target, ssp, full.model.name) %>%
        pivot_wider(names_from = region, values_from = value) %>%
        mutate(ratio = `Pacific OECD (R10)` / `Africa (R10)`) %>%
        ungroup()

      # Reorder ScenarioMIP scenarios
      ratio.pao.afr.data$target <- factor(ratio.pao.afr.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))

      p.ratio.pao.afr <- ggplot(
        ratio.pao.afr.data,
        aes(
          x = year, y = ratio,
          group = interaction(full.model.name, scenario, variable, unit)
        )
      ) +
        facet_wrap(~ssp, ncol = 5) +
        mark_history() +
        geom_point(
          aes(
            colour = model,
            shape = target
          )
        ) +
        geom_line(aes(color = model)) +
        scale_color_manual(values = plot.model.colors) +
        theme_jsk() +
        labs(
          y = paste0("Ratio PAO/AFR"),
          x = NULL,
          title = paste("Convergence of", v),
          subtitle = paste0("per unit of service (", name.service, ")"),
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(colour = guide_legend(title = NULL))

      p.ratio.pao.afr


      save_ggplot(
        p = p.ratio.pao.afr,
        h = 130,
        w = 350,
        format = "png",
        f = file.path(path.figures.convergence.indicators, paste0("ratio_pao_afr_perservice_", clean_string(v), "_", clean_string(name.service)))
      )

      write_csv(plot.data, file.path(path.figures.data.convergence.indicators, paste0("ratio_pao_afr_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))



    } else {
      print("No sufficient R10 data to calculate convergence as a ratio")
    }


    ### Three indicators together ----

    # Create individual indicator datasets with a common value column
    conv.data.ind <- conv.data %>%
      mutate(indicator = paste0("Std Dev (", y.unit, ")"), value = st.dev) %>%
      select(model, scenario, variable, unit, year, target, ssp, full.model.name, indicator, value)

    gini.data.ind <- gini.pop.weighted.data %>%
      mutate(indicator = "Gini (pop weighted)", value = gini.pop.weighted) %>%
      select(model, scenario, variable, unit, year, target, ssp, full.model.name, indicator, value)

    # Only create the ratio dataset if there is NAM data present.
    if (nrow(plot.data %>% filter(region == "North America (R10)")) > 0) {
      ratio.data.ind <- ratio.nam.afr.data %>%
        mutate(indicator = "Ratio NAM/AFR",
               value = ratio) %>%
        select(model, scenario, variable, unit, year, target, ssp, full.model.name, indicator, value)

      combined.data <- bind_rows(conv.data.ind, gini.data.ind, ratio.data.ind)
    } else {
      combined.data <- bind_rows(conv.data.ind, gini.data.ind)
    }

    # Reorder indicators
    combined.data$indicator <- factor(combined.data$indicator, levels = c(paste0("Std Dev (", y.unit, ")"), "Gini (pop weighted)", "Ratio NAM/AFR"))

    # Now create a plot with three rows (indicators) and five columns (ssp)
    p.combined <- ggplot(combined.data,
                         aes(x = year, y = value,
                             group = interaction(model, scenario, full.model.name, indicator))) +
      facet_grid(indicator ~ ssp, scales = "free_y", switch = "y") +
      geom_line(aes(color = model)) +
      geom_point(aes(color = model, shape = target)) +
      scale_color_manual(values = plot.model.colors) +
      scale_y_continuous(position = "left") +  # ensures axis tick labels are on the left
      labs(
        x = "Year",
        y = NULL,
        title = paste("Convergence of", v),
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      theme_jsk() +
      guides(
        color = guide_legend(title = "Model"),
        shape = guide_legend(title = "Target")
      ) +
      theme(strip.placement = "outside",           # moves facet strips outside the panels
            strip.text.y.left = element_text(angle = 90))  # rotates the text as needed

    p.combined


    save_ggplot(
      p = p.combined,
      h = 180,
      w = 300,
      format = "png",
      f = file.path(path.figures.convergence.indicators, paste0("conv_indicators_combined_perservice_", clean_string(v), "_", clean_string(name.service)))
    )



  } else {
    print(paste0("Something went wrong in calculating per capita data for ", v))
  }
} else {
  print(paste0("No data reported for ", v))
}



### GN vs GS ----


# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)


plot.data <- scenarios.with.aggregations %>% filter(variable %in% c(
  v,
  v.service
))

# Aggregate to GN and GS
plot.data <- create_globalnorth_globalsouth(df = plot.data, china.in.GN = FALSE)


# change to per service units (m2 or pkm)
if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # only if you have data for variable v...

  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  # order data by scenarioMIP target scenario and by region
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
  plot.data$region <- factor(plot.data$region, levels = c("Global South", "China+ (R10)", "Global North", "World"))


  # plot
  if (nrow(plot.data %>% filter(variable != v.service)) > 0) { # again, only if you have data for variable v...

    print(paste0("Plotting ", v, " per unit of ", v.service))

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()

    ### Faceted by region

    p <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(region ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )
    p

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("gn_gs_perservice_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.fe.per.service, paste0("gn_gs_perservice_", clean_string(v), "_", clean_string(name.service), ".csv")))


    #### Convergence plots

    # Remove models with no data
    plot.data <- plot.data %>% drop_na(value)

    p.convergence <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(model ~ target, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = region,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.region.colours.gn.gs.chn) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )
    p.convergence

    save_ggplot(
      p = p.convergence,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("gn_gs_perservice_conv_", clean_string(v), "_", clean_string(name.service)))
    )

    ### Convergence, by SSP
    p.convergence.ssp <- ggplot(
      plot.data,
      aes(
        x = year, y = value,
        group = interaction(full.model.name, scenario, region, variable, unit)
      )
    ) +
      facet_grid(model ~ ssp, scales = "free_y") +
      mark_history() +
      geom_line(
        aes(
          colour = region,
          linetype = target
        )
      ) +
      scale_color_manual(values = plot.region.colours.gn.gs.chn) +
      scale_linetype_manual(values = plot.target.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = NULL,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ")"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )
    p.convergence.ssp

    save_ggplot(
      p = p.convergence.ssp,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("gn_gs_perservice_conv_ssp_", clean_string(v), "_", clean_string(name.service)))
    )
  } else {
    print(paste0("Something went wrong in calculating per capita data for ", v))
  }
} else {
  print(paste0("No data reported for ", v))
}







## b) vs GDPpc ----


### Global ----

plot.data.gdp.percap <- scenarios %>%
  filter(
    region == "World",
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)

plot.data <- scenarios.with.aggregations %>% filter(
  region == "World",
  variable %in% c(
    v,
    v.service
  )
)


# change to per service units
if (nrow(plot.data %>% filter(variable != v.service)) > 0) {
  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  y.unit <- plot.data %>%
    pull(unit) %>%
    unique()

  plot.data <- plot.data %>%
    bind_rows(plot.data.gdp.percap) %>%
    select(-unit) %>%
    pivot_wider(names_from = variable, values_from = value)

  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


  # plot
  if (nrow(plot.data) > 0) {
    print(paste0("Plotting ", v, " (over GDP/cap)"))

    p <- ggplot(
      plot.data,
      aes(
        x = `GDP|PPP`, y = .data[[v]],
        group = interaction(full.model.name, scenario, region)
      )
    ) +
      facet_wrap(~target, ncol = 3) +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = gdp.percap.unit,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ") - vs GDP per capita"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("world_perservice_overgdp_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.fe.per.service, paste0("world_perservice_overgdp_", clean_string(v), "_", clean_string(name.service), ".csv")))
  } else {
    print(paste0("Something went wrong in calculating per capita data for ", v))
  }
} else {
  print(paste0("No data repoted for ", v))
}


### R5 ----

plot.data.gdp.percap <- scenarios %>%
  filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


# Clean service name:
name.service <- v.service %>%
  gsub("Energy Service\\|", "", .) %>%
  gsub("Residential and Commercial\\|", "", .) %>%
  gsub("Transportation\\|", "", .) %>%
  gsub("Non-Metallic Minerals\\|", "", .)

plot.data <- scenarios.with.aggregations %>% filter(
  (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
  variable %in% c(
    v,
    v.service
  )
)


# change to per capita units
if (nrow(plot.data %>% filter(variable != v.service)) > 0) {
  y.unit <- plot.data %>%
    filter(variable != v.service) %>%
    pull(unit) %>%
    unique()
  service.unit <- plot.data %>%
    filter(variable != v) %>%
    pull(unit) %>%
    unique()

  if (!is.na(y.unit)) {
    plot.data <- to_per_unit_service_scenariomip(df = plot.data, var.service = v.service)
  } else {
    print("Unit error.")
  }

  y.unit <- plot.data %>%
    pull(unit) %>%
    unique()

  plot.data <- plot.data %>%
    bind_rows(plot.data.gdp.percap) %>%
    select(-unit) %>%
    pivot_wider(names_from = variable, values_from = value)

  # order data by scenarioMIP target scenario and by region
  plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
  plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


  # plot
  if (nrow(plot.data) > 0) {
    print(paste0("Plotting ", v, " (over GDP/cap)"))

    p <- ggplot(
      plot.data,
      aes(
        x = `GDP|PPP`, y = .data[[v]],
        group = interaction(full.model.name, scenario, region)
      )
    ) +
      facet_grid(region ~ target, scales = "free_y") +
      geom_line(
        aes(
          colour = model,
          linetype = ssp
        )
      ) +
      scale_color_manual(values = plot.model.colors) +
      scale_linetype_manual(values = plot.ssp.linetypes) +
      theme_jsk() +
      theme(
        strip.text.y = element_text(angle = 0)
      ) +
      labs(
        y = y.unit,
        x = gdp.percap.unit,
        title = v,
        subtitle = paste0("per unit of service (", name.service, ") - vs GDP per capita"),
        caption = paste0("File: ", IAM_SCENARIOS_FILE)
      ) +
      guides(
        colour = guide_legend(title = NULL),
        linetype = guide_legend(title = NULL)
      )

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures.fe.per.service, paste0("r5_perservice_overgdp_", clean_string(v), "_", clean_string(name.service)))
    )

    write_csv(plot.data, file.path(path.figures.data.fe.per.service, paste0("r5_perservice_overgdp_", clean_string(v), "_", clean_string(name.service), ".csv")))
  } else {
    print(paste0("Something went wrong in calculating per capita data for ", v))
  }
} else {
  print(paste0("No data repoted for ", v))
}






# ---- Final Energy intensity per unit of GDP ----

path.figures.fe.intensity <- file.path(path.figures, "FE intensity")
path.figures.data.fe.intensity <- file.path(path.figures.data, "FE intensity")

if (!dir.exists(path.figures.data.fe.intensity)) {
  dir.create(path.figures.data.fe.intensity, recursive = TRUE)
}

to_per_unit_gdp_scenariomip <- function(df) {
  # formatting
  df <- df %>%
    rename(simple.model = model, model = full.model.name)

  # Extract current unit of service (e.g. "bn m2")
  gdp.unit <- df %>%
    filter(variable == "GDP|PPP") %>%
    pull(unit) %>%
    unique()

  # Correct the unit of the gdp
  df <- df %>%
    mutate(value = if_else(variable == "GDP|PPP", value * billion, value)) %>%
    mutate(unit = if_else(variable == "GDP|PPP", "USD_2010/yr", unit))

  formatted.gdp.unit <- df %>%
    filter(variable == "GDP|PPP") %>%
    pull(unit) %>%
    unique()

  df.gdp <- df %>%
    filter(variable == "GDP|PPP") %>%
    rename(gdp.value = value) %>%
    select(model, scenario, region, year, gdp.value)

  # Calculate variable per unit of service
  df <- df %>%
    filter(variable != "GDP|PPP") %>%
    left_join(df.gdp) %>%
    mutate(value = value / gdp.value) %>%
    mutate(unit = paste0(unit, "/", formatted.gdp.unit)) %>%
    select(-gdp.value)

  unit.per.gdp <- unique(df$unit)


  # correct the units
  if (unit.per.gdp == "EJ/yr/USD_2010/yr") {
    df <- df %>%
      mutate(
        value = value * exa / mega,
        unit = "MJ/USD_2010"
      )
  } else {
    print(paste0("The unit for variable ", v, " is not processed to per gdp variables in this script."))
    df <- df %>%
      mutate(
        value = value,
        unit = paste0(unit, " [per unit service]")
      )
  }

  # formatting
  df <- df %>%
    rename(model = simple.model, full.model.name = model)

  return(df)
}


## choose variable to be calculated per unit of service: ----

vars.fe.intensity <- c(
  "Final Energy|Residential and Commercial",
  "Final Energy|Residential and Commercial|Electricity",
  "Final Energy|Transportation",
  "Final Energy|Industry",
  "Final Energy|Electricity"
)

#' Final Energy variables to be calculated per unit of service:
#' "Final Energy|Residential and Commercial",
#' "Final Energy|Residential and Commercial|Electricity",
#' "Final Energy|Transportation",
#' "Final Energy|Industry"
#'
#' Energy service variables:
#' Main ones:
#' "Energy Service|Residential and Commercial|Floor Space"
#' "Energy Service|Transportation|Passenger"
#' "Energy Service|Transportation|Freight"
#'
#' "Production|Iron and Steel" (= "Production|Iron and Steel|Iron" + "Production|Iron and Steel|Steel")
#' "Production|Non-Metallic Minerals|Cement"

v <- "Final Energy|Residential and Commercial"
# v.service = "Production|Non-Metallic Minerals|Cement"

## a) vs time ----

### Global ----

for (v in vars.fe.intensity) {
  plot.data <- scenarios.with.aggregations %>% filter(
    region == "World",
    variable %in% c(
      v,
      "GDP|PPP"
    )
  )

  # change to per service units (m2 or pkm)
  if (nrow(plot.data %>% filter(variable != "GDP|PPP")) > 0) { # only if you have data for variable v...

    y.unit <- plot.data %>%
      filter(variable != "GDP|PPP") %>%
      pull(unit) %>%
      unique()
    gdp.unit <- plot.data %>%
      filter(variable == "GDP|PPP") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_unit_gdp_scenariomip(df = plot.data)
    } else {
      print("Unit error.")
    }

    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


    # plot
    if (nrow(plot.data %>% filter(variable != "GDP|PPP")) > 0) { # again, only if you have data for variable v...

      print(paste0("Plotting ", v, " per unit of ", "GDP|PPP"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()

      p <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_wrap(~target, ncol = 3) +
        mark_history() +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per unit of GDP|PPP",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.fe.intensity, paste0("world_intensity_pergdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.fe.intensity, paste0("world_intensity_pergdp_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per unit of service data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of loop over vars.fe.intensity


### R5 ----

for (v in vars.fe.intensity) {
  plot.data <- scenarios.with.aggregations %>% filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      v,
      "GDP|PPP"
    )
  )

  # change to per service units (m2 or pkm)
  if (nrow(plot.data %>% filter(variable != "GDP|PPP")) > 0) { # only if you have data for variable v...

    y.unit <- plot.data %>%
      filter(variable != "GDP|PPP") %>%
      pull(unit) %>%
      unique()
    gdp.unit <- plot.data %>%
      filter(variable == "GDP|PPP") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_unit_gdp_scenariomip(df = plot.data)
    } else {
      print("Unit error.")
    }

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


    # plot
    if (nrow(plot.data %>% filter(variable != "GDP|PPP")) > 0) { # again, only if you have data for variable v...

      print(paste0("Plotting ", v, " per unit of ", "GDP|PPP"))

      y.unit <- plot.data %>%
        pull(unit) %>%
        unique()

      p <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per unit of GDP|PPP",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )


      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.fe.intensity, paste0("r5_intensity_pergdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.fe.intensity, paste0("r5_intensity_pergdp_", clean_string(v), ".csv")))


      #### Convergence plots ----

      # Remove models with no data
      plot.data <- plot.data %>% drop_na(value)

      p.convergence <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ target, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.region.colours.r5) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per unit of GDP|PPP",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )


      save_ggplot(
        p = p.convergence,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.fe.intensity, paste0("r5_intensity_pergdp_conv_", clean_string(v)))
      )

      # Faceting by SSP
      p.convergence.ssp <- ggplot(
        plot.data,
        aes(
          x = year, y = value,
          group = interaction(full.model.name, scenario, region, variable, unit)
        )
      ) +
        facet_grid(model ~ ssp, scales = "free_y") +
        mark_history() +
        geom_line(
          aes(
            colour = region,
            linetype = target
          )
        ) +
        scale_color_manual(values = plot.region.colours.r5) +
        scale_linetype_manual(values = plot.target.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = NULL,
          title = v,
          subtitle = "per unit of GDP|PPP",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )


      save_ggplot(
        p = p.convergence.ssp,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.fe.intensity, paste0("r5_intensity_pergdp_conv_ssp_", clean_string(v)))
      )
    } else {
      print(paste0("Something went wrong in calculating per unit of service data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of loop over vars.fe.intensity




## b) vs GDPpc ----


### Global ----

plot.data.gdp.percap <- scenarios %>%
  filter(
    region == "World",
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


for (v in vars.fe.intensity) {
  plot.data <- scenarios.with.aggregations %>% filter(
    region == "World",
    variable %in% c(
      v,
      "GDP|PPP"
    )
  )

  # change to per service units (m2 or pkm)
  if (nrow(plot.data %>% filter(variable != "GDP|PPP")) > 0) { # only if you have data for variable v...

    y.unit <- plot.data %>%
      filter(variable != "GDP|PPP") %>%
      pull(unit) %>%
      unique()
    gdp.unit <- plot.data %>%
      filter(variable == "GDP|PPP") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_unit_gdp_scenariomip(df = plot.data)
    } else {
      print("Unit error.")
    }

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)

    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))


    # plot
    if (nrow(plot.data) > 0) {
      print(paste0("Plotting ", v, " (over GDP/cap)"))

      p <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_wrap(~target, ncol = 3) +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = gdp.percap.unit,
          title = v,
          subtitle = paste0("per unit of GDP|PPP - vs GDP per capita"),
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.fe.intensity, paste0("world_fe_pergdp_overgdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.fe.intensity, paste0("world_fe_pergdp_overgdp_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per unit of service data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of loop over vars.fe.intensity




### R5 ----

plot.data.gdp.percap <- scenarios %>%
  filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      "Population",
      "GDP|PPP" # note; also possible to use GDP|MER
    )
  ) %>%
  to_per_capita_scenariomip(y.u = "billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>%
  pull(unit) %>%
  unique()


for (v in vars.fe.intensity) {
  plot.data <- scenarios.with.aggregations %>% filter(
    (region == "World" | grepl(region, pattern = " (R5)", fixed = T)),
    variable %in% c(
      v,
      "GDP|PPP"
    )
  )

  # change to per service units (m2 or pkm)
  if (nrow(plot.data %>% filter(variable != "GDP|PPP")) > 0) { # only if you have data for variable v...

    y.unit <- plot.data %>%
      filter(variable != "GDP|PPP") %>%
      pull(unit) %>%
      unique()
    gdp.unit <- plot.data %>%
      filter(variable == "GDP|PPP") %>%
      pull(unit) %>%
      unique()

    if (!is.na(y.unit)) {
      plot.data <- to_per_unit_gdp_scenariomip(df = plot.data)
    } else {
      print("Unit error.")
    }

    y.unit <- plot.data %>%
      pull(unit) %>%
      unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)

    # order data by scenarioMIP target scenario and by region
    plot.data$target <- factor(plot.data$target, levels = c("VLLO", "VLHO", "L", "ML", "M", "H"))
    plot.data$region <- factor(plot.data$region, levels = c("Middle East & Africa (R5)", "Latin America (R5)", "Asia (R5)", "Reforming Economies (R5)", "OECD & EU (R5)", "World"))


    # plot
    if (nrow(plot.data) > 0) {
      print(paste0("Plotting ", v, " (over GDP/cap)"))

      p <- ggplot(
        plot.data,
        aes(
          x = `GDP|PPP`, y = .data[[v]],
          group = interaction(full.model.name, scenario, region)
        )
      ) +
        facet_grid(region ~ target, scales = "free_y") +
        geom_line(
          aes(
            colour = model,
            linetype = ssp
          )
        ) +
        scale_color_manual(values = plot.model.colors) +
        scale_linetype_manual(values = plot.ssp.linetypes) +
        theme_jsk() +
        theme(
          strip.text.y = element_text(angle = 0)
        ) +
        labs(
          y = y.unit,
          x = gdp.percap.unit,
          title = v,
          subtitle = paste0("per unit of GDP|PPP - vs GDP per capita"),
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(
          colour = guide_legend(title = NULL),
          linetype = guide_legend(title = NULL)
        )

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures.fe.intensity, paste0("r5_fe_pergdp_overgdp_", clean_string(v)))
      )

      write_csv(plot.data, file.path(path.figures.data.fe.intensity, paste0("r5_fe_pergdp_overgdp_", clean_string(v), ".csv")))
    } else {
      print(paste0("Something went wrong in calculating per unit of service data for ", v))
    }
  } else {
    print(paste0("No data reported for ", v))
  }
} # end of loop over vars.fe.intensity
