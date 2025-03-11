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

here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Notes ----
# The list of variables is here: https://docs.google.com/spreadsheets/d/1H65lrzcBBBoUYKc4rjt1sJdbdLTf7YMRM-n2k325FDU/edit?usp=sharing
# (not sure whether Tommaso has access already, or whether this is an open drive) This list was originally used to inquire with the teams whether they can extend their reporting, so this also gives an impression of what would be available from the models.
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

# visualisation choices ----
STARTYEAR <- 2025

plot.model.colors <- c(
  "AIM" = "#4D5CAD",
  "COFFEE" = "#69BA7F",
  "GCAM" = "#759EA8",
  "IMAGE" = "#868367",
  "MESSAGE" = "#892F71",
  "REMIND" = "#facb1e",
  "WITCH" = "#fb6a4a")

plot.ssp.linetypes <- c(
  "SSP1" = "3313",
  "SSP2" = "solid",
  "SSP3" = "3333",
  "SSP4" = "1111",
  "SSP5" = "3131"
)
# plot.ssp.shapes


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

vars.fe <- c("Final Energy",
             "Final Energy|Industry",
             "Final Energy|Residential and Commercial",
             "Final Energy|Transportation",
             "Final Energy|Non-Energy Use",

             "Final Energy|Electricity",
             "Final Energy|Transportation|Electricity",
             "Final Energy|Industry|Electricity",
             "Final Energy|Residential and Commercial|Electricity")
# to calculate:
# share of electricity in final energy (per sector)

## Full list of variables
vars.all <- c(
  vars.survey,
  vars.socioecon,
  vars.fe
)


# Loading IAM data ----
IAM_SCENARIOS_LOCATION <- here("data", "data_vetting", "scens")
# IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-02-17.csv"
# IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-03-05.csv" # version 'demand_world_r5_total_directvariables_v20250307_a.zip'
IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-03-11.csv" # version 'x.zip'

scenarios.alldata <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, IAM_SCENARIOS_FILE), mode="fast")

# only keep relevant variables
scenarios <- scenarios.alldata %>%
  filter(Variable %in% vars.all)
rm(scenarios.alldata) # remove large dataframe from environment to free up RAM
gc()

# to long format
scenarios <- scenarios %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  filter(year<=2100)

# add some extra helpful information
scenarios <- scenarios %>%
  add_scenariomip_targets_to_IAM_scenarios() %>%
  add_ssp_basis_to_IAM_scenarios() %>%
  remove_scenarios_with_issues() %>% # temporary fix (which should be reported to the modelling teams)
  simplify_model_names(keep.full.model.name = T) %>%
  remove_all_zero_values() # temporary fix (which should be reported to the modelling teams)

# save some useful lists
model.list <- scenarios %>% pull(model) %>% unique()
model.list.simple <- scenarios %>% distinct(model) %>% simplify_model_names() %>% pull(model) %>% unique()



# Loading other data ----
# ...

# ... ... ... ... ... ... ... ... ... ------------------------------------------
# DIRECT VARIABLES -------------------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------

# totals (all direct variables) ----
## Global ----
for (v in vars.all){
  plot.data <- scenarios %>% filter(region == "World",
                                    variable == v)

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))

  if (nrow(plot.data)>0){
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>% pull(unit) %>% unique()
    p <- ggplot(plot.data,
                aes(x = year, y = value,
                    group = interaction(full.model.name,scenario,region,variable,unit))) +
      facet_wrap(~target, ncol = 3) +
      mark_history() +
      geom_line(
        aes(colour = model,
            linetype = ssp)
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
      guides(colour = guide_legend(title = NULL),
             linetype = guide_legend(title = NULL))

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures, paste0("world_total_", clean_string(v)))
    )
  } else {
    print( paste0("No data repoted for ", v) )
  }

}
## R5 ----
for (v in vars.all){
  plot.data <- scenarios %>% filter((region == "World" | grepl(region, pattern=" (R5)", fixed=T)),
                                    variable == v)

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))


  if (nrow(plot.data)>0){
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>% pull(unit) %>% unique()
    p <- ggplot(plot.data,
                aes(x = year, y = value,
                    group = interaction(full.model.name,scenario,region,variable,unit))) +
      facet_grid(region~target, scales="free_y") +
      mark_history() +
      geom_line(
        aes(colour = model,
            linetype = ssp)
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
      guides(colour = guide_legend(title = NULL),
             linetype = guide_legend(title = NULL))

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures, paste0("r5_total_", clean_string(v)))
    )
  } else {
    print( paste0("No data repoted for ", v) )
  }

}


# ... ... ... ... ... ... ... ... ... ------------------------------------------
# DIRECT VARIABLES (Per Capita) ------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------


to_per_capita_scenariomip <- function(df, y.u, p.u=pop.unit){

  # formatting
  df <- df %>%
    rename(simple.model=model,model=full.model.name)

  # to per capita
  df <- df %>% to_per_capita() %>%
    mutate(value = value / p.u)

  # correct the units
  if (y.u == "EJ/yr"){
    df <- df %>%
      mutate(value = value * exa / giga,
             unit = "GJ/cap/yr")
  } else if (y.u == "billion pkm/yr"){
    df <- df %>%
      mutate(value = value * billion,
             unit = "pkm/cap/yr")
  } else if (y.u == "billion tkm/yr"){
    df <- df %>%
      mutate(value = value * billion,
             unit = "tkm/cap/yr")
  } else if (y.u == "Mt/yr"){
    df <- df %>%
      mutate(value = value * mega,
             unit = "t/cap/yr")
  } else if (y.u == "bn m2"){ # change in common-definitions to 'billion m2'?
    df <- df %>%
      mutate(value = value * billion,
             unit = "m2/cap")
  } else if (y.u == "million"){
    df <- df %>%
      mutate(value = value * million,
             unit = "unit/cap")
  } else if (y.u == "°C-days"){ # change in common-definitions to '°C-days/yr' -> average per capita already?
    df <- df %>%
      mutate(value = value,
             unit = "°C-days/cap/yr")
  } else if (y.u == "billion USD_2010/yr"){
    df <- df %>%
      mutate(value = value * billion,
             unit = "USD_2010/cap/yr")
  } else {
    print(paste0("The unit for variable ", v, " is not processed to per capita variables in this script."))
    df <- df %>%
      mutate(value = value,
             unit = paste0(unit, " [per capita]") )
  }

  # formatting
  df <- df %>%
    rename(model=simple.model,full.model.name=model)

  return(df)
}


## Global ----
for (v in vars.all){
  plot.data <- scenarios %>% filter(region == "World",
                                    variable %in% c(v,
                                                    "Population"))
  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))

  # change to per capita units
  if (nrow(plot.data%>%filter(variable!="Population"))>0){
    y.unit <- plot.data %>% filter(variable!="Population") %>% pull(unit) %>% unique()

    if(!is.na(y.unit)){
      plot.data <- to_per_capita_scenariomip(df=plot.data, y.u=y.unit)
    } else {
      print("Unit error.")
    }



    # plot
    if (nrow(plot.data%>%filter(variable!="Population"))>0){
      print(paste0("Plotting ", v, " (per capita)"))

      y.unit <- plot.data %>% pull(unit) %>% unique()
      p <- ggplot(plot.data,
                  aes(x = year, y = value,
                      group = interaction(full.model.name,scenario,region,variable,unit))) +
        facet_wrap(~target, ncol = 3) +
        mark_history() +
        geom_line(
          aes(colour = model,
              linetype = ssp)
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
        guides(colour = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL))

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures, paste0("world_percapita_", clean_string(v)))
      )
    } else {
      print( paste0("Something went wrong in calculating per capita data for ", v) )
    }


  } else {
    print( paste0("No data repoted for ", v) )
  }



}

## R5 ----
for (v in vars.all){
  plot.data <- scenarios %>% filter((region == "World" | grepl(region, pattern=" (R5)", fixed=T)),
                                    variable %in% c(v,
                                                    "Population"))

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))


  # change to per capita units
  if (nrow(plot.data%>%filter(variable!="Population"))>0){
    y.unit <- plot.data %>% filter(variable!="Population") %>% pull(unit) %>% unique()

    if(!is.na(y.unit)){
      plot.data <- to_per_capita_scenariomip(df=plot.data, y.u=y.unit)
    } else {
      print("Unit error.")
    }



    # plot
    if (nrow(plot.data%>%filter(variable!="Population"))>0){
      print(paste0("Plotting ", v, " (per capita)"))

      y.unit <- plot.data %>% pull(unit) %>% unique()
      p <- ggplot(plot.data,
                  aes(x = year, y = value,
                      group = interaction(full.model.name,scenario,region,variable,unit))) +
        facet_grid(region~target, scales="free_y") +
        mark_history() +
        geom_line(
          aes(colour = model,
              linetype = ssp)
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
        guides(colour = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL))

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures, paste0("r5_percapita_", clean_string(v)))
      )
    } else {
      print( paste0("Something went wrong in calculating per capita data for ", v) )
    }


  } else {
    print( paste0("No data repoted for ", v) )
  }



}




# ... ... ... ... ... ... ... ... ... ------------------------------------------
# CALCULATED/DERIVED VARIABLES -------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------

# Electrification ----
calculate_electrification <- function(df){

  df %>% filter(variable %in% c(
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
      grepl(variable, pattern="Electricity", fixed=T),
      "Electricity",
      "Total")
    ) %>%
    remove_variable_lastlevel_match("Electricity") %>%
    pivot_wider(names_from = total.or.electricity, values_from = value) %>%
    mutate(`Electrification rate` = Electricity / Total * 100) %>%
    mutate_cond(variable=="Final Energy", variable = "Final Energy|Total") %>%
    remove_variable_firstlevel_match("Final Energy") %>%
    mutate(variable = paste0("Electrification of Final Energy (", variable, ")"),
           unit = "%") %>%
    rename(value = `Electrification rate`) %>%

    return()

}

scenarios.electrification <- scenarios %>% calculate_electrification()

vars.electrification <- scenarios.electrification %>%
  pull(variable) %>% unique()

## Global ----
for (v in vars.electrification){
  plot.data <- scenarios.electrification %>% filter(region == "World",
                                    variable == v)

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))


  if (nrow(plot.data)>0){
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>% pull(unit) %>% unique()
    p <- ggplot(plot.data,
                aes(x = year, y = value,
                    group = interaction(full.model.name,scenario,region,variable,unit))) +
      facet_wrap(~target, ncol = 3) +
      mark_history() +
      geom_line(
        aes(colour = model,
            linetype = ssp)
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
      guides(colour = guide_legend(title = NULL),
             linetype = guide_legend(title = NULL))

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures, paste0("world_electrification_", clean_string(v)))
    )
  } else {
    print( paste0("No data repoted for ", v) )
  }

}
## R5 ----
for (v in vars.electrification){
  plot.data <- scenarios.electrification %>% filter((region == "World" | grepl(region, pattern=" (R5)", fixed=T)),
                                    variable == v)

  if (nrow(plot.data)>0){
    print(paste0("Plotting ", v))

    y.unit <- plot.data %>% pull(unit) %>% unique()
    p <- ggplot(plot.data,
                aes(x = year, y = value,
                    group = interaction(full.model.name,scenario,region,variable,unit))) +
      facet_grid(region~target, scales="free_y") +
      mark_history() +
      geom_line(
        aes(colour = model,
            linetype = ssp)
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
      guides(colour = guide_legend(title = NULL),
             linetype = guide_legend(title = NULL))

    save_ggplot(
      p = p,
      h = 200,
      w = 300,
      format = "png",
      f = file.path(path.figures, paste0("r5_electrification_", clean_string(v)))
    )
  } else {
    print( paste0("No data repoted for ", v) )
  }

}



# Per GDP on x-axis [per/cap service over per/cap GDP]   ----
# ... use `to_per_gdp()`
# example: pkm/$
gdp.unit <- billion

## R5 ----
plot.data.gdp.percap <- scenarios %>%
  filter((region == "World" | grepl(region, pattern=" (R5)", fixed=T)),
         variable %in% c("Population",
                         "GDP|PPP" # note; also possible to use GDP|MER
                         )) %>%
  to_per_capita_scenariomip(y.u="billion USD_2010/yr")
gdp.percap.unit <- plot.data.gdp.percap %>% pull(unit) %>% unique()

for (v in c("Energy Service|Residential|Floor Space",
            "Energy Service|Transportation|Freight",
            "Energy Service|Transportation|Passenger",
            "Stocks|Transportation|Light-Duty Vehicle",
            "Stocks|Transportation|Light-Duty Vehicle|Battery-Electric",
            vars.electrification)){
  plot.data <- scenarios %>% bind_rows(scenarios.electrification %>% select(-c(Total,Electricity))) %>% filter((region == "World" | grepl(region, pattern=" (R5)", fixed=T)),
                                    variable %in% c(v,
                                                    "Population"
                                                    ))

  # order data by scenarioMIP target scenario
  plot.data$target <- factor(plot.data$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))



  # change to per capita units
  if (nrow(plot.data%>%filter(variable!="Population"))>0){
    y.unit <- plot.data %>% filter(variable!="Population") %>% pull(unit) %>% unique()

    if (v %nin% vars.electrification){
      if(!is.na(y.unit)){
        plot.data <- to_per_capita_scenariomip(df=plot.data, y.u=y.unit)
      } else {
        print("Unit error.")
      }
    }
    y.unit.percapita <- plot.data %>% filter(variable!="Population") %>% pull(unit) %>% unique()

    plot.data <- plot.data %>%
      bind_rows(plot.data.gdp.percap) %>%
      select(-unit) %>%
      pivot_wider(names_from = variable, values_from = value)



    # plot
    if (nrow(plot.data)>0){
      print(paste0("Plotting ", v, " (over GDP/cap)"))

      p <- ggplot(plot.data,
                  aes(x = `GDP|PPP`, y = .data[[v]],
                      group = interaction(full.model.name,scenario,region))) +
        facet_grid(region~target, scales="free_y") +
        geom_line(
          aes(colour = model,
              linetype = ssp)
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
          subtitle = "per capita",
          caption = paste0("File: ", IAM_SCENARIOS_FILE)
        ) +
        guides(colour = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL))

      save_ggplot(
        p = p,
        h = 200,
        w = 300,
        format = "png",
        f = file.path(path.figures, paste0("r5_overgdp_", clean_string(v)))
      )
    } else {
      print( paste0("Something went wrong in calculating per capita data for ", v) )
    }


  } else {
    print( paste0("No data repoted for ", v) )
  }



}




# Normalised to 2025 ---
# ... use `normalise_iamc_long(starting.year = STARTYEAR)`

