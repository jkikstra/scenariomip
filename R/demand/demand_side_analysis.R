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
             "Final Energy|Non-Energy Use")
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
IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-03-05.csv"

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
  simplify_model_names(keep.full.model.name = T) %>%
  # temporary fixes (which should be reported to the modelling teams)
  remove_scenarios_with_issues() %>%
  remove_all_zero_values()

# save some useful lists
model.list <- scenarios %>% pull(model) %>% unique()
model.list.simple <- scenarios %>% distinct(model) %>% simplify_model_names() %>% pull(model) %>% unique()



# Loading other data ----
# ...


# totals (all direct variables) ----
## Global ----
for (v in vars.all){
  plot.data <- scenarios %>% filter(region == "World",
                                    variable == v)

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
        title = v
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
        title = v
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



## per capita (all direct variables) ----
# TODO. Note: need to think about how to handle units. population in millions, while service is in different units.

## Example figure: total over time ----
p <- ggplot(scenarios %>% filter(region == "World",
                                 variable == "Energy Service|Transportation|Passenger"),
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
    y = "billion pkm/yr"
  ) +
  guides(colour = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))
p

save_ggplot(
  p = p,
  h = 200,
  w = 300,
  f = file.path(path.figures, "transport_passenger_total")
)
## Example figure: per capita over time ----
p <- ggplot(scenarios %>% filter(region == "World",
                                 variable %in% c("Energy Service|Transportation|Passenger",
                                                 "Population")) %>%
              to_per_capita() %>% mutate(unit = "thousand pkm/yr per capita"),
            aes(x = year, y = value * 1e3,
                group = interaction(full.model.name,scenario,region,variable,unit))) +
  facet_wrap(~target, ncol = 3) +
  mark_history() +
  geom_line(
    aes(colour = model)
  ) +
  scale_color_manual(values = plot.model.colors) +
  theme_jsk() +
  labs(
    y = "pkm/yr per capita"
  ) +
  guides(colour = guide_legend(title = NULL))
p
save_ggplot(
  p = p,
  h = 200,
  w = 300,
  f = file.path(path.figures, "transport_passenger_percap")
)


# Regional ----
## Example figure: total over time ----
p <- ggplot(scenarios %>% filter((region == "World" | grepl(region, pattern="R10", fixed=T)),
                                 !(scenario=="SSP2 - Medium Emissions_a" & model=="GCAM"), # reporting error; likely unit issue
                                 variable == "Energy Service|Transportation|Passenger"),
            aes(x = year, y = value,
                group = interaction(full.model.name,scenario,region,variable,unit))) +
  facet_grid(region~target, scales="free_y") +
  mark_history() +
  geom_line(
    aes(colour = model)
  ) +
  scale_color_manual(values = plot.model.colors) +
  theme_jsk() +
  labs(
    y = "billion pkm/yr"
  ) +
  guides(colour = guide_legend(title = NULL))
p

save_ggplot(
  p = p,
  h = 200,
  w = 300,
  f = file.path(path.figures, "transport_passenger_total_regional")
)
