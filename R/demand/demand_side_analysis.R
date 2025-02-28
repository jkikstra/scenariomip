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


# Helper functions ----
# ...

# Variable selection ----

## Survey list
vars.survey <- vroom(here("data", "demand", "Demand_Variable_Survey.csv"), skip = 7) %>%
  pull(Variable)

## Misc
vars.socioecon <- c("Population", "GDP|MER", "GDP|PPP")

## Full list of variables
vars.all <- c(
  vars.survey,
  vars.socioecon
)


# Loading IAM data ----
IAM_SCENARIOS_LOCATION <- here("data", "data_vetting", "scens")
IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-02-17.csv"

scenarios.alldata <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, IAM_SCENARIOS_FILE), mode="fast")

# only keep relevant variables
scenarios <- scenarios.alldata %>%
  filter(Variable %in% vars.all)

# to long format
scenarios <- scenarios %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  filter(year<=2100)

# add some extra helpful information
scenarios <- scenarios %>%
  add_scenariomip_targets_to_IAM_scenarios() %>%
  add_ssp_basis_to_IAM_scenarios() %>%
  simplify_model_names(keep.full.model.name = T)

# save some useful lists
model.list <- scenarios %>% pull(model) %>% unique()
model.list.simple <- scenarios %>% distinct(model) %>% simplify_model_names() %>% pull(model) %>% unique()



# Loading other data ----
# ...


# Global ----
## Example figure: total over time ----
p <- ggplot(scenarios %>% filter(region == "World",
                                 !(scenario=="SSP2 - Medium Emissions_a" & model=="GCAM"), # reporting error; likely unit issue
                                 variable == "Energy Service|Transportation|Passenger"),
            aes(x = year, y = value,
                group = interaction(full.model.name,scenario,region,variable,unit))) +
  facet_wrap(~target, ncol = 3) +
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

## Example figure: per capita over time ----
p <- ggplot(scenarios %>% filter(region == "World",
                                 !(scenario=="SSP2 - Medium Emissions_a" & model=="GCAM"), # reporting error; likely unit issue
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
