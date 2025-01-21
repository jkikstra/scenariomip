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

# CONFIG -----------------------------------------------------------------------
STARTYEAR <- 2025

# Select folder for scenario data ----------------------------------------------
SCENARIO.FILES.FOLDER <- here("data")

# Scenario input data ----------------------------------------------------------
SCENARIO.INPUT.BATCH.FILES.DATASTRING <- "20241122"

## List all batch files --------------------------------------------------------
scenario.files <- list.files(SCENARIO.FILES.FOLDER,
                             full.names = T)

## Read in all batch files and combine to one dataframe ------------------------
# Loop through files to find matching ones
data.list <- list()
for (f in scenario.files) {
  # Check if the file contains the search string
  if (grepl(SCENARIO.INPUT.BATCH.FILES.DATASTRING, f)) {
    # Read the file into a dataframe (assuming it's a CSV, adjust if needed)
    df <- load_csv_iamc(file_path = f, mode = "fast")
    # Add the dataframe to the list
    data.list[[length(data.list) + 1]] <- df
  }
}
# Combine all dataframes into one
s <- bind_rows(data.list)

# Convert to long --------------------------------------------------------------
# ...Note: this can take fairly long. If you want this to be faster, then first filter for relevant variables, regions, etc.
s <- s %>% iamc_wide_to_long(upper.to.lower = T)

# General filtering ------------------------------------------------------------
s <- s %>% filter(
  # model!="REMIND 3.1" # was called REMIND 3.1 in first submission, then reuploaded as REMIND-MAgPIE 3.4-4.8 (checked to have exactly the same temperature values)
)


# Filter for emissions ---------------------------------------------------------
s.em <- s %>% filter_begins_with("Emissions")
s.em.w <- s.em %>% filter(region=="World")

# Filter for species of interest -----------------------------------------------

variables.to.keep <- c(
  s.em.w %>% filter_starts_with("Emissions|HFC") %>% pull(variable) %>% unique(),
  s.em.w %>% iamc_variable_keep_two_levels(levels = c(1,2)) %>% pull(variable) %>% unique()
)

emissions <- s.em.w %>% filter(variable%in%variables.to.keep) %>%
  iamc_variable_keep_two_levels(levels = c(2,3)) %>%
  mutate(species = paste0(variable, " [", unit, "]"))

variables.reported.global <- emissions %>%
  distinct(model,variable,unit,species)

# Save out list ----------------------------------------------------------------
write_delim(
  x = variables.reported.global %>% distinct(model,species) %>% mutate(in.data = "TRUE") %>%
    pivot_wider(names_from = model, values_from = in.data),
  file = here("output-other", "F-gases", "models-reporting-species.csv"),
  delim = ","
)

# Plot emissions species and scenarios -----------------------------------------
p.emissions <- ggplot(emissions %>% filter(
  year >= 2010, year <= 2100,
  scenario %in% c(
    "SSP1 - Very Low Emissions",
    "SSP2 - Medium Emissions",
    "SSP5 - High Emissions"
  )
),
                      aes(x=year,y=value,group=interaction(model,scenario,species),colour=scenario,shape=scenario)) +
  facet_grid(species~model, scales = "free_y") +
  geom_point() +
  geom_line() +
  theme_jsk() +
  mark_history() +
  ylab(NULL) +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0)
  )
p.emissions

save_ggplot(
  p = p.emissions,
  f = here("output-other", "F-gases", "models_trajectories-species"),
  h = 600,
  w = 600
)




# SHOW AR6 MESSAGE INFILLED ----------------------------------------------------
ar6.message.infilled <- load_csv_iamc(file_path = "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Scenario data/Scenario Databases/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv",
                              mode = "fast") %>%
  filter_starts_with(variable.string = "MESSAGE", column.name = "Model") %>%
  filter_starts_with(variable.string = "AR6 climate diagnostics|Infilled|", column.name = "Variable") %>%
  iamc_wide_to_long(upper.to.lower = T)

## plot emissions --------------------------------------------------------------
p.emissions.message.ar6 <- ggplot(ar6.message.infilled %>% filter(
  year >= 2010, year <= 2100
) %>% iamc_variable_keep_two_levels(levels = c(4,5)) %>%
  mutate(species = paste0(variable, " [", unit, "]")) %>%
  mutate(model = "MESSAGE"),
aes(x=year,y=value,group=interaction(model,scenario,species))) +
  facet_wrap(~species, scales = "free_y") +
  mark_history() +
  geom_line(alpha = 0.05) +
  theme_jsk() +
  ylab(NULL)
p.emissions.message.ar6

save_ggplot(
  p = p.emissions.message.ar6,
  f = here("output-other", "F-gases", "message_ar6_infilled_trajectories-species"),
  h = 600,
  w = 600
)
