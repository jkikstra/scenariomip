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
# install.packages("qpdf")
library("qpdf")


here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Notes ----
#' preparation for meeting with Stephanie Fiedler 21.03.2025
#' ...following up from analysis provided to her during the Reading meeting, emailed on 29.10.2024, with attachments like "slcf_regional_shares_Sulfur" from the script `scenarioMIP_prepareforReading.R`


# Where to save the figures? ----

path.figures <- here("figures", "aerchemmip")
path.figures.data <- here("figures_data", "aerchemmip")

if(!dir.exists(path.figures.data)) { dir.create(path.figures.data, recursive = TRUE) }

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

## standard list
vars.slcf <- c(
  "Emissions|Sulfur",
  "Emissions|NH3",
  "Emissions|BC",
  "Emissions|OC"
)


## Full list of variables
vars.all <- c(
  vars.slcf
)


# Loading IAM data ----
IAM_SCENARIOS_LOCATION <- here("data", "data_vetting", "scens")
IAM_SCENARIOS_FILE <- "scenarios_scenariomip_allmodels_2025-03-11.csv" # version presented in ScenarioMIP demand-side meeting on 13.03.2025

DOWNLOAD.NOTE <- "Downloaded on 11 March 2025 from internal ScenarioMIP database"


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
  remove_scenarios_with_issues() %>% # temporary fix (which should be reported to the modelling teams)
  remove_all_zero_values() # temporary fix (which should be reported to the modelling teams)

# save some useful lists
model.list <- scenarios %>% pull(model) %>% unique()
model.list.simple <- scenarios %>% distinct(model) %>% simplify_model_names() %>% pull(model) %>% unique()
scenario.list.main <- scenarios %>% distinct(scenario) %>% filter(!grepl("_", x=scenario)) %>% pull(scenario)
scenario.list.alternative <- scenarios %>% distinct(scenario) %>% filter(grepl("_", x=scenario)) %>% pull(scenario)


# Loading other data ----
# ...

# ... ... ... ... ... ... ... ... ... ------------------------------------------
# DIRECT VARIABLES -------------------------------------------------------------
# ... ... ... ... ... ... ... ... ... ------------------------------------------

# Short-lived forcers / pollution ----------------------------------------------
s.slcf.r10 <- scenarios %>% filter(
  grepl(x=region, pattern="R10", fixed=T)
)
# order data by scenarioMIP target scenario
s.slcf.r10$target <- factor(s.slcf.r10$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))

for (v in vars.slcf){
  for (MAIN in c(T,F)){

    if (MAIN) {s.list <- scenario.list.main} else {s.list <- scenario.list.alternative}


    df <- s.slcf.r10 %>%
      filter(
        variable == v,
        year %in% seq(2010,2100,10),
        scenario %in% s.list
      )

    variable.saving.name <- df %>%
      iamc_variable_keep_one_level(level = -1) %>%
      pull(variable) %>% unique()
    variable.saving.name <- paste0(variable.saving.name, ifelse(MAIN, "_main", "_alternative"))

    slcf.unit <- df %>%
      pull(unit) %>% unique()

    if (MAIN){
      p.slcf.r10 <- ggplot(df) +
        facet_grid(target~interaction(ssp,model)) +
        theme_jsk() +
        mark_history() +
        geom_area(
          aes(x=year, y=value,
              fill = region)
        )
    } else {
      p.slcf.r10 <- ggplot(df) +
        facet_grid(model~scenario) +
        theme_jsk() +
        mark_history() +
        geom_area(
          aes(x=year, y=value,
              fill = region)
        )
    }
    p.slcf.r10 <- p.slcf.r10 +
      labs(title = paste0("Total regional emissions for: ", variable.saving.name),
           subtitle = ifelse(MAIN, "Main scenario candidates", "Alternative scenario candidates"),
           caption = DOWNLOAD.NOTE) +
      ylab(slcf.unit)

    save_ggplot(
      f = file.path(path.figures, paste0("slcf_regional_levels_",variable.saving.name)),
      p = p.slcf.r10,
      h = 400,
      w = 1000
    )


    df.percentage <- df %>%
      group_by(model,scenario,variable,unit,year) %>%
      mutate(global=sum(value)) %>%
      mutate(share=value/global)


    if (MAIN){
      p.slcf.r10.percentage <- ggplot(df.percentage) +
        facet_grid(target~interaction(ssp,model)) +
        theme_jsk() +
        mark_history() +
        geom_area(
          aes(x=year, y=share,
              fill = region)
        )
    } else {
      p.slcf.r10.percentage <- ggplot(df.percentage) +
        facet_grid(model~scenario) +
        theme_jsk() +
        mark_history() +
        geom_area(
          aes(x=year, y=share,
              fill = region)
        )
    }
    p.slcf.r10.percentage <- p.slcf.r10.percentage +
      labs(title = paste0("Regional share of global emissions for: ", variable.saving.name),
           subtitle = ifelse(MAIN, "Main scenario candidates", "Alternative scenario candidates"),
           caption = DOWNLOAD.NOTE) +
      ylab(slcf.unit) +
      scale_y_continuous(labels = scales::percent)

    save_ggplot(
      f = file.path(path.figures, paste0("slcf_regional_shares_",variable.saving.name)),
      p = p.slcf.r10.percentage,
      h = 400,
      w = 1000
    )

  }
}

# combine all in one pdf

FILES.pdf <- file.path(path.figures, dir(path.figures, pattern = "*.pdf"))  # get file names
qpdf::pdf_combine(
  input = FILES.pdf[((grepl(FILES.pdf, pattern="_main",fixed=T))&!(grepl(FILES.pdf, pattern="slcf_main.pdf",fixed=T)))],
  output = file.path(path.figures, "slcf_main.pdf")
)
qpdf::pdf_combine(
  input = FILES.pdf[((grepl(FILES.pdf, pattern="_alternative",fixed=T))&!(grepl(FILES.pdf, pattern="slcf_alternative.pdf",fixed=T)))],
  output = file.path(path.figures, "slcf_alternative.pdf")
)
