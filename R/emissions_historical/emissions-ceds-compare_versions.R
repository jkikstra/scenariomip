# Check CEDS update

# Load libraries ---------------------------------------------------------------
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

# Paths ------------------------------------------------------------------------
path.figures <- here("figures", "emissions-historical", "harmonization_sectors")

path.data <- here("data", "emissions_historical")


# Load data --------------------------------------------------------------------
old <- vroom(file.path(path.data, "ceds_cmip7_national_0010.csv")) %>% iamc_wide_to_long() # v2024_07_08
new <- vroom(file.path(path.data, "ceds_cmip7_national_0011.csv")) %>% iamc_wide_to_long() # v_2025_03_11

old.intl <- vroom(file.path(path.data, "ceds_cmip7_international_0010.csv")) %>% iamc_wide_to_long() # v2024_07_08
new.intl <- vroom(file.path(path.data, "ceds_cmip7_international_0011.csv")) %>% iamc_wide_to_long() # v_2025_03_11

add_sector_and_species_columns <- function(df){
  df %>%
    mutate(sector = str_replace(variable, "^Emissions\\|", "")) %>%
    mutate(species = str_extract(sector, "^[^|]+")) %>%
    mutate(sector = ifelse(
      species==sector,
      "Total",
      str_replace(sector, paste0("^",species,"\\|"), "")
    )) %>%
    return()

}

ceds.comparison <- bind_rows(
  old %>% mutate(version = "2024_07_08"),
  new %>% mutate(version = "2025_03_11")
) %>% add_sector_and_species_columns()

ceds.comparison.intl <- bind_rows(
  old.intl %>% mutate(version = "2024_07_08"),
  new.intl %>% mutate(version = "2025_03_11")
) %>% add_sector_and_species_columns() %>%
  filter(
    sector%nin%c(
      "Waste", "Industrial Sector" # see https://github.com/JGCRI/CEDS/issues/57
    )
  )


# Plot differences -------------------------------------------------------------
ceds <- ceds.comparison %>% bind_rows(ceds.comparison.intl)
ceds.sectors <- ceds %>% distinct(sector) %>% pull(sector)
ceds.species <- ceds %>% distinct(species) %>% pull(species)



## By country ----

# for (sec in c("Agriculture")){
for (sec in ceds.sectors){
  for (spec in ceds.species){
  # for (spec in c("BC")){
    plot.data <- ceds %>% filter(sector == sec, species == spec)
    v <- plot.data %>% distinct(variable) %>% pull(variable)
    if (!(plot.data %>% pull(value) %>% sum() == 0)){
      p.ceds <- ggplot(plot.data,
                       aes(x=year,y=value)) +
        facet_wrap(~region) +
        geom_line(aes(colour=version)) +
        theme_jsk() +
        labs(title = v,
             caption = "CEDS_v_2025_03_11")
      save_ggplot(
        p = p.ceds,
        f = file.path(path.figures,
                 paste0("ceds-update_", clean_string(v))  ),
        format = "pdf",
        w = 400,
        h = 400
      )
    } else {
      p.ceds <- ggplot() + annotate("text", x=0,y=0, label = paste0("Only zeroes for ", sec,"|",spec ) ) +
        theme_void() +
        labs(caption = "CEDS_v_2025_03_11")
      save_ggplot(
        p = p.ceds,
        f = file.path(path.figures,
                 paste0("ceds-update_", clean_string(v))  ),
        format = "pdf",
        w = 80,
        h = 30
      )
    }

  }
}

### Combine PDFs -----------------------------------------------------------------
FILES.pdf <- file.path(path.figures, dir(path.figures, pattern = "ceds-update_Emissions*"))  # get file names
FILES.pdf <- FILES.pdf[grepl(FILES.pdf,pattern=".pdf",fixed=T)]
qpdf::pdf_combine(
  input = FILES.pdf,
  output = file.path(path.figures, "ceds-update_all_percountry.pdf")
)


## Aggregated to World ----
ceds.world <- ceds %>% reframe(
  value = sum(value),
  .by = c("model", "scenario", "variable", "unit", "year", "version", "sector", "species")
) %>% mutate(region="World")
### Plot by species ----
for (spec in ceds.species){
  plot.data <- ceds.world %>% filter(species == spec)
  species.unit <- plot.data %>% distinct(unit) %>% pull(unit)
  if (!(plot.data %>% pull(value) %>% sum() == 0)){
    p.ceds <- ggplot(plot.data,
                     aes(x=year,y=value)) +
      facet_wrap(sector~., nrow = 3) +
      geom_line(aes(colour=version)) +
      theme_jsk() +
      labs(title = spec,
           subtitle = "World",
           y = species.unit,
           caption = "CEDS_v_2025_03_11")
    save_ggplot(
      p = p.ceds,
      f = file.path(path.figures,
                    paste0("ceds-update-world_", clean_string(spec))  ),
      format = "pdf",
      w = 400,
      h = 400
    )
  } else {
    p.ceds <- ggplot() + annotate("text", x=0,y=0, label = paste0("Only zeroes for ", spec ) ) +
      theme_void() +
      labs(caption = "CEDS_v_2025_03_11")
    save_ggplot(
      p = p.ceds,
      f = file.path(path.figures,
                    paste0("ceds-update-world_", clean_string(spec))  ),
      format = "pdf",
      w = 80,
      h = 30
    )
  }

}


### By sector ----
for (sec in ceds.sectors){
  plot.data <- ceds.world %>% filter(sector == sec)
  if (!(plot.data %>% pull(value) %>% sum() == 0)){
    p.ceds <- ggplot(plot.data,
                     aes(x=year,y=value)) +
      facet_wrap(interaction(species,unit)~., nrow = 5, scales = "free_y") +
      geom_line(aes(colour=version)) +
      theme_jsk() +
      labs(title = sec,
           subtitle = "World",
           y = NULL,
           caption = "CEDS_v_2025_03_11")
    save_ggplot(
      p = p.ceds,
      f = file.path(path.figures,
                    paste0("ceds-update-world_", clean_string(sec))  ),
      format = "pdf",
      w = 400,
      h = 400
    )
  } else {
    p.ceds <- ggplot() + annotate("text", x=0,y=0, label = paste0("Only zeroes for ", sec ) ) +
      theme_void() +
      labs(caption = "CEDS_v_2025_03_11")
    save_ggplot(
      p = p.ceds,
      f = file.path(path.figures,
                    paste0("ceds-update-world_", clean_string(sec))  ),
      format = "pdf",
      w = 80,
      h = 30
    )
  }

}

### Combine PDFs -----------------------------------------------------------------
FILES.pdf <- file.path(path.figures, dir(path.figures, pattern = "ceds-update-world_*"))  # get file names
FILES.pdf <- FILES.pdf[grepl(FILES.pdf,pattern=".pdf",fixed=T)]
qpdf::pdf_combine(
  input = FILES.pdf,
  output = file.path(path.figures, "ceds-update_all_world.pdf")
)
