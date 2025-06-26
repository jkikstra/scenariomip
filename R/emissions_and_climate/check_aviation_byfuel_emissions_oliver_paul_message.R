#' Code for aviation by fuel analysis  ScenarioMIP
#' Developed by Jarmo Kikstra


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


# Load files ----
f.old <- "C:/Users/kikstra/Documents/GitHub/emissions_harmonization_historical/data/raw/ceds/ceds_cmip7_intlAviationShipping_0010.csv"
f.new <- "C:/Users/kikstra/Documents/GitHub/emissions_harmonization_historical/data/raw/ceds/ceds_cmip7_Aircraft_intlShipping_byfuel_v_2025_03_18.csv"

d.old <- read_csv(f.old)
d.new <- read_csv(f.new)

d <- bind_rows(d.old,d.new) %>%
  iamc_wide_to_long() %>%
  add_sector_and_species_columns()

# Plot differences ----

## Totals ----
p.total <- ggplot(
  d %>% filter(year>=2015,
               grepl(x=sector,pattern="Aircraft",fixed=T)) %>%
    reframe(
      value = sum(value),
      .by = c("model", "scenario", "region", "species", "unit", "year")
    ),
  aes(x=year,y=value,linetype=model)
) +
  facet_wrap(~interaction(species,unit), scales="free_y") +
  geom_line() +
  labs(title = "Global Aircraft and International Shipping emissions updated CEDS", y=NULL) +
  theme_jsk()
p.total

save_ggplot(p=p.total,f=here("figures", "aviation_ceds_update_total_formessage"),
            h=200, w=200)


## By fuel and species ----
p.byfuel <- ggplot(
  d %>% filter(year>=2015,
               grepl(x=sector,pattern="Aircraft",fixed=T)),
  aes(x=year,y=value,linetype=model,colour=sector)
) +
  facet_grid(interaction(species,unit)~sector, scales="free_y") +
  geom_line(aes(group=interaction(model,scenario,region,variable))) +
  labs(title = "Global Aircraft and International Shipping emissions updated CEDS", y=NULL) +
  theme_jsk()
# p.byfuel

save_ggplot(p=p.byfuel,f=here("figures", "aviation_ceds_update_byfuel_formessage"),
            h=800, w=400)
