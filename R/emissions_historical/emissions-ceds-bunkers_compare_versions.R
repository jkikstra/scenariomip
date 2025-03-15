# Load libraries ---------------------------------------------------------------
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
# install.packages(c(
#   "here",
#   "tidyverse",
#   "vroom",
#   "readxl",
#   "patchwork",
#   "ggthemes",
#   "ggsci",
#   "testthat",
#   "geomtextpath",
#   "stringr",
#   "ggthemes"
# ))
here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Check CEDS update
path.data <- here("data", "emissions_historical")
old <- vroom(file.path(path.data, "ceds_cmip7_intlAviationShipping_0010.csv")) %>% iamc_wide_to_long() # v2024_07_08
new <- vroom(file.path(path.data, "ceds_cmip7_intlAviationShipping_0011.csv")) %>% iamc_wide_to_long() # v_2025_03_11

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

ceds.bunkers.comparison <- bind_rows(
  old %>% mutate(version = "2024_07_08"),
  new %>% mutate(version = "2025_03_11")
) %>% add_sector_and_species_columns() %>%
  mutate_cond(grepl(sector,pattern="Aircraft"), sector="Aircraft") %>%
  mutate_cond(grepl(sector,pattern="Shipping"), sector="International Shipping") %>%
  reframe(
    value = sum(value),
    .by = c("model", "scenario", "region", "sector", "species", "unit", "year", "version")
  )
# plot global timeseries per species
p.ceds.bunkers <- ggplot(ceds.bunkers.comparison,# %>% filter(year>=1800,year<=2000),
aes(x=year,y=value,linetype=version)) +
  facet_wrap(sector~unit, scales="free_y") +
  geom_line(
    aes(group=interaction(model,scenario,region,sector,species))
  )
p.ceds.bunkers
save_ggplot(
  p = p.ceds.bunkers,
  f = here("figures", "emissions-historical", "bunkers", "ceds-update-bunkers"),
  w = 400,
  h = 275
)

# check SO2/CO2 issue highlighted in 2024 old version by Leon Simons
shipping.so2.co2 <- ceds.bunkers.comparison %>%
  filter(sector=="International Shipping", species%in%c("Sulfur", "CO2"))
shipping.so2.co2.point <- shipping.so2.co2 %>%
  select(-species) %>%
  pivot_wider(names_from = unit, values_from = value)

p.shipping.so2.co2.point <- ggplot(shipping.so2.co2.point,
                             aes(x=`Mt SO2/yr`, y=`Mt CO2/yr`)) +
  geom_point(aes(colour=version))
p.shipping.so2.co2.point
p.shipping.so2.co2.time <- ggplot(shipping.so2.co2,
                                   aes(x=year, y=value)) +
  facet_grid(species~., scales = "free_y") +
  geom_line(aes(colour=version))
p.shipping.so2.co2.time


p.shipping.so2.co2 <- (p.shipping.so2.co2.point + p.shipping.so2.co2.time)
save_ggplot(
  p = p.shipping.so2.co2,
  f = here("figures", "emissions-historical", "bunkers", "ceds-update-shipping-so2-co2"),
  w = 300,
  h = 200
)
