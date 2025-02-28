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

library("countrycode")

here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

## Functions ----
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


## Agricultural Waste Burning (AgWast / AWB) and GDP per capita ----------------

### Data ----
awb <- vroom(here("data", "emissions_historical", "gfed_cmip7_0011.csv")) %>%
  iamc_wide_to_long() %>%
  filter_variable_includes("Agricultural Waste Burning") %>%
  rename(iso = region) %>%
  mutate(iso = toupper(iso))

pop <- population %>% mutate(iso = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c"))

gdp <- vroom(here("data", "misc", "API_NY.GDP.PCAP.KD_DS2_en_csv_v2_76156", "API_NY.GDP.PCAP.KD_DS2_en_csv_v2_76156.csv"), skip = 4) %>%
  wb_parse() %>%
  rename(gdp_pcap=value)

### Combined data ----
awb.wcorr <- awb %>% left_join(pop, by = c("iso", "year")) %>%
  mutate(t_pcap = value * 1e6 / population) %>%
  left_join(gdp, by = c("iso", "year")) %>%
  add_sector_and_species_columns()

### Plots ----

#### Correlation plot ----
p.awb.gdp_cap.corr <- ggplot(
  awb.wcorr %>%
    filter(
      species == "BC",
      # population>0.5*1e6,
      # t_pcap>1e-6
    ),
  aes(x = gdp_pcap, y = t_pcap)
) +
  facet_wrap(iso~species, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", formula = 'y ~ x') +
  theme_void()
p.awb.gdp_cap.corr

save_ggplot(
  p = p.awb.gdp_cap.corr,
  h = 500,
  w = 300,
  f = here("figures", "correlates", "agwaste_gdp")
)

##### Correlation tests ----

cor(awb.wcorr$gdp_pcap, awb.wcorr$t_pcap, method = c("pearson", "kendall", "spearman"))
cor.test(awb.wcorr$gdp_pcap, awb.wcorr$t_pcap, method=c("pearson", "kendall", "spearman"))

r = cor(awb.wcorr$gdp_pcap, awb.wcorr$t_pcap)
lm.fit = lm(awb.wcorr$gdp_pcap~awb.wcorr$t_pcap)
R2 = summary(lm.fit)$r.squared
R2
