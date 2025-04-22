# File names ----

## Harmonization ----

HISTORY.COUNTRY.AND.GLOBAL <- "cmip7_history_0022.csv" # national, harmonization sectors, CEDS & GFED
HISTORY.IAMREGIONS <- "iamc_regions_cmip7_history_0021_0020.csv" # regional, harmonization sectors, CEDS & GFED
HISTORY.COMPOSITE.GLOBAL.INCL.FGASES <- "cmip7_history_world_0022.csv" # global, harmonization sectors, CEDS & GFED


## Supplemental ----

HISTORY.COUNTRY.EDGAR <- "edgar_national_0010.csv" # national, harmonization sectors
HISTORY.IAMREGIONS.EDGAR <- "iamc_regions_edgar_history_0020.csv" # regional, harmonization sectors
HISTORY.WORLD.EDGAR <- "edgar_global_0010.csv" # global, harmonization sectors

HISTORY.COUNTRY.FAO <- "fao_national_0010.csv" # national, harmonization sectors
HISTORY.IAMREGIONS.FAO <- "iamc_regions_fao_history_0020.csv" # regional, harmonization sectors
HISTORY.WORLD.FAO <- "fao_global_0010.csv" # global, harmonization sectors


# Loading data ----

## Harmonization ----
# In harmonization sectors
hist.national <- vroom(here("data", "data_vetting", "hist", HISTORY.COUNTRY.AND.GLOBAL)) %>%
  iamc_wide_to_long() %>%
  process_history_national()
hist.regional <- vroom(here("data", "data_vetting", "hist", HISTORY.IAMREGIONS)) %>%
  iamc_wide_to_long() %>%
  process_history_regional()

# global level, all species
hist.global.allspecies <- vroom(here("data", "data_vetting", "hist", HISTORY.COMPOSITE.GLOBAL.INCL.FGASES)) %>%
  iamc_wide_to_long() %>%
  format_historical()

## Supplemental ----

### EDGAR ----
hist.national.edgar <- vroom(here("data", "data_vetting", "hist", HISTORY.COUNTRY.EDGAR)) %>%
  bind_rows(vroom(here("data", "data_vetting", "hist", HISTORY.WORLD.EDGAR))) %>%
  iamc_wide_to_long() %>%
  process_history_national(add.total=F) %>%
  filter(
    !(year=2023 & value==0) # data provided by Steve only has EDGAR until 2022 for many sectors, with zeroes reported
  )
hist.regional.edgar <- vroom(here("data", "data_vetting", "hist", HISTORY.IAMREGIONS.EDGAR)) %>%
  iamc_wide_to_long() %>%
  process_history_regional(add.total=F) %>%
  filter(
    !(year=2023 & value==0) # data provided by Steve only has EDGAR until 2022 for many sectors, with zeroes reported
  )

### FAO ----
hist.national.fao <- vroom(here("data", "data_vetting", "hist", HISTORY.COUNTRY.FAO)) %>%
  bind_rows(vroom(here("data", "data_vetting", "hist", HISTORY.WORLD.FAO))) %>%
  iamc_wide_to_long() %>%
  process_history_national(add.total=F)
hist.regional.fao <- vroom(here("data", "data_vetting", "hist", HISTORY.IAMREGIONS.FAO)) %>%
  iamc_wide_to_long() %>%
  process_history_regional(add.total=F)

### CAMS ----
# tbd.


### GAINS ----
# tbd.


# Put all historical data in one dataframe ----
# all hist
hist.data.iam.regions <-
  # IAM regions
  hist.regional %>% filter(region!="World") %>%
  bind_rows(hist.regional.edgar %>% filter(region!="World")) %>% # IAM regions
  bind_rows(hist.regional.fao %>% filter(region!="World")) %>% # IAM regions
  # World
  bind_rows(hist.national %>% filter(region=="World")) %>%
  bind_rows(hist.national.edgar %>% filter(region=="World")) %>% # World
  bind_rows(hist.national.fao %>% filter(region=="World")) %>% # World
  # don't include domestic aviation in the comparison
  filter(!(grepl(variable, pattern="Aircraft", fixed = T) & region!="World"))

write_delim(
  x = hist.data.iam.regions,
  file = file.path(output.folder.data, paste0("history_iamregions_", DOWNLOAD.DATE, ".csv")),
  delim = ","
)

