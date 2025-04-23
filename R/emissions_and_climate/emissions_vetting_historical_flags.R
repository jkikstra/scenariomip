
# step 1: what is zero? (CEDS)
HARMONIZATION.YEAR <- 2023
zero.in.ceds <- hist.data.iam.regions %>% filter(year==HARMONIZATION.YEAR,
                                 grepl(model,pattern="CEDS",fixed=T),
                                 region=="World") %>%
  filter(value==0) %>%
  # distinct(variable)
  add_sector_and_species_columns() %>%
  distinct(model,sector,species) %>%
  mutate(reported_as_zero_in_ceds ="zero_in_ceds") %>%
  pivot_wider(
    names_from = species,
    values_from = reported_as_zero_in_ceds
  ) %>%
  arrange(model,sector)
write_delim(
  x = zero.in.ceds,
  file = here("data", "data_vetting", "figures", "vetting-historical-reporting", paste0("variables_zero_in_ceds_", as.character(HARMONIZATION.YEAR), ".csv")),
  delim = ","
)




# step 2: distance to CEDS-GFED

# step 3: outside of range of historical data (totals, global)
#' TODO:
#' - [ ] add GAINS global totals
#' - [ ] add CAMS global totals

# step 4: outside of range of historical data (totals per region)

# step 5: outside of range of historical data (totals per region and sector)


# # Put all historical data in one dataframe ----
# # all hist
# hist.data.iam.regions <-
#   # IAM regions
#   hist.regional %>%
#   bind_rows(hist.regional.edgar) %>% # IAM regions
#   bind_rows(hist.regional.fao) %>% # IAM regions
#   # World
#   bind_rows(hist.national %>% filter(region=="World")) %>%
#   bind_rows(hist.national.edgar %>% filter(region=="World")) %>% # World
#   bind_rows(hist.national.fao %>% filter(region=="World")) %>% # World
#   # don't include domestic aviation in the comparison
#   filter(!(grepl(variable, pattern="Aircraft", fixed = T) & region!="World"))
#
# write_delim(
#   x = hist.data.iam.regions,
#   file = file.path(output.folder.data, paste0("history_iamregions_", DOWNLOAD.DATE, ".csv")),
#   delim = ","
# )



#
# # scen hist years
# scenarios_harmonization_histyears <- scenarios_harmonization %>%
#   filter(year%in%c(2015,2020,2025)) %>%
#   iamc_long_to_wide() %>%
#   mutate(`2023` = `2020` + (`2025` - `2020`)/5 * 3  ) %>%
#   select(-`2020`, -`2025`)
#
# # CEDS: absolute distance (2015, 2023)
# diff.all.hist <- scenarios_harmonization_histyears %>%
#   left_join(hist.data.iam.regions %>% rename(hist.model=model) %>% select(-scenario) %>%
#               filter(year%in%c(2015,2023)) %>% iamc_long_to_wide() %>% rename(hist.2015=`2015`, hist.2023=`2023`),
#             by = join_by(region, variable, unit),
#             relationship = "many-to-many")
#
# diff.ceds.gfed <- diff.all.hist %>% filter((grepl(hist.model,pattern="CEDSv_2025_03_11", fixed=T) | grepl(hist.model,pattern="GFED", fixed=T) ),
#                                            !grepl(variable, pattern='Total', fixed=T))
#
# diff.ceds.gfed.abs.and.rel <- diff.ceds.gfed %>%
#   mutate(`Absolute difference (2023)` = `2023`-hist.2023,
#          `Relative difference (2023)` = `Absolute difference (2023)`/hist.2023) %>%
#   mutate(`Absolute difference (2015)` = `2015`-hist.2015,
#          `Relative difference (2015)` = `Absolute difference (2015)`/hist.2015) %>%
#   mutate(Information = "difference = model - history")
# View(diff.ceds.gfed.abs.and.rel)
#
# write_delim(
#   x = diff.ceds.gfed.abs.and.rel,
#   file = file.path(IAM_SCENARIOS_LOCATION, paste0("difference_to_CEDS_and_GFED_", substr(IAM_SCENARIOS_FILE, start=nchar(IAM_SCENARIOS_FILE)-13, stop = nchar(IAM_SCENARIOS_FILE)-4),"",".csv")),
#   delim = ","
# )
#
# # TBD: uncertainties
# # Emissions|CO2|Energy Sector
#
# # TBD: trends
