
# TODO ----
# step 1: variables for harmonization

# step 2: enough years

# step 3: other weird things

# step 4: check zero in CEDS
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


# step 5: model-specific coverage of species-sector space

present.or.not.template <- expand_grid(
  model.list,
  sector.species.list
) %>% rename(model=model.list)

for (yr in c(2020,2021,2023,2025)){
  model.variable.reporting <- scenarios_harmonization %>%
    filter(region=="World",year==yr) %>%
    add_sector_and_species_columns() %>%
    distinct(model,sector,species) %>%
    mutate(reported ="yes")

  present.or.not <- present.or.not.template %>%
    left_join(
      model.variable.reporting
    ) %>%
    mutate_cond(
      is.na(reported),
      reported = "no"
    ) %>%

    # add zero in ceds info
    left_join(
      zero.in.ceds %>% select(-model) %>% pivot_longer(3:ncol(zero.in.ceds)-1, names_to = "species", values_to = "zero_in_ceds") %>%
        filter(!is.na(zero_in_ceds))
    ) %>%
    mutate_cond(
      ((reported=="no") & (!is.na(zero_in_ceds))), # if
      reported = "no (but zero in CEDS)" # then
    ) %>%
    select(-zero_in_ceds) %>%

    # allow peat burning to not be reported
    mutate_cond(
      ((reported=="no") & (sector=="Peat Burning")), # if
      reported = "no (but will be filled in)" # then
    ) %>%


    # back to wide format
    pivot_wider(
      names_from = species,
      values_from = reported
    ) %>%
    arrange(model,sector)
  present.or.not

  dir.create(here("data", "data_vetting", "figures", "vetting-historical-reporting"))
  write_delim(
    x = present.or.not,
    file = here("data", "data_vetting", "figures", "vetting-historical-reporting", paste0("variable_reporting", as.character(yr), ".csv")),
    delim = ","
  )
}

