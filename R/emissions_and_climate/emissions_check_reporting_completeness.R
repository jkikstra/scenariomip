
# TODO ----
# step 1: variables for harmonization

# step 2: enough years

# step 3: other weird things


# OLD implementation ----
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
    ) %>% pivot_wider(
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


for (m in present.or.not %>% pull(model) %>% unique() ){

  present.or.not.per.model <- present.or.not %>% filter(
    model==m
  )

  m.simple <- present.or.not.per.model %>% simplify_model_names() %>% pull(model) %>% unique()


  dir.create(here("data", "data_vetting", "figures", "per-model", m.simple))
  write_delim(
    x = present.or.not.per.model,
    file = here("data", "data_vetting", "figures", "per-model", m.simple, paste0("variable_reporting_",m,"_20250409.csv")),
    delim = ","
  )
}
