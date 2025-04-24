# do subsectors, used for scenarioMIP, like Emissions|CO2|*, add up to the total Emissions|CO2?

# Emissions|CO2 {original} == sum(Emissions|CO2|*) {original} ----
aggregation.issues <- NULL
for (e in EMISSIONS.SPECIES.SECTORAL){
  totals.original <- scenarios %>% filter(variable == paste0("Emissions|",e)) %>%
    rename(total=value) %>%
    mutate(variable=e) %>%
    select(model,scenario,region,variable,unit,year,
           total)
  sums.original <- scenarios %>%
    filter(variable %in% paste0(paste0("Emissions|",e,"|"), SECTOR.VARIABLES.ALL ) ) %>%
    reframe(sums=sum(value),
            .by = c("model","scenario","region","unit","year")) %>%
    mutate(variable=e)

  compare <- left_join(
    totals.original,
    sums.original
  )

  aggregation.issues <- aggregation.issues %>%
    bind_rows(
      compare %>% filter(sums!=total) %>% mutate(diff = sums - total)
    )
}
# expect_equal(nrow(aggregation.issues),
#              0,
#              label = aggregation.issues)
write_delim(x = aggregation.issues,
            file = here("data", "data_vetting", "output_data", "aggregation_checks", "original_reporting_sums_not_same_as_total.csv"),
            delim = ",")
write_delim(x = data.frame(variable=SECTOR.VARIABLES.ALL),
            file = here("data", "data_vetting", "output_data", "aggregation_checks", "original_reporting_sums_not_same_as_total_sumvariables.csv"),
            delim = ",")

# Emissions|CO2 {original} == Emissions|CO2|Total {harmonized} ----
aggregation.issues <- NULL
for (e in EMISSIONS.SPECIES.SECTORAL){
  totals.original <- scenarios %>% filter(variable == paste0("Emissions|",e)) %>%
    rename(original=value) %>%
    mutate(variable=e) %>%
    select(model,scenario,region,variable,unit,year,
           original)
  totals.processed <- scenarios_harmonization %>% filter(variable == paste0("Emissions|",e,"|Total")) %>%
    rename(processed=value) %>%
    mutate(variable=e) %>%
    select(model,scenario,region,variable,unit,year,
           processed)

  compare <- left_join(
    totals.original,
    totals.processed
  )

  aggregation.issues <- aggregation.issues %>%
    bind_rows(
      compare %>% filter(processed!=original)
    )
}
expect_equal(nrow(aggregation.issues),
             0,
             label = aggregation.issues)

# sum(Emissions|CO2|*) {harmonized} == Emissions|CO2|Total {harmonized} ----

