

# load scenario data ----
if (MESSAGE.INTERNAL.OR.SCEN.EXPL=="internal"){
  scenarios <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, "SSP_SSP2_v4.0.1_SSP2 - Low Emissions.csv"), mode="fast") %>%
    bind_rows(
      load_excel_iamc(
        file.path(IAM_SCENARIOS_LOCATION, "SSP_SSP3_v5.2_baseline_1000f.xlsx")
      )
    )
} else {
  scenarios <- load_csv_iamc(file.path(IAM_SCENARIOS_LOCATION, IAM_SCENARIOS_FILE), mode="fast")
}

# only keep emissions data
scenarios <- scenarios %>%
  filter(str_starts(Variable, "Emissions"))
# to long format
scenarios <- scenarios %>%
  iamc_wide_to_long(upper.to.lower = T) %>% #ifelse(MESSAGE.INTERNAL.OR.SCEN.EXPL%in%c("scen.exp.scenariomip"),T,F)) %>%
  filter(year<=2100)

# renaming ----
# ece-internal database specific stuff:
if (MESSAGE.INTERNAL.OR.SCEN.EXPL=="internal"){
  scenarios <- scenarios  %>% mutate(model="MESSAGE")
}

# filtering ----
# remove scenarios we do not want
scenarios <- scenarios %>%
  remove_unwanted_scenarios()

# remove all non-native regions, by removing (Rx) regions
scenarios <- scenarios %>%
  filter(
    !grepl(x=region, pattern="R10", fixed=T),
    !grepl(x=region, pattern="R9", fixed=T),
    !grepl(x=region, pattern="R5", fixed=T)
  )


# lists ----
model.list <- scenarios %>% pull(model) %>% unique()
model.list.simple <- scenarios %>% distinct(model) %>% simplify_model_names() %>% pull(model) %>% unique()
scenario.list.main <- scenarios %>% distinct(scenario) %>% filter(!grepl("_", x=scenario)) %>% pull(scenario)
scenario.list.alternative <- scenarios %>% distinct(scenario) %>% filter(grepl("_", x=scenario)) %>% pull(scenario)

# tests ----
stopifnot(length(model.list)==length(model.list.simple)) # check that there is not multiple model versions in the data
