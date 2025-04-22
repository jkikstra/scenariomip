# create helper columns (sector, species)
scenarios_harmonization <- scenarios %>%
  add_sector_and_species_columns()

# 1-on-1 mapping
scenarios_harmonization_1_on_1 <- scenarios_harmonization %>% filter(
  # filter
  sector %in% c(
    "Total",

    "Energy|Supply",
    "Energy|Demand|Bunkers|International Shipping",
    "Energy|Demand|Residential and Commercial and AFOFI",
    "Product Use",
    "AFOLU|Agricultural Waste Burning",
    "AFOLU|Land|Fires|Forest Burning",
    "AFOLU|Land|Fires|Grassland Burning",
    "AFOLU|Land|Fires|Peat Burning",
    "Waste"
  )
) %>%
  # rename existing 1-on-1 mappings
  mutate_cond(sector=="Energy|Supply", sector="Energy Sector") %>%
  mutate_cond(sector=="Energy|Demand|Bunkers|International Shipping", sector="International Shipping") %>%
  mutate_cond(sector=="Energy|Demand|Residential and Commercial and AFOFI", sector="Residential Commercial Other") %>%
  mutate_cond(sector=="Product Use", sector="Solvents Production and Application") %>%
  mutate_cond(sector=="AFOLU|Agricultural Waste Burning", sector="Agricultural Waste Burning") %>%
  mutate_cond(sector=="AFOLU|Land|Fires|Forest Burning", sector="Forest Burning") %>%
  mutate_cond(sector=="AFOLU|Land|Fires|Grassland Burning", sector="Grassland Burning") %>%
  mutate_cond(sector=="AFOLU|Land|Fires|Peat Burning", sector="Peat Burning") %>%
  mutate_cond(sector=="Waste", sector="Waste") %>%
  # remove helper columns
  mutate(variable = paste0("Emissions|", species, "|", sector)) %>%
  select(-species,-sector)

scenarios_harmonization_aggregation <- process_industry(scenarios_harmonization) %>%
  bind_rows(process_aircraft(scenarios_harmonization)) %>%
  bind_rows(process_transport(scenarios_harmonization)) %>%
  bind_rows(process_agriculture(scenarios_harmonization))

# combine ----
scenarios_harmonization <- bind_rows(
  scenarios_harmonization_1_on_1,
  scenarios_harmonization_aggregation
)

# further filtering of species ----
scenarios_harmonization <- scenarios_harmonization %>%
  # remove Kyoto Gases aggregate, which we do not use for harmonization
  filter(!grepl(variable, pattern="Kyoto Gases"))


# save for further inspection (in wide format) ----
write_delim(x = scenarios_harmonization %>%
              filter(year>=2010) %>% # select only years common to all IAMs, for ease of use and because it makes the file a bit smaller
              iamc_long_to_wide(),
            file = file.path(IAM_SCENARIOS_LOCATION, paste0(substr(IAM_SCENARIOS_FILE, start=1, stop = nchar(IAM_SCENARIOS_FILE)-4),"_harmonizationsectors",".csv")),
            delim = ",")
