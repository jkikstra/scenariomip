# Remove unwanted scenarios ----
remove_unwanted_scenarios <- function(df){
  df %>%
    filter(
      !(model=="MESSAGEix-GLOBIOM 2.1-M-R12")
    ) %>%
    return()
}



# Naming ----

## ScenarioMIP design description paper ----
rename_scenariomip_shortletters <- function(df){
  df %>%

    # options
    # 1 high-extension -- High Emissions (H): something that explores high-end range emission pathways, would be good to submit SSP3 and SSP5 based variants; teams may consider even specific high emission variants of these scenarios; as indicated in the document – we expect the scenarios to come out below RCP8.5
    # 2 high-overshoot -- N/A: extension variant after 2125.
    # 3 low -- Low Emissions (L): comparable with likely 2C; reaching net zero GHG; SSP1 / SSP2 based
    # 4 medium-extension -- Medium Emissions (M): following current policies; more or less stable emissions; based on all SSPs, or if one needs to prioritize SSP2
    # 5 medium-overshoot -- Medium-Low Emissions (ML): emissions are reduced slowly after 2050 based on medium; reaching net-zero around 2100
    # 6 verylow -- Very Low Emissions (VLLO): lowest possible emission trajectory – trying to stay as close to C1 type scenarios as possible; SSP1/SSP2 based, this pathways should depict futures with sustainable levels of land-based CDR. Please note that the 2030 emission level should be ‘ plausible’
    # 7 verylow-overshoot -- Low Overshoot (VLHO): transitioning from Low (or slightly above) to Very Low based on high levels of negative emissions (SSP1/SSP2 based), reaching the VL level around 2100 (can be slightly later). The moment of transition from Low is not specified in detail – but could be around 2050


    # short letters
    mutate_cond(scenario=="high-extension",scenario="H") %>%
    # mutate_cond(scenario=="high-overshoot",scenario="HOS") %>%
    mutate_cond(scenario=="medium-extension",scenario="M") %>%
    mutate_cond(scenario=="medium-overshoot",scenario="ML") %>%
    mutate_cond(scenario=="low",scenario="L") %>%
    mutate_cond(scenario=="verylow",scenario="VLLO") %>%
    mutate_cond(scenario=="verylow-overshoot",scenario="VLHO") %>%

    return()
}


## Add target scenarios info to IAM scenarios ----
add_scenariomip_targets_to_IAM_scenarios <- function(df){
  df %>% mutate(target = NA) %>%
    mutate_cond(grepl(x=scenario, pattern="High Emissions", fixed=T), target = "H") %>%
    mutate_cond(grepl(x=scenario, pattern="Medium Emissions", fixed=T), target = "M") %>%
    mutate_cond(grepl(x=scenario, pattern="Low Emissions", fixed=T), target = "L") %>% # needs to come before ML and VL, to correctly overwrite
    mutate_cond(grepl(x=scenario, pattern="Medium-Low Emissions", fixed=T), target = "ML") %>%
    mutate_cond(grepl(x=scenario, pattern="Very Low Emissions", fixed=T), target = "VLLO") %>% # not the case for REMIND
    mutate_cond(grepl(x=scenario, pattern="Low Overshoot", fixed=T), target = "VLHO") %>% # not the case for REMIND

    return()
}

## Add SSP info ----
add_ssp_basis_to_IAM_scenarios <- function(df){
  df %>% mutate(ssp=substr(scenario, start = 1, stop = 4))
}

## Plotting ----

### Shorter sector names ----
shorter_sector_names <- function(df){
  df %>%
    mutate_cond(variable=="Agricultural Waste Burning", variable = "AWB") %>%
    mutate_cond(variable=="Energy Sector", variable = "Energy") %>%
    mutate_cond(variable=="Forest Burning", variable = "Forest Fires") %>%
    mutate_cond(variable=="Grassland Burning", variable = "Grassland Fires") %>%
    mutate_cond(variable=="Industrial Sector", variable = "Industry") %>%
    mutate_cond(variable=="International Shipping", variable = "Intl. Shipping") %>%
    mutate_cond(variable=="Residential Commercial Other", variable = "ResCom Other") %>%
    mutate_cond(variable=="Solvents Production and Application", variable = "Solvents") %>%
    mutate_cond(variable=="Transportation Sector", variable = "Transport") %>%
    return()
}


# Historical emissions ----
format_historical <- function(df, start.year=1997, end.year=2023){# note that the current GFED data source doesn't go earlier than 1997
  return(
    df %>%
      filter(
        year>=start.year, year<=end.year
      )
  )
}
format_historical_add_total <- function(df){
  return(
    df %>%
      bind_rows(
        df %>%
          mutate(variable_copy = variable) %>%
          iamc_variable_keep_two_levels(c(1,2)) %>% # remove the sector level information; sum over sectors
          mutate(scenario = "Combined historical data sources") %>% # remove source information; sum over historical data sources
          reframe(
            value = ifelse(is.na(sum(value)),
                           0,
                           sum(value)),
            .by = c("model", "scenario", "region", "variable", "unit", "year")
          ) %>%
          mutate(variable = paste0(variable, "|Total"))
      )
  )
}

format_historical_onlykeep_iamnativeregions <- function(df,
                                                        message.name = "MESSAGEix-GLOBIOM-GAINS 2.1-R12",
                                                        coffee.name = "COFFEE 1.5"){
  only.selected.native.regions <- df %>% filter(
    grepl(x=region, pattern=message.name, fixed=T)|
      grepl(x=region, pattern="AIM 3.0", fixed=T)|
      grepl(x=region, pattern=coffee.name, fixed=T)|
      grepl(x=region, pattern="GCAM 7.1", fixed=T)|
      grepl(x=region, pattern="IMAGE 3.4", fixed=T)|
      grepl(x=region, pattern="REMIND-MAgPIE 3.4-4.8", fixed=T)|
      grepl(x=region, pattern="WITCH 6.0", fixed=T)
  )

  return(
    only.selected.native.regions
  )
}

format_historical_add_world_onlykeep_iamnativeregions <- function(df){
  only.selected.native.regions <- format_historical_onlykeep_iamnativeregions(df)

  world <- only.selected.native.regions %>%
    reframe(
      value = sum(value),
      .by = c("model", "scenario", "variable", "unit", "year")
    ) %>%
    mutate(region="World") %>%
    select(model,scenario,region,variable,unit,year,value)

  return(
    bind_rows(
      only.selected.native.regions,
      world
    )
  )
}

process_history_national <- function(df, add.total=T){

  if (add.total){
    df <- df %>%
      format_historical() %>%
      format_historical_add_total()
  } else {
    df <- df %>%
      format_historical()
  }

  return(
    df
  )
}
process_history_regional <- function(df, add.total=T){

  if (add.total){
    df <- df %>%
      format_historical() %>%
      format_historical_add_total() %>%
      format_historical_add_world_onlykeep_iamnativeregions()
  } else {
    df <- df %>%
      format_historical() %>%
      format_historical_add_world_onlykeep_iamnativeregions()
  }

  return(
    df
  )
}

# Aggregation to harmonization sectors ----

# helper column
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




# Industrial sector: create aggregate sector

process_industry <- function(df){

  industrial_sector_data <- df %>%
    filter(
      sector %in% c(
        "Energy|Demand|Industry",
        "Energy|Demand|Other Sector",
        "Industrial Processes",
        "Other"
      )
    )
  industrial_sector_data <- industrial_sector_data %>%
    reframe(
      value = sum(value, na.rm = T),
      .by = c("model", "scenario", "region", "unit", "year", "species")
    ) %>%
    mutate(sector = "Industrial Sector")

  industrial_sector_data <- industrial_sector_data %>%
    mutate(variable = paste0("Emissions|", species,"|",sector)) %>%
    select(model,scenario,region,variable,unit, year, value)

  return(industrial_sector_data)

}

process_aircraft <- function(df){

  # Aircraft: create aggregate (intl + domestic) sector
  aircraft_sector_data <- df %>%
    filter(
      sector %in% c(
        "Energy|Demand|Bunkers|International Aviation",
        "Energy|Demand|Transportation|Domestic Aviation"
      )
    )
  aircraft_sector_data <- aircraft_sector_data %>%
    # sum aircraft sectors
    reframe(
      value = sum(value, na.rm = T),
      .by = c("model", "scenario", "region", "unit", "year", "species")
    ) %>%
    mutate(sector = "Aircraft")

  aircraft_sector_data <- aircraft_sector_data %>%
    mutate(variable = paste0("Emissions|", species,"|",sector)) %>%
    select(model,scenario,region,variable,unit,year,value) %>%
    filter(
      # only keep World for Aircraft
      region=="World"
    )

  return(aircraft_sector_data)

}

process_transport <- function(df){

  # Transportation: subtract domestic aviation
  transport_sector_data <- scenarios_harmonization %>%
    filter(
      sector %in% c(
        "Energy|Demand|Transportation",
        "Energy|Demand|Transportation|Domestic Aviation"
      )
    )
  transport_sector_data <- transport_sector_data %>%
    select(-variable) %>%
    pivot_wider(names_from = sector, values_from = value) %>%
    # set so zero if not in data
    mutate(`Energy|Demand|Transportation` = if ("Energy|Demand|Transportation" %in% names(.)) `Energy|Demand|Transportation` else 0) %>%
    mutate(`Energy|Demand|Transportation|Domestic Aviation` = if ("Energy|Demand|Transportation|Domestic Aviation" %in% names(.)) `Energy|Demand|Transportation|Domestic Aviation` else 0) %>%
    # subtract domestic aviation
    mutate(value = ifelse(is.na(`Energy|Demand|Transportation`), 0, `Energy|Demand|Transportation`) - ifelse(is.na(`Energy|Demand|Transportation|Domestic Aviation`), 0, `Energy|Demand|Transportation|Domestic Aviation`)) %>%
    reframe(
      value = sum(value),
      .by = c("model", "scenario", "region", "unit", "year", "species")
    ) %>%
    mutate(sector = "Transportation Sector")

  transport_sector_data <- transport_sector_data %>%
    mutate(variable = paste0("Emissions|", species,"|",sector)) %>%
    select(model,scenario,region,variable,unit,year,value)

  return(transport_sector_data)

}

process_agriculture <- function(df){

  agriculture_sector_data <- df %>%
    filter(
      sector %in% c(
        "AFOLU|Agriculture",
        "AFOLU|Land|Harvested Wood Products",
        "AFOLU|Land|Land Use and Land-Use Change",
        "AFOLU|Land|Other",
        "AFOLU|Land|Wetlands"
      )
    )
  agriculture_sector_data <- agriculture_sector_data %>%
    reframe(
      value = sum(value, na.rm = T),
      .by = c("model", "scenario", "region", "unit", "year", "species")
    ) %>%
    mutate(sector = "Agriculture")

  agriculture_sector_data <- agriculture_sector_data %>%
    mutate(variable = paste0("Emissions|", species,"|",sector)) %>%
    select(model,scenario,region,variable,unit, year, value)

  return(agriculture_sector_data)

}


