# TODO:
# - [ ] Automate this script

update_region_names <- function(df,
                                new.name.coffee="COFFEE 1.6",
                                new.name.gcam="GCAM 7.1",
                                new.name.remind="REMIND-MAgPIE 3.5-4.10"){
  df %>%
    # update COFFEE name
    mutate(
      region = str_replace(
        region,
        "^COFFEE 1\\.5",
        new.name.coffee
      )
    ) %>%
    # # update GCAM name
    # mutate(
    #   region = str_replace(
    #     region,
    #     "^GCAM 7\\.1",
    #     new.name.gcam
    #   )
    # ) %>%
    # update REMIND name
    mutate(
      region = str_replace(
        region,
        "^REMIND-MAgPIE 3\\.4-4\\.8",
        new.name.remind
      )
    ) %>%
    return()
}

hist.data.iam.regions <- hist.data.iam.regions %>%
  update_region_names()



