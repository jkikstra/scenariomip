# TODO ----
#'- [ ] replace `hist.national` with `hist.data.iam.regions`
#'

filter_ceds_gfed <- function(df, keep=T){
  if (keep){
    return(
      df %>%
        filter(grepl(x=model, pattern="CEDS", fixed=T)|grepl(x=model, pattern="GFED", fixed=T))
    )
  } else {
    return(
      df %>% filter(!grepl(x=model, pattern="CEDS", fixed=T),
                    !grepl(x=model, pattern="GFED", fixed=T))
    )
  }
}


# Global ----
plot_historical_global_sector <- function(df, model.list.simple.for.plotting=model.list.simple,
                                          sector.list=sector.list,
                                          years.hist.to.highlight = c(1990,1995,2000,2010,2015,2020,2023)){
  for (m in model.list.simple.for.plotting){

    p.data.hist <- hist.data.iam.regions %>%
      filter(region=="World") %>%
      iamc_region_keep_one_level(-1) %>%
      iamc_variable_keep_one_level(-1) %>%
      shorter_sector_names()
    p.data.hist <-
      p.data.hist %>% filter_ceds_gfed(keep = F) %>%
      bind_rows(
        p.data.hist %>% filter_ceds_gfed() %>%  # combine data from CEDS and GFED into one variable
          filter(variable!="Total") %>% # remove separate totals of each historical data source
          bind_rows(
            # add combined total of historical data sources
            p.data.hist %>% filter(variable=="Total") %>% filter_ceds_gfed() %>%
              reframe(
                value = sum(value),
                .by = c("scenario", "variable", "region", "unit", "year")
              ) %>%
              mutate(model = p.data.hist %>% pull(model) %>% unique() %>% paste(collapse = "+"))
          )
      )


    for (scenario.set in c("main", "alternative")){
      if (scenario.set=="main") {s.list <- scenario.list.main} else {s.list <- scenario.list.alternative}
      p.data.model <- df %>%
        filter(region=="World",
               scenario%in%s.list) %>% mutate(full.model.name = model) %>%
        simplify_model_names() %>%
        filter(grepl(model, pattern=m, fixed=T))
      if (nrow(p.data.model)==0){
        next # skip this iteration if there's no model data (e.g. no alternative scenarios submitted)
      }
      p.data.model <- p.data.model %>%
        iamc_region_keep_one_level(-1) %>%
        iamc_variable_keep_one_level(-1) %>%
        shorter_sector_names()

      p.emissions.by.model.sector.global <- ggplot(
        p.data.model,
        aes(x=year,y=value)
      ) +
        facet_wrap(unit~variable, scales="free_y") +

        # IAMs
        geom_line(aes(colour=scenario,
                      linetype=full.model.name,
                      group=interaction(full.model.name,scenario,region,variable))) +
        # edgar
        geom_line(
          data = p.data.hist %>% filter_ceds_gfed(keep=F),
          colour = "darkgrey"
        ) +
        geom_point(
          data = p.data.hist %>% filter_ceds_gfed(keep=F) %>% filter(year%in%years.hist.to.highlight),
          colour = "darkgrey",
          aes(shape = model)
        ) +

        # CEDS-GFED
        geom_line(
          data = p.data.hist %>% filter_ceds_gfed(keep=T),
          colour = "black"
        ) +
        geom_point(
          data = p.data.hist %>% filter_ceds_gfed(keep=T) %>% filter(year%in%years.hist.to.highlight),
          colour = "black",
          aes(shape = model)
        ) +



        theme_jsk() +
        ylab(NULL) + xlab(NULL) +
        labs(
          title = m,
          subtitle = paste0("Scenario set: ", scenario.set)
        ) +
        mark_history()


      dir.create(here("data", "data_vetting", "figures", "per-model", m))
      save_ggplot(
        p = p.emissions.by.model.sector.global,
        h = 500,
        w = 700,
        format = "pdf",
        f = here("data", "data_vetting", "figures", "per-model", m, paste0("emissions_global", "_", scenario.set) )
      )

    }

  }
}

plot_historical_global_total <- function(df, model.list.simple.for.plotting=model.list.simple,
                                         years.hist.to.highlight = c(1990,1995,2000,2010,2015,2020,2023)){
  for (m in model.list.simple.for.plotting){

    p.data.hist <- hist.global.allspecies %>% mutate(variable="Total") %>%
      reframe(
        value = ifelse(unit == "Mt CO2/yr",
                       sum(value),
                       value),
        .by = c("scenario", "region", "variable", "unit", "year")
      ) %>% mutate(model="Composite history")
    p.data.hist.edgar <- hist.national.edgar %>%
      filter_includes("Total") %>%
      filter(region=="World") %>%
      mutate(variable="Total", scenario="historical")

    for (scenario.set in c("main", "alternative")){

      if (scenario.set=="main") {s.list <- scenario.list.main} else {s.list <- scenario.list.alternative}
      p.data.model <- df %>%
        filter_includes("Total") %>%
        filter(region=="World",
               scenario%in%s.list) %>% mutate(full.model.name = model) %>%
        simplify_model_names() %>%
        filter(grepl(model, pattern=m, fixed=T))
      if (nrow(p.data.model)==0){
        next # skip this iteration if there's no model data (e.g. no alternative scenarios submitted)
      }
      p.data.model <- p.data.model %>%
        iamc_region_keep_one_level(-1) %>%
        iamc_variable_keep_one_level(-1)


      p.emissions.by.model.sector.global <- ggplot(
        p.data.model,
        aes(x=year,y=value)
      ) +
        facet_wrap(unit~variable, scales="free_y") +

        # IAM
        geom_line(aes(colour=scenario,
                      linetype=full.model.name,
                      group=interaction(full.model.name,scenario,region,variable))) +

        # EDGAR
        geom_line(
          data = p.data.hist.edgar,
          colour = "darkgrey"
        ) +
        geom_point(
          data = p.data.hist.edgar %>% filter(year%in%years.hist.to.highlight),
          colour = "darkgrey",
          aes(shape = model)
        ) +

        # CEDS-GFED
        geom_line(
          data = p.data.hist,
          colour = "black"
        ) +
        geom_point(
          data = p.data.hist %>% filter(year%in%years.hist.to.highlight),
          colour = "black",
          aes(shape = model)
        ) +
        theme_jsk() +
        ylab(NULL) + xlab(NULL) +
        labs(
          title = m,
          subtitle = paste0("Scenario set: ", scenario.set)
        ) +
        mark_history()


      dir.create(here("data", "data_vetting", "figures", "per-model", m))
      save_ggplot(
        p = p.emissions.by.model.sector.global,
        h = 500,
        w = 500,
        format = "pdf",
        f = here("data", "data_vetting", "figures", "per-model", m, paste0("emissions_global_totals", "_", scenario.set) )
      )


    }
  }
}

# Regional ----

plot_historical_regional_sector <- function(df, model.list.simple.for.plotting=model.list.simple,
                                            # sector.list=sector.list,
                                            years.hist.to.highlight = c(1990,1995,2000,2010,2015,2020,2023)){

  # regional
  for (m in model.list.simple.for.plotting){

    dir.create(here("data", "data_vetting", "figures", "per-model", m))

    unique.sectors <- sector.list %>% pull(sector) %>% unique()
    excluded.sectors <- c("International Shipping", "Aircraft")
    unique.sectors.regional <- unique.sectors %>% (\(x) setdiff(x, excluded.sectors))()

    for (s in unique.sectors.regional){

      # for (s in c("Total") ){
      for (scenario.set in c("main", "alternative")){
        if (scenario.set=="main") {s.list <- scenario.list.main} else {s.list <- scenario.list.alternative}
        p.data.model <- df %>%
          filter(scenario%in%s.list)
        if (nrow(p.data.model)==0){
          next # skip this iteration if there's no model data (e.g. no alternative scenarios submitted)
        }
        p.data.model <- p.data.model %>%
          left_join(sector.list) %>% filter(sector==s) %>%
          mutate(full.model.name = model) %>%
          simplify_model_names() %>%
          filter_region_includes(m) %>%
          iamc_region_keep_one_level(-1)
        p.data.hist <- hist.regional %>% filter_region_includes(m) %>%
          left_join(sector.list) %>% filter(sector==s) %>%
          iamc_region_keep_one_level(-1)
        p.data.hist.edgar <- hist.regional.edgar %>% filter_region_includes(m) %>%
          left_join(sector.list) %>% filter(sector==s) %>%
          iamc_region_keep_one_level(-1)

        if (s == "Total"){
          p.data.hist <- p.data.hist %>%
            bind_rows(
              # add a combined total
              p.data.hist %>% reframe(model="CEDS+GFED", value=sum(value), .by = c("scenario","region", "variable", "unit", "year", "sector"))
            )
        }

        if (nrow(p.data.model)+nrow(p.data.hist)>0){
          # plot only if there is data to plot (e.g. no regional data for international shipping / aircraft)

          p.emissions.by.model.sector <- ggplot(
            p.data.model,
            aes(x=year,y=value)
          ) +
            facet_grid(unit~region, scales="free_y") +

            # IAM data
            geom_line(aes(colour=scenario,
                          linetype=full.model.name,
                          group=interaction(model,full.model.name,scenario,region,variable))) +

            # EDGAR
            geom_line(
              data = p.data.hist.edgar,
              colour = "darkgrey"
            ) +
            geom_point(
              data = p.data.hist.edgar %>% filter(year%in%years.hist.to.highlight),
              colour = "darkgrey",
              aes(shape = model)
            ) +

            # history
            geom_line(
              data = p.data.hist,
              colour = "black",
              aes(group=interaction(model,scenario,region,variable))
            ) +
            geom_point(
              data = p.data.hist %>% filter(year%in%years.hist.to.highlight),
              colour = "black",
              aes(shape = model)
            ) +
            theme_jsk() +
            ylab(NULL) + xlab(NULL) +
            labs(
              title = m,
              subtitle = paste0(s, "\n", "Scenario set: ", scenario.set)
            )



          save_ggplot(
            p = p.emissions.by.model.sector,
            h = 500,
            w = 500,
            format = "pdf",
            f = here("data", "data_vetting", "figures", "per-model", m, paste0("emissions_", s, "_", scenario.set) )
          )

        } else {
          print(paste0("Skipping non-existing regional emissions for ", s))
        }

      }
    }
  }

}





# Combine PDFs ----
plot_historical_combine_pdf <- function(model.list.that.has.been.plotted=model.list.simple){
  # combine all in one pdf
  for (m in model.list.that.has.been.plotted){
    path.model.out <- here("data", "data_vetting", "figures", "vetting-historical-plots") #m)
    path.model.in <- here("data", "data_vetting", "figures", "per-model", m)

    FILES.pdf <- file.path(path.model.in, dir(path.model.in, pattern = "*.pdf"))  # get file names

    OUT_FILE_NAME_MAIN <- file.path(path.model.out, paste0("vetting_plots_",m,"_main_",Sys.Date(),".pdf"))
    OUT_FILE_NAME_ALT <- file.path(path.model.out, paste0("vetting_plots_",m,"_alternative_",Sys.Date(),".pdf"))

    qpdf::pdf_combine(
      input = FILES.pdf[((grepl(FILES.pdf, pattern="_main",fixed=T))&!(grepl(FILES.pdf, pattern=OUT_FILE_NAME_MAIN,fixed=T)))],
      output = OUT_FILE_NAME_MAIN
    )

    qpdf::pdf_combine(
      input = FILES.pdf[((grepl(FILES.pdf, pattern="_alternative",fixed=T))&!(grepl(FILES.pdf, pattern=OUT_FILE_NAME_ALT,fixed=T)))],
      output = OUT_FILE_NAME_ALT
    )

  }
}


# Run code ----

## Visualisation choices ----
STARTYEAR <- 2025

plot.model.colors <- c(
  "AIM" = "#4D5CAD",
  "COFFEE" = "#69BA7F",
  "GCAM" = "#759EA8",
  "IMAGE" = "#868367",
  "MESSAGE" = "#892F71",
  "REMIND" = "#facb1e",
  "WITCH" = "#fb6a4a")

## Variables used in plotting ----
sector.list <- hist.national %>% mutate(variable_copy=variable) %>%  iamc_variable_keep_one_level(-1) %>% distinct(variable_copy,variable) %>% rename(sector=variable) %>% rename(variable=variable_copy)
variable.list <- hist.national %>% distinct(variable)
sector.species.list <- hist.national %>% distinct(variable) %>%
  add_sector_and_species_columns() %>%
  distinct(sector,species)

## Plotting runs ----
plot_historical_global_total(scenarios_harmonization)
plot_historical_global_sector(scenarios_harmonization)
plot_historical_regional_sector(scenarios_harmonization)
plot_historical_combine_pdf()
