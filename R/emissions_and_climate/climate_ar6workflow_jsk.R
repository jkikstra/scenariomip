
# Load ----
## infilled ----
ar6like.infilled.emissions <- load_excel_iamc(file.path(CLIMATE.DATA.LOCATION, AR6.CLIMATE.FILE.JSK)) %>%
  filter(str_starts(Variable, "AR6 climate diagnostics|Infilled|Emissions")) %>%
  iamc_wide_to_long(upper.to.lower = T)

## temps ----
ar6like.temp50 <- load_excel_iamc(file.path(CLIMATE.DATA.LOCATION, AR6.CLIMATE.FILE.JSK)) %>%
  filter(Variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile") %>%
  iamc_wide_to_long(upper.to.lower = T)

p.data.temps <- ar6like.temp50 %>% add_scenariomip_targets_to_IAM_scenarios() %>% simplify_model_names(keep.full.model.name = T) %>% add_ssp_basis_to_IAM_scenarios()


# Peak and 2100 ----

p.data.temps.2100 <- p.data.temps %>% filter(year==2100)

p.data.temps.peak <- p.data.temps %>% reframe(
  value = max(value),
  .by = c("model", "scenario", "region", "variable", "unit", "target", "full.model.name", "ssp")
)

write_delim(x = p.data.temps.peak %>% rename(peak=value) %>%
              left_join(p.data.temps.2100 %>% rename(`2100`=value)),
            file = here("data", "data_vetting", "figures", "climate", "ar6_temps_peak_2100.csv"),
            delim = ",")


# Plot ----
# ...


## All ----



p.data.temps$target <- factor(p.data.temps$target, levels=c('VLLO','VLHO','L','ML', 'M', 'H'))

p.temps <- ggplot(
  p.data.temps %>% filter(scenario %in% scenario.list.main),
  aes(x=year,y=value,colour=target,
      group=interaction(full.model.name,scenario,region,variable))
) +
  facet_grid(model~target, scales = "free") +
  mark_history() +
  # geom_texthline(yintercept = 1.5, label="1.5C", linetype="dashed", colour="black",
  #                hjust=0.05) +
  geom_hline(yintercept = 1.5, linetype="dashed", colour="black") +
  geom_line(
    aes(y=value,
        colour=model,
        linetype=ssp,
        group=interaction(model,scenario,variable)),
    linewidth=0.5
  ) +
  geom_point(
    data=. %>% filter(year%in%seq(1990,2100,10)),
    aes(y=value,
        colour=model,
        shape=ssp),
    size=2
  ) +
  geom_text(data=p.data.temps.2100 %>%
              filter(scenario %in% scenario.list.main),
            aes(label=round(value,digits = 1),
                colour=target,
                x=2110)) +
  geom_text(data=p.data.temps.peak %>% left_join(p.data.temps.2100 %>% rename(value.2100=value)) %>%
              filter(scenario %in% scenario.list.main),
            aes(label=paste0("(",round(value,digits = 1),")"),
                colour=target,
                y=value.2100,
                x=2135)) +
  scale_color_manual(values = plot.model.colors) +
  scale_linetype_manual(values = plot.ssp.linetypes) +
  theme_jsk() +
  ylab("degreeC above pre-industrial") +
  labs(title = "AR6 workflow with global harmonization in 2015",
       subtitle = "2100 (peak) temperatures",
       caption = paste0("Download date scenarios: ", DOWNLOAD.DATE) ) +
  coord_cartesian(xlim = c(1995,2140))

save_ggplot(
  p = p.temps,
  h = 400,
  w = 450,
  f = here("data", "data_vetting", "figures", "climate", "ar6_temps_all_models_targets_main")
)


## Per model ----
for (m in model.list.simple){
  if (nrow(
    p.data.temps %>% filter(scenario %in% scenario.list.main,
                            model==m)
  )==0){
    next
  }
  p.temps <- ggplot(
    p.data.temps %>% filter(scenario %in% scenario.list.main,
                            model==m),
    aes(x=year,y=value,colour=target,
        group=interaction(full.model.name,scenario,region,variable))
  ) +
    facet_grid(ssp~target, scales = "free") +
    mark_history() +
    # geom_texthline(yintercept = 1.5, label="1.5C", linetype="dashed", colour="black",
    #                hjust=0.05) +
    geom_hline(yintercept = 1.5, linetype="dashed", colour="black") +
    geom_line(
      aes(y=value,
          colour=model,
          linetype=ssp,
          group=interaction(model,scenario,variable)),
      linewidth=0.5
    ) +
    geom_point(
      data=. %>% filter(year%in%seq(1990,2100,10)),
      aes(y=value,
          colour=model,
          shape=ssp),
      size=2
    ) +
    geom_text(data=p.data.temps.2100 %>%
                filter(scenario %in% scenario.list.main,
                       model==m),
              aes(label=round(value,digits = 1),
                  colour=target,
                  x=2110)) +
    geom_text(data=p.data.temps.peak %>% left_join(p.data.temps.2100 %>% rename(value.2100=value)) %>%
                filter(scenario %in% scenario.list.main,
                       model==m),
              aes(label=paste0("(",round(value,digits = 1),")"),
                  colour=target,
                  y=value.2100,
                  x=2135)) +
    scale_color_manual(values = plot.model.colors) +
    scale_linetype_manual(values = plot.ssp.linetypes) +
    theme_jsk() +
    ylab("degreeC above pre-industrial") +
    labs(title = paste0(m, ": AR6 workflow with global harmonization in 2015"),
         subtitle = "2100 (peak) temperatures",
         caption = paste0("Download date scenarios: ", DOWNLOAD.DATE) ) +
    coord_cartesian(xlim = c(1995,2140))

  save_ggplot(
    p = p.temps,
    h = 200,
    w = 300,
    f = here("data", "data_vetting", "figures", "climate", paste0("ar6_temps_",m,"_targets_main"))
  )
}


## Per target ----
