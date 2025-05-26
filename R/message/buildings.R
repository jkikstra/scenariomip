floorm <- vroom(here("data", "scenarios_scenariomip_allmodels_2025-05-25-message.csv")) %>%
  iamc_wide_to_long(upper.to.lower = T) %>%
  filter_includes("Floor")
popm <- vroom(here("data", "scenarios_scenariomip_allmodels_2025-05-25-message.csv")) %>%
  filter(Variable=="Population") %>%
  iamc_wide_to_long(upper.to.lower = T)
floor.pc <- floorm %>% bind_rows(popm) %>%
  to_per_capita()

ggplot(floor.pc %>% filter_region_includes("MESSAGE") %>% filter(year>=2020) %>% mutate(
  region = substr(region, start=nchar("MESSAGEix-GLOBIOM-GAINS 2.1-M-R12"), stop=nchar(region)),
  value = value * 1e9 / 1e6
), aes(x=year,y=value)) +
  mark_history(sy=2025) +
  facet_grid(variable~region) +
  geom_line(aes(colour=scenario, group=interaction(model,scenario,region,variable))) +
  theme_jsk() +
  labs(title = "Floor space per capita, ScenarioMIP, upload May 23rd")
