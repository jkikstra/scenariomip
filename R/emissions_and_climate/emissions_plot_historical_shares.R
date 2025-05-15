#' TODO
#' - [ ] add burning sectors to this overview
#' - [ ] split CEDS IAMregion into model-specific files

# step 1: shares by sector (global) - CEDS
global.ceds <- hist.data.iam.regions %>%
  filter(year==HARMONIZATION.YEAR,
         grepl(model,pattern="CEDS",fixed=T),
         region=="World") %>%
  add_sector_and_species_columns()
global.ceds.sector.shares <- global.ceds %>% filter(sector!="Total") %>%
  left_join(global.ceds %>% filter(sector=="Total") %>% select(-variable,-sector,-scenario) %>% rename(total=value)) %>%
  mutate(share=value/total)

p.global.ceds.sector.shares <- ggplot(
  global.ceds.sector.shares,
  aes(x=species,y=share,fill=sector)
) +
  geom_col() +
  theme_jsk() +
  labs(
    title = paste0("Share of global emissions in CEDS in ", as.character(HARMONIZATION.YEAR)),
    caption = paste0("Version: ", global.ceds %>% pull(model) %>% unique(),
                     "\nDoes not include burning emissions.")
  )
p.global.ceds.sector.shares
path.model.out <- here("data", "data_vetting", "figures", "vetting-historical-plots")
save_ggplot(
  p = p.global.ceds.sector.shares,
  h = 200,
  w = 300,
  format = "pdf",
  f = file.path(path.model.out, paste0("CEDS_sector_shares_", as.character(HARMONIZATION.YEAR)) )
)
write_delim(
  x = global.ceds.sector.shares,
  file = file.path(path.model.out, paste0("CEDS_sector_shares_", as.character(HARMONIZATION.YEAR), ".csv") ),
  delim = ","
)


# step 2: shares by sector (countries) - CEDS
iamregion.ceds <- hist.data.iam.regions %>%
  filter(year==HARMONIZATION.YEAR,
         grepl(model,pattern="CEDS",fixed=T),
         region!="World") %>%
  add_sector_and_species_columns()
iamregion.ceds.sector.shares <- iamregion.ceds %>% filter(sector!="Total") %>%
  left_join(iamregion.ceds %>% filter(sector=="Total") %>% select(-variable,-sector,-scenario) %>% rename(total=value)) %>%
  mutate(share=value/total)

p.iamregion.ceds.sector.shares <- ggplot(
  iamregion.ceds.sector.shares,
  aes(x=species,y=share,fill=sector)
) +
  facet_wrap(~region, nrow = 3,
             scales = "free_x", strip.position = "top") +
  geom_col() +
  theme_jsk() +
  theme(strip.placement = "outside") +
  labs(
    title = paste0("Share of IAM-region emissions in CEDS in ", as.character(HARMONIZATION.YEAR)),
    caption = iamregion.ceds %>% pull(model) %>% unique()
  )
# p.iamregion.ceds.sector.shares
path.model.out <- here("data", "data_vetting", "figures", "vetting-historical-plots")
save_ggplot(
  p = p.iamregion.ceds.sector.shares,
  h = 200,
  w = 5000,
  format = "pdf",
  f = file.path(path.model.out, paste0("CEDS_IAMregion_sector_shares_", as.character(HARMONIZATION.YEAR)) ),
  limitsize = FALSE
)
