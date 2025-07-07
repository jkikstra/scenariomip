library("here")
library("tidyverse")
library("vroom")
library("readxl")
library("patchwork")
library("ggthemes")
library("ggsci")
library("testthat")
library("stringr")
library("ggthemes")
# install.packages("qpdf")
library("qpdf")
library("geomtextpath")


here::i_am("scenariomip.Rproj")

source(here("R","utils.R"))

# Notes ----
#' v1
#' - an example for Ken Caldeira 07.07.2025


# Where to save the figures? ----
path.figures <- here("figures", "ar6")

# Where is the data? ----
path.data.folder <- "C:/Users/kikstra/OneDrive - IIASA/_Other/Data/Scenario data/Scenario Databases/AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1"

data <- load_csv_iamc(file_path = file.path(path.data.folder, "AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv"), mode = "fast") %>%
  filter((grepl(Variable, pattern="Effective", fixed=T) | grepl(Variable, pattern="Temp", fixed=T)),
         grepl(Variable, pattern="50.0", fixed=T),
         grepl(Variable, pattern="MAGICC", fixed=T)) %>%
  iamc_wide_to_long(upper.to.lower = T)

meta <- read_excel(file.path(path.data.folder, "AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"),
                   sheet = "meta_Ch3vetted_withclimate") %>%
  select(Model,Scenario,Category, IMP_marker) %>% rename(model=Model,scenario=Scenario)

meta %>% distinct(IMP_marker)


# visualise some scenarios ----
IMP.CHOICES <- c('SP','Ren','ModAct','CurPol', 'Ren-2.0')
IMP.CHOICES <- c('SP','Ren','ModAct','CurPol')

rename_IPs <- function(df){
  df %>%
    mutate_cond(IMP_marker=="CurPol", IMP_marker="Current Policies (3.0C)") %>%
    mutate_cond(IMP_marker=="ModAct", IMP_marker="Moderate Action (2.7C)") %>%
    mutate_cond(IMP_marker=="Ren-2.0", IMP_marker="High Renewables (1.6C)") %>%
    mutate_cond(IMP_marker=="Ren", IMP_marker="High Renewables (1.4C)") %>%
    mutate_cond(IMP_marker=="SP", IMP_marker="Sustainable Development (1.2C)")
}


data.twoscen <- data %>% left_join(meta) %>% filter(IMP_marker %in% IMP.CHOICES)

data.temp <- data %>% left_join(meta) %>% filter(IMP_marker %in% IMP.CHOICES) %>%
  filter(
    variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"
  ) %>% rename_IPs()
data.temp %>% filter(year==2100)

data.twoscen.stack <- data.twoscen %>% rename_IPs() %>%
  filter(variable%in%c(
    "AR6 climate diagnostics|Effective Radiative Forcing|CO2|MAGICCv7.5.3|50.0th Percentile",
    "AR6 climate diagnostics|Effective Radiative Forcing|CH4|MAGICCv7.5.3|50.0th Percentile",
    "AR6 climate diagnostics|Effective Radiative Forcing|N2O|MAGICCv7.5.3|50.0th Percentile",
    "AR6 climate diagnostics|Effective Radiative Forcing|Aerosols|MAGICCv7.5.3|50.0th Percentile",
    "AR6 climate diagnostics|Effective Radiative Forcing|F-Gases|MAGICCv7.5.3|50.0th Percentile"
  )) %>%
  mutate(variable = substr(variable, nchar("AR6 climate diagnostics|Effective Radiative Forcing|")+1, nchar(variable))) %>%
  mutate(variable = substr(variable, 1, nchar(variable)-nchar("|MAGICCv7.5.3|50.0th Percentile")))
data.twoscen.total <- data.twoscen %>% rename_IPs() %>%
  filter(
    # variable == "AR6 climate diagnostics|Effective Radiative Forcing|MAGICCv7.5.3|50.0th Percentile"
    variable %in% c("AR6 climate diagnostics|Effective Radiative Forcing|Basket|Anthropogenic|MAGICCv7.5.3|50.0th Percentile")
  )



data.twoscen.total$IMP_marker <- factor(data.twoscen.total$IMP_marker, levels=c('Current Policies (3.0C)',
                                                                                'Moderate Action (2.7C)',
                                                                                'High Renewables (1.6C)',
                                                                                'High Renewables (1.4C)',
                                                                                'Sustainable Development (1.2C)'))
data.twoscen.stack$IMP_marker <- factor(data.twoscen.stack$IMP_marker, levels=c('Current Policies (3.0C)',
                                                                                'Moderate Action (2.7C)',
                                                                                'High Renewables (1.6C)',
                                                                                'High Renewables (1.4C)',
                                                                                'Sustainable Development (1.2C)'))
data.temp$IMP_marker <- factor(data.temp$IMP_marker, levels=c('Current Policies (3.0C)',
                                                                                'Moderate Action (2.7C)',
                                                                                'High Renewables (1.6C)',
                                                                                'High Renewables (1.4C)',
                                                                                'Sustainable Development (1.2C)'))


p.twoscen <- ggplot(
  data.twoscen.total,
  aes(x=year, y=value)
) +
  facet_grid(~IMP_marker) +
  mark_history(sy=2025) +
  geom_area(data=data.twoscen.stack %>% rename_IPs(),
            mapping=aes(fill=variable), alpha=1) +
  geom_textline(data = . %>% filter(IMP_marker=="Current Policies (3.0C)"),
    aes(label="Total human-induced forcing"), linewidth=1.1, linetype="dashed",
                 # hjust = "ymax",
                 fontface = 1.5,
                 text_smoothing = 50) +
  geom_line(data = . %>% filter(IMP_marker!="Current Policies (3.0C)"),
            linewidth=1.1, linetype="dashed") +
  theme_jsk() +
  scale_fill_jco() +
  labs(y=bquote(W/m^2),
       title = "Illustrative Pathways from the IPCC Sixth Assessment Report (2100 temperature in parentheses)",
       subtitle = "Effective radiative forcing contributions") +
  legend_column_wise(ncol=5) +
  scale_x_continuous(expand = c(0,0), breaks = c(2025,2050,2075,2100)) +
  theme(legend.title = element_blank())
# p.twoscen

p.temp <- ggplot(
  data.temp,
  aes(x=year, y=value)
) +
  facet_grid(~IMP_marker) +
  mark_history(sy=2025) +
  geom_line(linewidth=1.33, linetype="solid") +
  theme_jsk() +
  scale_fill_jco() +
  labs(y="Temperature (°C)",
       subtitle = "Temperature change",
       caption = "Using MAGICC climate emulator, median of distribution. Illustrative pathways are 'IMP-CurPol', 'IMP-ModAct', 'IMP-Ren', and 'IMP-SP'.\n\nBased on data from Kikstra et al. (2022), Geoscientific Model Development:\nThe IPCC Sixth Assessment Report WGIII climate assessment of mitigation pathways: from emissions to global temperatures.") +
  legend_column_wise(ncol=5) +
  scale_x_continuous(expand = c(0,0), breaks = c(2025,2050,2075,2100)) +
  theme(legend.title = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))
# p.temp

p.example <- (p.twoscen + p.temp) + plot_layout(
  design = "
  AAAA
  AAAA
  BBBB
  "
)

save_ggplot(
  p = p.example,
  f = file.path(path.figures, "example_two_illustrative_pathways_AR6"),
  h = 200,
  w = 250
)
