# Install reticulate package if not already installed
if (!require("reticulate")) install.packages("reticulate")

# Import the reticulate package
library(reticulate)
# Import some R packages
library(here)
here::i_am("scenariomip.Rproj")
source(here("R","utils.R"))
library(vroom)
library(tidyverse)

# Configure reticulate to use the correct Python environment
# use_python("C:\\Users\\kikstra\\AppData\\Local\\miniconda3\\envs\\pyam")  # Adjust this path to your Python environment if needed
use_python("C:\\Users\\zaini\\AppData\\Local\\miniconda3\\envs\\pyam")  # Adjust this path to your Python environment if needed

INSTALL_PACKAGES <- F

if (INSTALL_PACKAGES){
# # Install Python packages if needed
# py_install("pyam-iamc", pip = TRUE)
py_run_string("
import pyam

conn = pyam.iiasa.Connection()
print(conn.valid_connections)

conn_ssp = pyam.iiasa.Connection('ssp_submission')

print(conn_ssp.models())
print(conn_ssp.scenarios())

")
}

# All model-scenario combinations; by properties ----

# Define the Python code to retrieve the scenario
py_run_string("
import pyam
import os

# Establish connection to the legacy pyam platform 'ssp-submission'
pyam.iiasa.Connection('ssp_submission')
conn_ssp = pyam.iiasa.Connection('ssp_submission')
props = conn_ssp.properties().reset_index()

# Retrieve the desired scenarios (download everything; loop over model-scenario combinations to not blow up RAM)
for index, (m, s) in enumerate(zip(props['model'], props['scenario'])):

  # N.B. if the code breaks somewhere, and you want to continue after the first N scenarios, uncomment the below and replace 96 with N
  # if index < 96:
  #   continue

  # Print what scenario will be downloaded
  ms = m + ' (' + s + ')'
  print(f'Downloading {ms} now...')

  # Download the data for this specific model and scenario
  df = pyam.read_iiasa(
      'ssp_submission',
      model=m,
      scenario=s
  )

  # Write out the scenario data
  output_file = 'scenarios_scenariomip_' + m + '_' + s + '.csv'
  df.to_csv(
      os.path.join('data','downloading_iters',output_file)
  )
  print(f'Data has been written to {output_file}')

")

# Recombine the data files from each model into one file
SCENARIO.FILES.FOLDER <- here("data", "downloading_iters")
FILES.csv <- file.path(SCENARIO.FILES.FOLDER, dir(SCENARIO.FILES.FOLDER, pattern = "*.csv"))  # get file names
FILES.csv <- FILES.csv[!grepl(FILES.csv, pattern="MESSAGEix-GLOBIOM 2.1-M-R12",fixed=T)]
scenarios_csv <- FILES.csv %>%
  map(~ (load_csv_iamc(.) %>% iamc_wide_to_long() %>% filter(year<=2100) ) ) %>%
  reduce(rbind) %>%
  drop_na() %>%
  arrange(Model, Scenario, Region, Unit, year) %>%
  iamc_long_to_wide()
iamc_cols <- c("Model", "Scenario", "Region", "Variable", "Unit")
df_cols <- scenarios_csv %>% colnames() %>% sort
year_cols_ordered <- setdiff(df_cols, iamc_cols)
sorted_iamc <- c(iamc_cols, year_cols_ordered)
scenarios_csv_o <- scenarios_csv %>% select(all_of(sorted_iamc))

# delete some unnecessary data (or data that otherwise needs to be removed)
scenarios_csv_o <- scenarios_csv_o %>% filter(
  # !(Model=="AIM 3.0" & Scenario=="SSP1 - High Emissions"),
  # !(Model=="AIM 3.0" & Scenario=="SSP2 - High Emissions"),
  !(Model=="MESSAGEix-GLOBIOM 2.1-M-R12")
) %>%
  select(-c(`2105`,`2110`,`2115`,`2120`,`2125`,`2130`,`2135`,`2140`,`2145`,`2150`)) %>%
  colnames()

write_delim(
  x = scenarios_csv_o,
  file = here("data", paste0("scenarios_scenariomip_allmodels_", Sys.Date(),"_b.csv")),
  delim = ","
)

scenarios_csv_message <- scenarios_csv_o %>% filter(grepl(Model, pattern="MESSAGE", fixed=T))
write_delim(
  x = scenarios_csv_message,
  file = here("data", paste0("scenarios_scenariomip_allmodels_", Sys.Date(),"-message_b.csv")),
  delim = ","
)

# FLOORSPACE MESSAGE
floor_message <- scenarios_csv_message %>% filter(grepl(x=Variable, pattern="Floor Space", fixed=T))
write_delim(
  x = floor_message,
  file = here("data", paste0("scenarios_floorspace_", Sys.Date(),"-message.csv")),
  delim = ","
)
