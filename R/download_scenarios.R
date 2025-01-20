# Install reticulate package if not already installed
if (!require("reticulate")) install.packages("reticulate")

# Import the reticulate package
library(reticulate)
# Import some R packages
library(here)
here::i_am("vetting_iam.Rproj")
source(here("R","utils.R"))
library(vroom)
library(tidyverse)

# Configure reticulate to use the correct Python environment
use_python("C:\\Users\\kikstra\\AppData\\Local\\miniconda3\\envs\\pyam")  # Adjust this path to your Python environment if needed

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

# All models; by model ----

# Define the Python code to retrieve the scenario
py_run_string("
import pyam
import os

# Establish connection to the legacy pyam platform 'ssp-submission'
pyam.iiasa.Connection('ssp_submission')
conn_ssp = pyam.iiasa.Connection('ssp_submission')
models = conn_ssp.models()

# Retrieve the desired scenarios (download everything; loop over model-scenario combinations to not blow up RAM)
for m in models:
  df = pyam.read_iiasa(
      'ssp_submission',
      model=m
  )

  # Write out the scenario data
  output_file = 'scenarios_scenariomip' + m + '.csv'
  df.to_csv(
      os.path.join('data','downloading_models',output_file)
  )
  print(f'Data has been written to {output_file}')

")

# Recombine the data files from each model into one file
SCENARIO.FILES.FOLDER <- here("data", "downloading_models")
FILES.csv <- file.path(SCENARIO.FILES.FOLDER, dir(SCENARIO.FILES.FOLDER, pattern = "*.csv"))  # get file names
scenarios_csv <- FILES.csv %>%
  map(~ (load_csv_iamc(.) %>% iamc_wide_to_long()) ) %>%
  reduce(rbind) %>%
  drop_na() %>%
  arrange(Model, Scenario, Region, Unit, year) %>%
  iamc_long_to_wide()
iamc_cols <- c("Model", "Scenario", "Region", "Variable", "Unit")
df_cols <- scenarios_csv %>% colnames() %>% sort
year_cols_ordered <- setdiff(df_cols, iamc_cols)
sorted_iamc <- c(iamc_cols, year_cols_ordered)
scenarios_csv_o <- scenarios_csv %>% select(all_of(sorted_iamc))

write_delim(
  x = scenarios_csv,
  file = here("data", paste0("scenarios_scenariomip_allmodels_", Sys.Date(),".csv")),
  delim = ","
)


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

  # if index < 96:
  #   continue

  # Print what scenario will be downloaded
  ms = m + ' (' + s + ')'
  print(f'Downloading {ms} now...')

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
scenarios_csv <- FILES.csv %>%
  map(~ (load_csv_iamc(.) %>% iamc_wide_to_long()) ) %>%
  reduce(rbind) %>%
  drop_na() %>%
  arrange(Model, Scenario, Region, Unit, year) %>%
  iamc_long_to_wide()
iamc_cols <- c("Model", "Scenario", "Region", "Variable", "Unit")
df_cols <- scenarios_csv %>% colnames() %>% sort
year_cols_ordered <- setdiff(df_cols, iamc_cols)
sorted_iamc <- c(iamc_cols, year_cols_ordered)
scenarios_csv_o <- scenarios_csv %>% select(all_of(sorted_iamc))

write_delim(
  x = scenarios_csv,
  file = here("data", paste0("scenarios_scenariomip_allmodels_", Sys.Date(),".csv")),
  delim = ","
)

scenarios_csv_message <- scenarios_csv %>% filter(grepl(Model, pattern="MESSAGE", fixed=T))
write_delim(
  x = scenarios_csv_message,
  file = here("data", paste0("scenarios_scenariomip_allmodels_", Sys.Date(),"-message.csv")),
  delim = ","
)

# Only MESSAGE ----

# Define the Python code to retrieve the scenario (only retrieve MESSAGE)
py_run_string("
import pyam
import os

# Establish connection to the legacy pyam platform 'ssp-submission'
pyam.iiasa.Connection('ssp_submission')
conn_ssp = pyam.iiasa.Connection('ssp_submission')

# Retrieve the desired scenarios (download everything)
df = pyam.read_iiasa(
    'ssp_submission',
    model='MESSAGEix-GLOBIOM 2.1-M-R12'
)

# Write out the scenario data
output_file = 'scenarios_scenariomip' + 'MESSAGE' + '.csv'
df.to_csv(
    os.path.join('data',output_file)
)
print(f'Data has been written to {output_file}')

")
