# Install reticulate package if not already installed
if (!require("reticulate")) install.packages("reticulate")

# Import the reticulate package
library(reticulate)

# Configure reticulate to use the correct Python environment
use_python("C:\\Users\\kikstra\\AppData\\Local\\miniconda3\\envs\\pyam")  # Adjust this path to your Python environment if needed

# # Install Python packages if needed
# py_install("pyam-iamc", pip = TRUE)
py_run_string("
import pyam

conn = pyam.iiasa.Connection()
print(conn.valid_connections)

conn_ssp = pyam.iiasa.Connection('ssp_submission')

print(conn_ssp.models().head())
print(conn.meta_columns.head())

")

# Define the Python code to retrieve the scenario
py_run_string("
import pyam
import os

# Establish connection to the legacy pyam platform 'ssp-submission'
pyam.iiasa.Connection('ssp_submission')

# Retrieve the desired scenarios (download everything)
df = pyam.read_iiasa(
    'ssp_submission'
)

# Write out the scenario data
output_file = 'scenarios_scenariomip.csv'
df.to_csv(
    os.path.join('data',output_file)
)
print(f'Data has been written to {output_file}')

")
