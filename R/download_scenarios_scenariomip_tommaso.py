import pyam
import os
import pandas as pd
# import ixmp4 # at the moment this doesn't concern scenariomip platform, but only the new ones

# Ensure output directory exists
output_dir = os.path.join('data', 'downloading_iters')
os.makedirs(output_dir, exist_ok=True)

# Connect to IIASA database
conn_ssp = pyam.iiasa.Connection('ssp_submission')
props = conn_ssp.properties().reset_index()

# Loop through scenarios
for index, (m, s) in enumerate(zip(props['model'], props['scenario'])):
    # Use original naming convention
    output_file = 'scenarios_scenariomip_' + m + '_' + s + '.csv'
    output_path = os.path.join(output_dir, output_file)

    # Skip if file already exists
    if os.path.exists(output_path):
        print(f"✅ Skipping {output_file} (already exists)")
        continue

    ms = f"{m} ({s})"
    print(f"⬇️ Downloading {ms}...")

    try:
        df = pyam.read_iiasa('ssp_submission', model=m, scenario=s)
        df.to_csv(output_path)    # save table as a csv
        print(f"✅ Data saved to {output_file}")
    except Exception as e:
        print(f"❌ Error downloading {ms}: {e}")
        

# Simplified example:
# df = pyam.read_iiasa(
#   'ssp_submission',
#   model="MESSAGE*",
#   scenario='SSP2 - Low*',
#   region="World"
#   )

# output_file = 'justmip_data.csv'

# df.to_csv(
#     os.path.join('..', 'data','scenariomip','some_data.csv')
# )
