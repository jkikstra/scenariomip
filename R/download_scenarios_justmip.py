import pyam
import ixmp4
import os

platform = ixmp4.Platform("justmip-dev")
props = platform.runs.tabulate()
print(props)

# Ensure output directory exists
output_dir = os.path.join('data', 'justmip', 'downloading_iters')
os.makedirs(output_dir, exist_ok=True)

# If you need to retrieve data from a specific model:
# df_model = props[props['model'] == "AIM 3.0"]
# for index, (m, s) in enumerate(zip(df_model['model'], df_model['scenario'])):

# Retrieve the desired scenarios (download everything; loop over model-scenario combinations to not blow up RAM)
for index, (m, s) in enumerate(zip(props['model'], props['scenario'])):

  output_file = 'justmip_' + m + '_' + s + '.csv'
  output_path = os.path.join(output_dir, output_file)

  # Skip if file already exists
  if os.path.exists(output_path):
      print(f"✅ Skipping {output_file} (already exists)")
      continue
  
  ms = f"{m} ({s})"
  print(f"⬇️ Downloading {ms}...")

  try:
      df = pyam.read_ixmp4('justmip-dev', model=m, scenario=s)
      if "type" not in df.meta.columns:
        raise ValueError("Missing 'type' column")
      df.to_csv(output_path)    # save table as a csv
      print(f"✅ Data saved to {output_file}")
  except Exception as e:
      print(f"❌ Error downloading {ms}: {e}")

