import pyam
import os
 
df = pyam.read_ixmp4(
    platform="justmip-dev",  
    model="REMIND-MAgPIE 3.6-4.13",  
    scenario="SSP1_800f_dls1",          
    variable="*",          
    region="*",             
)

# output_dir = os.path.join('data', 'justmip', 'downloading_iters')
# output_file = "justmip-IMAGE-SSP2.csv"
# output_path = os.path.join(output_dir, output_file)
# df.data.to_csv(output_path, index=False)


df.data.to_csv("C:/Users/zaini/Documents/GitHub/scenariomip/data/justmip/justmip-REMIND-MAgPIE_3.6-4.13_SSP1_800f_dls1_2026_05_04.csv", index=False)



# import os
# import re
# import pyam

# PLATFORM = "justmip-dev"
# MODEL = "REMIND-MAgPIE 3.6-4.13"
# SCENARIO = "SSP2_800f"

# output_dir = r"C:\Users\zaini\Documents\GitHub\scenariomip\data\justmip\downloading_iters"
# os.makedirs(output_dir, exist_ok=True)


# def safe_filename(text):
#     """Make region names safe for Windows filenames."""
#     return re.sub(r'[\\/*?:"<>|]', "_", text)


# # Step 1: get the list of regions.
# # This assumes that downloading a small subset is possible.
# # Pick one variable that exists broadly in the database.
# probe = pyam.read_ixmp4(
#     platform=PLATFORM,
#     model=MODEL,
#     scenario=SCENARIO,
#     variable="*",
#     region="*",
#     year=2020,      # use one year only to keep this small
# )

# regions = sorted(probe.data["region"].unique())

# print(f"Found {len(regions)} regions:")
# for r in regions:
#     print(" -", r)


# # Step 2: download and save one region at a time.
# for region in regions:
#     print(f"\nDownloading region: {region}")

#     try:
#         df = pyam.read_ixmp4(
#             platform=PLATFORM,
#             model=MODEL,
#             scenario=SCENARIO,
#             variable="*",
#             region=region,
#         )

#         filename = f"justmip_REMIND-MAgPIE-3.6-4.13_SSP2_800f_{safe_filename(region)}.csv"
#         output_path = os.path.join(output_dir, filename)

#         df.data.to_csv(output_path, index=False)

#         print(f"Saved: {output_path}")

#     except Exception as e:
#         print(f"FAILED for region {region}: {e}")