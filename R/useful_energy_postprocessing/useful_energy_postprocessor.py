# Generate useful energy with the FE-to-UE postprocessing tool

# This script is supposed to be run on IAM scenario data:
# - in csv format (NB NOT csv UTF-8)
# - in wide format (years as columns)
# - in IAMC format. Columns expected: Model, Scenario, Region, Variable, Unit, 2020, 2025, 2030, ...

# make sure the input IAM data file is in the same folder as this script

import pandas as pd
from pathlib import Path

# ---- CONFIG ----

# Input IAM data file (in same folder as script):
iam_data_filename = "scenarios_justmip_AIM_2026-03-16.csv"

# File where the coefficients are:
coeff_xlsx = "2020_12_10_Downscaling_Variables_list_with_ue_coeffs_cb.xlsx"
# Output file name containing IAM data with useful energy added:
out_wide = iam_data_filename.replace(".csv", "") + "_with_UE.csv"  


# ---- CALCULATIONS ----

# Canonical column names we expect to be id columns in wide CSV
ID_COLS_CANDIDATES = ["Model", "Scenario", "Region", "Variable", "Unit"]

def main():
    script_dir = Path(__file__).parent
    iam_csv = script_dir / iam_data_filename
    coeff_path = script_dir / coeff_xlsx

    # --- Read IAM data ---
    df = pd.read_csv(iam_csv, sep=None, engine="python")
    # Identify year-like columns: 4-digit numbers
    year_cols = [c for c in df.columns if str(c).isdigit() and len(str(c)) == 4]
    # Identify id columns present
    id_cols = [c for c in ID_COLS_CANDIDATES if c in df.columns]
    if not year_cols:
        raise ValueError("No year columns detected. If your file is already long, share a sample of headers.")
    if len(id_cols) < 4:
        raise ValueError(f"Could not find the usual id columns. Found: {id_cols}. "
                         "Make sure headers include Model, Scenario, Region, Variable, Unit.")

    # --- Pivot wide -> long ---
    long = df.melt(id_vars=id_cols, value_vars=year_cols,
                   var_name="year", value_name="value")
    long["year"] = pd.to_numeric(long["year"], errors="coerce")
    long["value"] = pd.to_numeric(long["value"], errors="coerce")

    # --- Read UE coefficients ---
    coeff = pd.read_excel(coeff_path)
    # Keep only needed columns, drop NAs
    coeff = coeff[["Final Energy", "UE coeff"]].copy()
    coeff = coeff[coeff["UE coeff"].notna()]
    # Align naming difference
    coeff["Final Energy"] = coeff["Final Energy"].str.replace(
        "Buildings", "Residential and Commercial", regex=False
    )

    # --- Compute UE on the long data ---
    # Keep only final-energy rows that have a coefficient
    fe = long[long["Variable"].isin(coeff["Final Energy"])].copy()
    fe = fe.merge(coeff, left_on="Variable", right_on="Final Energy", how="left")
    fe = fe[fe["UE coeff"].notna()].copy()

    fe["Variable_UE"] = fe["Variable"].str.replace("Final", "Useful", regex=False)
    fe["value_UE"] = fe["value"] * fe["UE coeff"]

    ue_long = fe[id_cols + ["year"]].copy()
    ue_long["Variable"] = fe["Variable_UE"]
    ue_long["value"] = fe["value_UE"]


    # Concatenate original (long) + UE
    out_long_df = pd.concat([long, ue_long], ignore_index=True)

    # Add sector aggregates
    out_long_df = add_sector_aggregates_long(out_long_df)

    # --- (Optional) Pivot back to wide, keeping the input layout ---
    out_wide_df = out_long_df.pivot_table(
        index=id_cols, columns="year", values="value", aggfunc="sum"
    ).reset_index()
    # Flatten MultiIndex columns if any
    out_wide_df.columns = [str(c) for c in out_wide_df.columns]
    out_wide_df.to_csv(script_dir / out_wide, index=False)
    print(f"[OK] Wrote scenario data with UE: {out_wide}")

# Calculate Useful Energy by sector as sum of all the subsectors (i.e. fuels)
def add_sector_aggregates_long(df):
    id_cols = ["Model", "Scenario", "Region", "Unit", "year"]
    # print(f"Detected ID columns: {id_cols}") # for debugging
    # Make sure types are numeric for summation
    df["value"] = pd.to_numeric(df["value"], errors="coerce")

    parents = [
        "Useful Energy|Industry",
        "Useful Energy|Transportation",
        "Useful Energy|Residential and Commercial",
    ]

    agg_rows = []

    # 1) Sector aggregates from their children (exclude the parent itself)
    for p in parents:
        mask_children = df["Variable"].str.startswith(p + "|")
        if mask_children.any():
            g = (
                df.loc[mask_children, id_cols + ["value"]]
                  .groupby(id_cols, as_index=False)["value"]
                  .sum()
            )
            g["Variable"] = p
            agg_rows.append(g[id_cols + ["Variable", "value"]])

    # 2) Total "Useful Energy" from all detailed children (exclude parents)
    # Children are things like 'Useful Energy|X|...'
    mask_children_any = df["Variable"].str.startswith("Useful Energy|") & df["Variable"].str.contains(r"\|")
    if mask_children_any.any():
        ue_total = (
            df.loc[mask_children_any, id_cols + ["value"]]
              .groupby(id_cols, as_index=False)["value"]
              .sum()
        )
        ue_total["Variable"] = "Useful Energy"
        agg_rows.append(ue_total[id_cols + ["Variable", "value"]])

    if agg_rows:
        agg_df = pd.concat(agg_rows, ignore_index=True)
        # Append and drop exact duplicates if they already existed
        df_out = pd.concat([df, agg_df], ignore_index=True).drop_duplicates(
            subset=["Model", "Scenario", "Region", "Variable", "Unit", "year"], keep="last"
        )
        return df_out
    return df

if __name__ == "__main__":
    main()





























# import pandas as pd
# from pathlib import Path

# # IAM data file name
# iam_data_filename = "scenarios_wellbeing_remind_ar6.csv" # Change filename if needed

# def main():
#     # Path to IAM data CSV (assumed in same folder as script)
#     script_dir = Path(__file__).parent
#     iam_csv = script_dir / iam_data_filename
#     coeff_xlsx = script_dir / "2020_12_10_Downscaling_Variables_list_with_ue_coeffs_cb.xlsx"

#     # Read IAM data
#     df = pd.read_csv(iam_csv)

#     # Read UE coefficients
# # Read UE coefficients
#     coeff = pd.read_excel(coeff_xlsx)
#     print("Columns in coeff after reading Excel:", coeff.columns)

#     coeff["Final Energy"] = coeff["Final Energy"].replace({"Buildings": "Residential and Commercial"}, regex=True)

#     # Filter IAM data for variables with coefficients
#     fe_df = df[df["Variable"].isin(coeff["Final Energy"])]

#     # Merge coefficients into IAM data
#     merged = fe_df.merge(coeff, left_on="Variable", right_on="Final Energy", how="left")

#     # Calculate useful energy (by multiplying final energy with coefficients)
#     merged["Useful Energy variable"] = merged["Variable"].str.replace("Final", "Useful", regex=True)
#     merged["Useful Energy value"] = merged["value"] * merged["UE coeff"]

#     # Prepare output dataframe
#     ue_df = merged.copy()
#     ue_df["variable"] = ue_df["Useful Energy variable"]
#     ue_df["value"] = ue_df["Useful Energy value"]
#     ue_df = ue_df.drop(["Useful Energy variable", "Useful Energy value", "UE coeff", "Final Energy"], axis=1)

#     # Concatenate original and useful energy data
#     out_df = pd.concat([df, ue_df], ignore_index=True)

#     # Save to CSV
#     out_csv = script_dir / "iam_data_with_useful_energy.csv"
#     out_df.to_csv(out_csv, index=False)
#     print(f"Saved output to {out_csv}")

# if __name__ == "__main__":
#     main()