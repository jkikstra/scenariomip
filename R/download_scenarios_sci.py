import pyam
import ixmp4

platform = ixmp4.Platform("scenariocompass")

analysis_variables = [
    "GDP|PPP",
    "Emissions|Kyoto Gases",
    "Emissions|CO2"
]

df = pyam.read_ixmp4(
    platform="scenariocompass",  
    model="COFFEE 1.5",  
    scenario="*",          
    variable="*",          
    region="World",             
)

# df = pyam.read_ixmp4(platform, variable=analysis_variables, region="World")

# df.to_excel("../data/sci-dev-ar6-table-update.xlsx")
df.data.to_csv("C:/Users/zaini/Documents/GitHub/scenariomip/data/scenariocompass/allscenarios_world.csv", index=False)
