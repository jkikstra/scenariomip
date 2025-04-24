EMISSIONS.SPECIES.SECTORAL <- c(
  "CO2", "BC", "CH4", "CO", "NH3", "N2O", "NOx", "OC", "Sulfur", "VOC"
)

INDUSTRY.SECTOR.VARIABLES <- c(
  "Energy|Demand|Industry",
  "Energy|Demand|Other Sector",
  "Industrial Processes",
  "Other",
  "Other Capture and Removal"
)
AIRCRAFT.SECTOR.VARIABLES <- c(
  "Energy|Demand|Bunkers|International Aviation",
  "Energy|Demand|Transportation|Domestic Aviation"
)
AGRICULTURE.SECTOR.VARIABLES <- c(
  "AFOLU|Agriculture",
  "AFOLU|Land|Harvested Wood Products",
  "AFOLU|Land|Land Use and Land-Use Change",
  "AFOLU|Land|Other",
  "AFOLU|Land|Wetlands"
)

SECTOR.VARIABLES.MAPPING.ONE.ON.ONE <- c(
  "Energy|Supply",
  "Energy|Demand|Bunkers|International Shipping",
  "Energy|Demand|Residential and Commercial and AFOFI",
  "Product Use",
  "AFOLU|Agricultural Waste Burning",
  "AFOLU|Land|Fires|Forest Burning",
  "AFOLU|Land|Fires|Grassland Burning",
  "AFOLU|Land|Fires|Peat Burning",
  "Waste"
)

SECTOR.VARIABLES.ALL <- c(
  INDUSTRY.SECTOR.VARIABLES,
  setdiff(AIRCRAFT.SECTOR.VARIABLES, "Energy|Demand|Transportation|Domestic Aviation"),
  AGRICULTURE.SECTOR.VARIABLES,
  "Energy|Demand|Transportation",
  SECTOR.VARIABLES.MAPPING.ONE.ON.ONE
)
