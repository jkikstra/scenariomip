# install.packages("ncdf4")
# install.packages("terra")
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("ggspatial")
library(ncdf4)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

file_path <-
  here(
    "..","concordia",
    "results",
    "config_cmip7_v0_testing_ukesm_remind",
    "try1_rescue_utils",
    "BC-em-anthro_input4MIPs_emissions_RESCUE_IIASA-PIK-REMIND-MAgPIE-3.5-4.10-SSP1---Very-Low-Emissions_gn_201501-210012.nc"
  )
variable_name <- "BC_em_anthro_sector=4_1"  # Replace with the variable you want to plot

# Read data using terra
raster_stack <- terra::rast(file_path)
names(raster_stack)  # Inspect to get correct name if needed

# Extract and cap variable ------------------------------------------------
data_raster <- raster_stack[[variable_name]]

data_df <- as.data.frame(data_raster, xy = TRUE, na.rm = TRUE)
colnames(data_df)[3] <- "value"

cap <- quantile(data_df$value, 0.99, na.rm = TRUE)

data_df <- data_df |>
  mutate(value_capped = pmin(value, cap)) %>%
  mutate_cond(value_capped==0, value_capped=NA)

# Convert raster data to sf points and reproject --------------------------
data_sf <- st_as_sf(data_df, coords = c("x", "y"), crs = crs(data_raster))
data_robin <- st_transform(data_sf, crs = "+proj=robin")

# Get land borders --------------------------------------------------------
# Load and project land borders -------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  st_transform(crs = "+proj=robin")

# Plot --------------------------------------------------------------------
p.grid <- ggplot() +
  geom_sf(
    data = data_robin,
    aes(color = value_capped),
    size = 0.01
  ) +
  geom_sf(
    data = world,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  scale_color_viridis_c(
    option = "plasma",
    name = paste(variable_name, "(≤99th percentile)")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", size = 0.2),
    # panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 8),
    # legend.position = "bottom"
    legend.position = "none"
  ) +
  labs(
    title = "Example of a spatial distribution of emissions",
    subtitle = "CO2 emissions, Transportation",
    x = NULL,
    y = NULL
  )

# p.grid

save_ggplot(
  p = p.grid,
  f = here("figures", "maps", "grid"),
  h = 150,
  w = 200,
  bg = 'white', #format = "png",
  unit = "mm"
)
