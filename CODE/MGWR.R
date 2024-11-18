# Install and load required packages
install.packages(c("GWmodel"))
library(GWmodel)
library(sp)
library(sf)
library(ggplot2)

#Cargar datos
gdf = st_read("G:/My Drive/INVESTIGACION/PAPERS/ELABORACION/Modelo_Multiniveles/DATA/df_catchments_spatial.gpkg", quiet = TRUE)

#Para guardar datos
savefile <- "G:/My Drive/INVESTIGACION/PAPERS/ELABORACION/Modelo_Multiniveles/FIGURAS/"


# Ensure that there are no missing values in the required columns
gdf <- gdf[!is.na(gdf$lands_rec) & !is.na(gdf$elev_mean) & 
             !is.na(gdf$rel_mean) & !is.na(gdf$rainfallAnnual_mean) & 
             !is.na(gdf$area), ]

# Convert to an 'sf' object for compatibility
gdf_sf <- st_as_sf(gdf)

# Extract coordinates for GWR
coords <- st_coordinates(gdf_sf)

# Define the regression formula with offset for Area
# We use log(Area) as the offset to account for differing areas of mapping units
formula <- lands_rec ~ elev_mean + rel_mean + rainfallAnnual_mean + offset(log(area))

# Select the optimal bandwidth for GWPR
bandwidth <- bw.gwr(formula, data = gdf_sf)

# Print the selected bandwidth
print(paste("Selected Bandwidth:", bandwidth))

# Fit the Geographically Weighted Poisson Regression model
gwpr_model <- gwr.basic(formula, data = gdf_sf, coords = coords,
                        bandwidth = bandwidth, gweight = gwr.Gaussian,
                        family = "poisson", hatmatrix = TRUE)

# View a summary of the GWPR model
summary(gwpr_model)

# Extract local coefficients for visualization
local_coefficients <- gwpr_model$SDF

# Plot local coefficients for 'elev_mean'
spplot(local_coefficients, "elev_mean", main = "Local Coefficients for Elevation",
       col.regions = rev(terrain.colors(100)))

# Plot local coefficients for 'rel_mean'
spplot(local_coefficients, "rel_mean", main = "Local Coefficients for Relief",
       col.regions = rev(terrain.colors(100)))

# Plot local coefficients for 'rainfallAnnual_mean'
spplot(local_coefficients, "rainfallAnnual_mean", main = "Local Coefficients for Rainfall",
       col.regions = rev(terrain.colors(100)))
