##PRISM Climate Data - WBP
##June 2023
##cecimartinez333@gmail.com



# load necessary packages -------------------------------------------------

library(prism) # for retrieving prism data
library(tidyverse)
library(maps)
library(terra)
library(raster)
library(sf)
library(sp)
library(dplyr)

### Getting PRISM climate data to create rasters of monthly climate variables in our study area ###
prism_set_dl_dir("normals/")
prism_path <-  "normals/"

# checking file path
list.files(prism_path, pattern = "bil$")

# making own annual temps and ppt
mean_temp <- prism_archive_subset(
  "tmean", "annual normals", resolution = "4km"
)

mean_ppt <- prism_archive_subset(
  "ppt", "annual normals", resolution = "4km"
)

mean_temp <- pd_to_file(mean_temp)
mean_ppt <- pd_to_file(mean_ppt)

## now make the files into rasters
mean_temp_rast <- rast(mean_temp)
mean_ppt_rast <- rast(mean_ppt)

# crop climate to extent of pine occurrences in FIA plots in Idaho ### 
idaho_map <- ggplot2::map_data('state', region = c("Idaho"))
idaho_spat <- vect(idaho_map, geom = c("long", "lat"), crs = "+proj=longlat +datum=NAD83")
idaho_spat <- project(idaho_spat, crs(mean_temp_rast))

# cropping climate normals to idaho
crop_ppt_norms <- crop(mean_ppt_rast, idaho_spat)
crop_temp_norms <- crop(mean_temp_rast, idaho_spat)

# write rasters as tifs
writeRaster(crop_ppt_norms, "outputs/crop_ppt_norms_idaho.tif", overwrite = TRUE)
writeRaster(crop_temp_norms, "outputs/crop_temp_norms_idaho.tif", overwrite = TRUE)

# convert to df for plotting in ggplot
ppt_df <- as.data.frame(crop_ppt_norms, xy = TRUE, na.rm = TRUE)
colnames(ppt_df) <- c("x", "y", "precip")
temp_df <- as.data.frame(crop_temp_norms, xy = TRUE, na.rm = TRUE)
colnames(temp_df) <- c("x", "y", "tmean")

#making idaho into df
idaho_boundary_df <- as.data.frame(geom(idaho_spat))

#plot precip
ggplot() +
  geom_tile(data = ppt_df, aes(x = x, y = y, fill = precip)) +  
  scale_fill_viridis_c(name = "Precipitation (mm)") +  
  geom_path(data = idaho_boundary_df, aes(x = x, y = y), color = "red", size = 1) + 
  labs(title = "mean annual precipitation",
       x = "Longitude", y = "Latitude") +
  coord_fixed() +
  theme_bw()

#plot temps
ggplot() +
  geom_tile(data = temp_df, aes(x = x, y = y, fill = tmean)) +  
  scale_fill_gradient(name = "Temperature (°C)",
                      low = "yellow", high = "red") +
  geom_path(data = idaho_boundary_df, aes(x = x, y = y), color = "black", size = 1) +  # Add Idaho boundary
  labs(title = "mean annual temperature",
       x = "Longitude", y = "Latitude") +
  coord_fixed() +
  theme_bw()

#okay now bring in pine data to merge wiht climate data
pines_dat <- read_csv("data/pines_filtereddata.csv")

