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

#okay now bring in data to merge climate with plotdata
 <- read_csv("tree_ring_data_wrangling/rw_dat_2018.csv")
wbp_dat$PLOT_CN <- as.character(wbp_dat$PLOT_CN)
wbp_dat$TRE_CN <- as.character(wbp_dat$TRE_CN)
wbp_dat$CORE_CN <- as.character(wbp_dat$CORE_CN)


iw_spat <- SpatialPointsDataFrame(coords = cbind(clim_map$long, clim_map$lat), 
                                  data = clim_map, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))

wbp_spat <- SpatialPointsDataFrame(coords = cbind(wbp_meta$LON_FUZZED, wbp_meta$LAT_FUZZED), 
                                  data = wbp_meta, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))

crop_wbp <- extent(iw_spat)

#cropping to extent of interior west, this takes some time
# the crop function works for the raster packlage but not the terra package
pptStackCropped <- crop(pptStack, crop_wbp)
tminStackCropped <- crop(tminStack, crop_wbp)
tmaxStackCropped <- crop(tmaxStack, crop_wbp)
vpdStackCropped <- crop(vpdStack, crop_wbp)
tmeanStackCropped <- crop(tmeanStack, crop_wbp)

# create an extent object to try to get terra crop function to work
#crop_es <- terra::ext(c(-114.0472, -109.0396, 36.99588, 42.00354))

# pptStackCropped <- terra::crop(pptStack, crop_es)
# tminStackCropped <- terra::crop(tminStack, crop_es)
# tmaxStackCropped <- terra::crop(tmaxStack, crop_es)
# vpdStackCropped <- terra::crop(vpdStack, crop_es)

### Want a single climate value instead of several values for each data point, over the entire geographic extent of my occurrence data


# Export rasters to PRISM data formatted folder
clim_path <-  "PRISM/data_formatted/"

writeRaster(pptStackCropped, paste0(clim_path, "new_wbp_pptStack.tif"), overwrite = T)
writeRaster(tminStackCropped, paste0(clim_path, "new_wbp_tminStack.tif"), overwrite = T)
writeRaster(tmaxStackCropped, paste0(clim_path, "new_wbp_tmaxStack.tif"), overwrite = T)
writeRaster(vpdStackCropped, paste0(clim_path, "new_wbp_vpdStack.tif"), overwrite = T)
writeRaster(tmeanStackCropped, paste0(clim_path, "new_wbp_tmeanStack.tif"), overwrite = T)


#extract PRISM data into vectors for new wbp core data
ppt_extr <- terra::extract(pptStackCropped, wbp_spat[,c("LON_FUZZED", "LAT_FUZZED")]) 
tmin_extr <- terra::extract(tminStackCropped, wbp_spat[,c("LON_FUZZED", "LAT_FUZZED")])
tmax_extr <- terra::extract(tmaxStackCropped, wbp_spat[,c("LON_FUZZED", "LAT_FUZZED")])
vpd_extr <- terra::extract(vpdStackCropped,  wbp_spat[,c("LON_FUZZED", "LAT_FUZZED")])
tmean_extr <- terra::extract(tmeanStackCropped, wbp_spat[,c("LON_FUZZED", "LAT_FUZZED")])

# Add sensible column names for raster extracted climate data by referencing the original climate files
# Each row is an individual tree's data 
ppt_extr <- as.data.frame(ppt_extr)
tmin_extr <- as.data.frame(tmin_extr)
tmax_extr <- as.data.frame(tmax_extr)
vpd_extr <- as.data.frame(vpd_extr)
tmean_extr <- as.data.frame(tmean_extr)

PRISM_path <-  "PRISM/data/"
ppt_files <- list.files(path = PRISM_path, pattern = glob2rx("*ppt*_bil"), full.names = TRUE)

colNames <- lapply(strsplit(ppt_files, "4kmM[23]_"), function(x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)

colnames(ppt_extr) <- paste0("ppt_", colNames)
colnames(tmin_extr) <- paste0("tmin_", colNames)
colnames(tmax_extr) <- paste0("tmax_", colNames)
colnames(vpd_extr) <- paste0("vpd_", colNames)
colnames(tmean_extr) <- paste0("tmean_", colNames)



## Add tre_cn, plt_cn, core_cn columns to link to other dataframes

ppt_df <- data.frame(LON_FUZZED = wbp_spat$LON_FUZZED, LAT_FUZZED = wbp_spat$LAT_FUZZED, ppt_extr)
tmin_df <- data.frame(LON_FUZZED = wbp_spat$LON_FUZZED, LAT_FUZZED = wbp_spat$LAT_FUZZED, tmin_extr)
tmax_df <- data.frame(LON_FUZZED = wbp_spat$LON_FUZZED, LAT_FUZZED = wbp_spat$LAT_FUZZED, tmax_extr)
vpd_df <- data.frame(LON_FUZZED = wbp_spat$LON_FUZZED, LAT_FUZZED = wbp_spat$LAT_FUZZED, vpd_extr)
tmean_df <- data.frame(LON_FUZZED = wbp_spat$LON_FUZZED, LAT_FUZZED = wbp_spat$LAT_FUZZED, tmean_extr)


#adding the CN, TRE_CN, PLT_CN to the climate dataframes
ppt_df <- merge(ppt_df, wbp_meta[, c("LON_FUZZED", "LAT_FUZZED", "CN", "PLOT_CN")], by = c("LON_FUZZED", "LAT_FUZZED"))
tmin_df <- merge(tmin_df, wbp_meta[, c("LON_FUZZED", "LAT_FUZZED", "CN",  "PLOT_CN")], by = c("LON_FUZZED", "LAT_FUZZED"))
tmax_df <- merge(tmax_df, wbp_meta[, c("LON_FUZZED", "LAT_FUZZED", "CN", "PLOT_CN")], by = c("LON_FUZZED", "LAT_FUZZED"))
vpd_df <- merge(vpd_df, wbp_meta[, c("LON_FUZZED", "LAT_FUZZED", "CN", "PLOT_CN")], by = c("LON_FUZZED", "LAT_FUZZED"))
tmean_df <- merge(tmean_df, wbp_meta[, c("LON_FUZZED", "LAT_FUZZED", "CN",  "PLOT_CN")], by = c("LON_FUZZED", "LAT_FUZZED"))
pr
ppt_df <- ppt_df %>%
  dplyr::select(CN, PLOT_CN, LON_FUZZED, LAT_FUZZED, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN) %>% 
  distinct(CORE_CN, .keep_all = TRUE)
# Renames CN to CORE_CN

tmin_df <- tmin_df %>%
  dplyr::select(CN, PLOT_CN, LON_FUZZED, LAT_FUZZED, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  %>% # Renames CN to CORE_CN%>% 
distinct(CORE_CN, .keep_all = TRUE)

tmax_df <- tmax_df %>%
  dplyr::select(CN, PLOT_CN, LON_FUZZED, LAT_FUZZED, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  %>% 
  distinct(CORE_CN, .keep_all = TRUE)

vpd_df <- vpd_df %>%
  dplyr::select(CN, PLOT_CN, LON_FUZZED, LAT_FUZZED, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  %>%
  distinct(CORE_CN, .keep_all = TRUE)

tmean_df <- tmean_df %>%
  dplyr::select(CN, PLOT_CN, LON_FUZZED, LAT_FUZZED, everything()) %>%  # Moves TRE_CN, CN, PLT_CN to the front
  rename(CORE_CN = CN)  %>% 
  distinct(CORE_CN, .keep_all = TRUE)

#n.tmx.extr <- as.data.frame(n.tmx.extr)
#n.tmx.extr$TRE_CN <- val_trees$TRE_CN
# Export climate data

write_csv(ppt_df, "PRISM/data_formatted/new_wbp_ppt_df.csv")
write_csv(tmin_df, "PRISM/data_formatted/new_wbp_tmin_df.csv")
write_csv(tmax_df, "PRISM/data_formatted/new_wbp_tmax_df.csv")
write_csv(vpd_df, "PRISM/data_formatted/new_wbp_vpd_df.csv")
write_csv(tmean_df, "PRISM/data_formatted/new_wbp_tmean_df.csv")


