library(terra)
library(rnaturalearth)
library(geodata)

# Use path to folder containing tif files
# path <- "C:/Users/hayes/OneDrive/USF/Kramer Lab/Lab Project - Fall 2026/Birds_of_Mexico/esri_lc_mexico"

# Land Cover Source:
# Esri | Sentinel-2 Land Cover Explorer - https://livingatlas.arcgis.com/landcoverexplorer//#mapCenter=-93.38063%2C14.80968%2C3.96&mode=step&timeExtent=2017%2C2025&year=2025

##### Script for Google Earth Engine ###########################################
# var mx = ee.FeatureCollection('FAO/GAUL/2015/level0')
# .filter(ee.Filter.eq('ADM0_NAME', 'Mexico'));
# var mxGeom = mx.geometry();
# 
# var wcTransform = [1/120, 0, -180, 0, -1/120, 90];
# 
# // Mexico spans roughly lon -118.5 to -86.5, lat 14.5 to 32.8.
# // Whole-degree chunk edges keep every chunk on the WorldClim grid.
# var lonStart = -119, lonEnd = -86, lonStep = 6;
# var latStart = 14,   latEnd = 33,  latStep = 5;
# 
# var col = ee.ImageCollection('projects/sat-io/open-datasets/landcover/ESRI_Global-LULC_10m_TS')
# .filterDate('2025-01-01', '2025-12-31');
# 
# var n = 0;
# for (var lon = lonStart; lon < lonEnd; lon += lonStep) {
#   for (var lat = latStart; lat < latEnd; lat += latStep) {
#     var region = ee.Geometry.Rectangle(
#       [lon, lat, Math.min(lon + lonStep, lonEnd), Math.min(lat + latStep, latEnd)],
#       'EPSG:4326', false);
#     
#     // Skip chunks that don't touch Mexico
#     if (!mxGeom.intersects(region, ee.ErrorMargin(1000)).getInfo()) continue;
# 
#     var sub = col.filterBounds(region);
#     var lc = sub.mosaic().setDefaultProjection(sub.first().projection());
# 
#     var modal = lc.reduceResolution({
#       reducer: ee.Reducer.mode().unweighted(),
#       maxPixels: 65535
#     });
# 
#     Export.image.toDrive({
#       image: modal.toByte(),
#       region: region,
#       crs: 'EPSG:4326',
#       crsTransform: wcTransform,
#       maxPixels: 1e10,
#       shardSize: 16,
#       fileDimensions: 2048,
#       folder: 'esri_lc_mexico',
#       description: 'esri_lc_modal_lon' + Math.abs(lon) + '_lat' + lat
#     });
#     n++;
#   }
# }
# print('Export tasks created:', n);
################################################################################

mx <- ne_countries(country = "mexico", returnclass = "sv")
climate <- geodata::worldclim_global(var = "bio", # 19 bioclim variables
                                          res = 0.5, 
                                          path = "worldclim_data", 
                                          download = TRUE)


climate <- mask(crop(climate,mx), mx)
names(climate) <- c(paste0("BIO", 1:19))

files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
lc <- merge(sprc(files))
NAflag(lc) <- 0

#compareGeom(lc, crop(climate, lc))

lc <- mask(lc, mx)               
lc <- crop(lc, mx)

#compareGeom(lc, climate)

names(lc) <- "esri_lc_2025"
lc_labels <- data.frame(ID = c(1, 2, 4, 5, 7, 8, 9, 10, 11), 
                        class_name = c("Water", "Trees", "Flooded Vegetation", "Crops", 
                                       "Built Area", "Bare Ground", "Snow/Ice", "Clouds", 
                                       "Rangeland"))
levels(lc) <- lc_labels


writeRaster(lc, "lc25", datatype = "INT1U", overwrite = TRUE)
