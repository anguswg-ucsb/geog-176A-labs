# Angus Watters
# Geog - 176A
# Lab 06


library(tidyverse)
library(sf)        # vector manipulation
library(raster)    # raster manipulation
library(fasterize) # "faster" raster
library(whitebox)  # terrain analysis

# Data libraries
library(osmdata)   # OSM API
library(elevatr)   # Elevation  Web Tiles
library(units)
library(mapview)

basin  = read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin/")
write_sf(basin, dsn = "data/USGS-11119750.gpkg")
plot(basin)
elev  = get_elev_raster(basin, z = 13) %>%
  crop(basin)
writeRaster(elev, "data/mission-creek-area-elev.tif", overwrite = TRUE)

elev2 = elev * 3.281
values(elev2)
plot(bb)
buildings = opq(basin) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()

centroids = query$osm_polygons %>%
  st_centroid()

basin_buildings = st_intersection(centroids, basin)

waterways = opq(basin) %>%
  add_osm_feature(key = 'waterway') %>%
  osmdata_sf()





