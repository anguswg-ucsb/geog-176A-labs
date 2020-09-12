# Angus Watters
# Geog - 176A
# Lab 06


library(tidyverse)
library(sf)        # vector manipulation
library(raster)    # raster manipulation
library(fasterize) # "faster" raster
library(whitebox)  # terrain analysis
library(AOI)

# Data libraries
library(osmdata)   # OSM API
library(elevatr)   # Elevation  Web Tiles
library(units)
library(mapview)
library(fasterize)

basin  = read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin/")
write_sf(basin, dsn = "data/USGS-11119750.gpkg")

# Elevation raster
elev  = get_elev_raster(basin, z = 13) %>%
  crop(basin) %>%
  mask(basin)


writeRaster(elev, "data/mission-creek-area-elev.tif", overwrite = TRUE)

# meters to feet conversion
elev2 = elev * 3.281

# OSM building query
buildings = opq(basin) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()

# building centroids
centroids = buildings$osm_polygons %>%
  st_centroid()

# clip buildings within basin boundary
basin_buildings = st_intersection(centroids, basin)

# railway point
railway = centroids %>%
  filter(amenity == 'railway')

# OSM waterway query
waterways = opq(basin) %>%
  add_osm_feature(key = 'waterway') %>%
  osmdata_sf()

streams = waterways$osm_lines

# clip buildings within basin boundary
basin_streams = st_intersection(streams, basin)

# Hillshade raster
wbt_hillshade('data/mission-creek-area-elev.tif', "data/mission-creek-area-hillshade.tif")
hillshade = raster('data/mission-creek-area-hillshade.tif')

# hillshade, basin boundary, river flowlines plots
plot(hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = FALSE, main = 'Hillshade')
plot(basin, add = TRUE, lwd = 2)
plot(basin_streams, add = TRUE, col = 'cyan4', lwd = 3)

# Height Above Nearest Drainage

# Stream raster
streams_buff = basin_streams %>% st_transform(5070) %>%
  st_buffer(10) %>%
  st_transform(4326)

streams_rast = fasterize(streams_buff, elev2)
writeRaster(streams_rast, 'data/mission-creek-area-streams-rast.tif', overwrite = TRUE)

# Hydrological corrected surface (breach depressions)
wbt_breach_depressions('data/mission-creek-area-elev.tif', 'data/mission-creek-area-breach-depress.tif')
breach_depress = raster('data/mission-creek-area-breach-depress.tif')

# HAND raster
wbt_elevation_above_stream('data/mission-creek-area-breach-depress.tif', 'data/mission-creek-area-streams-rast.tif', 'data/mission-creek-area-HAND.tif')
hand = raster('data/mission-creek-area-HAND.tif')

# correct local datum offsets
hand2 = hand + 3.69
hand2[streams_rast == 1] = 0

writeRaster(hand2, 'data/mission-creek-area-HAND-offset.tif', overwrite = TRUE)


# 2017 Impact Assessment:
#hand3 = hand2
#hand3[hand3 > 10.02] = NA

flood_func = function(x){
  ifelse(x < 10.02, x, NA)
}

hand_r = calc(hand2, flood_func)

plot(hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = FALSE, main = 'Hillshade')
plot(basin, add = TRUE, lwd = 2)
plot(hand_r, col = rev(blues9), add = TRUE, lwd = 6)
plot(railway, add = TRUE, col = 'green', cex = 1, pch = 16)
plot(basin_buildings$geometry, add = TRUE, col = cols, pch = 16, cex = 0.08)

# Estimate the impacts
flood_depth = extract(hand_r, basin_buildings)
cols = ifelse(!is.na(flood_depth), 'darkred', 'black')
sum(cols == 'darkred')















# Impacted Structures

build_on_stream = st_intersection(centroids, streams_buff)

plot(build_on_stream, col = 'black')



# Flood Inudation Map library

sb = aoi_get("Santa Barbara") %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

writeRaster(sb, filename = 'data/santa-barbara.tif', overwrite = TRUE)
plot(sb)

basin_sb = crop(hand, sb)








