# Angus Watters
# Geog - 176A
# Lab 04

library(knitr)
library(units)
library(readxl)
library(sp)
library(raster) # Raster Data handling
library(tidyverse) # Data Manipulation
library(getlandsat) # keyless Landsat data (2013-2017)
library(sf)
library(mapview)
library(osmdata)
library(elevatr)


goleta = read_csv('data/uscities.csv') %>%
  filter(city == 'Goleta') %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

class(goleta)
isS4(goleta)
elev = get_elev_raster(bb2, z = 13) %>% crop(bb2)

plot(elev)

elev2 = elev
elev2[elev2 <= 0] = NA
plot(elev2)

func = function(i) {
  ifelse(i <= 0, NA, 1)
}

elev3 = calc(elev, func)

elev4 = elev3*elev

hex = cellStats(elev2, fivenum)

reclass = data.frame(c(-Inf,100,200, 300, 400, 500), seq(100,600,100), c(0:5))

stac = stack(elev, elev3, elev4)

elev5_plot = reclassify(elev4, reclass) %>%
  plot(col = viridis::viridis(6))
bb2 = read_csv('data/uscities.csv') %>%
  filter(city == 'Grand Canyon Village') %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(10000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

scenes2 = lsat_scenes()

bb_wgs2 = st_transform(bb2, 4326) %>%  st_bbox()

down2 = scenes2 %>%
  filter(min_lat <= bb_wgs2$ymin, max_lat >= bb_wgs2$ymax, min_lon <= bb_wgs2$xmin,
         max_lon >= bb_wgs2$xmax,
         as.Date(acquisitionDate) == as.Date('2017-03-09'))

write.csv(down2, file = 'data/grand-canyon.csv', row.names = FALSE)

#### IN RMD
# Step 2
meta2 = read_csv('data/grand-canyon.csv')

files2 = lsat_scene_files(meta2$download_url) %>%
  filter(grepl(paste0('B', 1:6, '.TIF$', collapse = '|'), file)) %>%
  arrange(file) %>%
  pull(file)

# Step 3
st2 = sapply(files2, lsat_image)
s2 = stack(st2) %>%
  setNames(c('Coastal', 'Blue', 'Green', 'Red', 'NIR', 'SWIR1'))
## What are the dimensions of your stacked image? What is the CRS? What is the cell resolution?
## The stacked image has dimensions of 7811 rows, 7681 columns, 59996291 cells, and 6 layers.
## The CRS of the image stack is +proj=utm +zone=15 +datum=WGS84 +units=m +no_defs
## The cell resolution of the image stack is x = 30 and y = 30.

# Step 4
cropper2 = bb2 %>% st_as_sf() %>%
  st_transform(crs(s2))
r2 = crop(s2, cropper2)


# Step 1
nat_col = plotRGB(r2, r = 4, g = 3, b = 2)
inf_NIR = plotRGB(r2, r = 5, g = 4, b = 3, stretch = 'lin')
false_SWIR = plotRGB(r2, r = 5, g = 6, b = 4, stretch = 'hist')
false_agr = plotRGB(r2, r = 6, g = 5, b = 2, stretch = 'hist')



# Question 1:
bb = read_csv('data/uscities.csv') %>%
  filter(city == 'Palo') %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

# Question 2:

# Step 1
scenes = lsat_scenes()

bb_wgs = st_transform(bb, 4326) %>%  st_bbox()

down = scenes %>%
  filter(min_lat <= bb_wgs$ymin, max_lat >= bb_wgs$ymax, min_lon <= bb_wgs$xmin,
         max_lon >= bb_wgs$xmax,
         as.Date(acquisitionDate) == as.Date('2016-09-26'))

write.csv(down, file = 'data/palo-flood.csv', row.names = FALSE)

#### IN RMD
# Step 2
meta = read_csv('data/palo-flood.csv')

files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0('B', 1:6, '.TIF$', collapse = '|'), file)) %>%
  arrange(file) %>%
  pull(file)

# Step 3
st = sapply(files, lsat_image)
s = stack(st) %>%
  setNames(c('Coastal', 'Blue', 'Green', 'Red', 'NIR', 'SWIR1'))
## What are the dimensions of your stacked image? What is the CRS? What is the cell resolution?
## The stacked image has dimensions of 7811 rows, 7681 columns, 59996291 cells, and 6 layers.
## The CRS of the image stack is +proj=utm +zone=15 +datum=WGS84 +units=m +no_defs
## The cell resolution of the image stack is x = 30 and y = 30.

# Step 4
cropper = bb %>% st_as_sf() %>%
  st_transform(crs(s))
r = crop(s, cropper)

## What are the dimensions of your cropped image stack? What is the CRS? What is the cell resolution?
## The cropped image stack has dimensions of 340 rows, 346 columns, 117640 cells, and 6 layers.
## The CRS of the cropped image stack is +proj=utm +zone=15 +datum=WGS84 +units=m +no_defs
## The cell resolution of the cropped image stack is x = 30 and y = 30.

# Question 3:

# Step 1
nat_col = plotRGB(r, r = 4, g = 3, b = 2)
inf_NIR = plotRGB(r, r = 5, g = 4, b = 3, stretch = 'lin')
false_SWIR = plotRGB(r, r = 5, g = 6, b = 4, stretch = 'hist')
false_agr = plotRGB(r, r = 6, g = 5, b = 2, stretch = 'hist')

# Step 2
##Describe the purpose of applying a color stretch.
## When a color stretch is applied, the maximum and minimum colors in the images become the new range of colors.
## The result of the stretch is that the features being highlighted by the given RGB channel are more emphasized and clearly seen in the image.

# Question 4:

# Step 1

# NVDI
x = (r$NIR - r$Red)/ (r$NIR + r$Red)

ndvi_func = function(x) {
  ifelse(x < 0, 1, NA)
}
ndvi = calc(x, ndvi_func)
plot(ndvi)
#plot(x, col = colorRampPalette())

# NWDI
y = (r$Green - r$NIR)/ (r$NIR + r$Green)

nwdi_func = function(y) {
  ifelse(y > 0, 1, NA)
}
ndwi = calc(y, nwdi_func)

# MNDWI
z = (r$Green - r$SWIR1)/ (r$SWIR1 + r$Green)

mnwdi_func = function(z) {
  ifelse(z > 0, 1, NA)
}
mndwi = calc(z, mnwdi_func)

# WRI
a = (r$Green + r$Red)/ (r$SWIR1 + r$NIR)

wri_func = function(a) {
  ifelse(a > 1, 1, NA)
}
wri = calc(a, wri_func)

# SWI
b = (1)/ sqrt(r$Blue - r$SWIR1)

swi_func = function(b) {
  ifelse(b < 5, 1, NA)
}
swi = calc(b, swi_func)

water_features_stack = stack(ndvi, ndwi, mndwi, wri, swi) %>%
  setNames(c('NDVI', 'NDWI', 'MNDWI', 'WRI', 'SWI'))

plot(water_features_stack, col = "blue")
## Describe the 5 images. How are they simular and where do they deviate?
## The NDVI, NDWI, and WRI images are the most similar in that they show a lesser amount of flooded area compared to the SWI and MNDWI images.
## The SWI image highlights a wider flood area buffering the river and also picks up on other flooded areas outside of the rivers natural channel.

# Question 5:

# Step 1
set.seed(09052020)

# Step 2
v = getValues(r)
dim(v)

## What do the diminsions of the extracted values tell you about how the data was extracted?
## Dimensions:
## The dimensions of the extracted values from the 6 band raster stack are 117640, 6 which means there are 117640 cells and 6 layers in the raster.

idx = which(!is.na(v))
v = na.omit(v)

E = kmeans(v, centers = 12)

new_raster = r$Coastal
values(new_raster) = NA
new_raster[idx] = E$cluster

# Step 3

# Tables of binary flood masks vs. kmeans_raster
tab_ndvi = table(values(ndvi), values(new_raster))
tab_ndwi = table(values(ndwi), values(new_raster))
tab_mndwi = table(values(mndwi), values(new_raster))
tab_wri = table(values(wri), values(new_raster))
tab_swi = table(values(swi), values(new_raster))

ndvi_mask = which.max(tab_ndvi[1,])

flood_func = function(x){
  ifelse(x == ndvi_mask, 1, NA)
}
new_raster_2 = calc(new_raster, flood_func)

final = addLayer(water_features_stack, new_raster_2)
final[is.na(final)] = 0

# Question 6:

# Calculate total area of the flooded cells in each image
area_ndvi = (cellStats(ndvi, 'sum') * 900)
area_ndwi = (cellStats(ndwi, 'sum') * 900)
area_mndwi = (cellStats(mndwi, 'sum') * 900)
area_wri = (cellStats(wri, 'sum') * 900)
area_swi = (cellStats(swi, 'sum') * 900)
area_new_rast_2 = (cellStats(new_raster_2, 'sum') * 900)

area_df = data.frame(img_band = c('NDVI', 'NDWI', 'MNDWI', 'WRI', 'SWI', 'k-means'),
                      total_area = c(area_ndvi, area_ndwi, area_mndwi, area_wri, area_swi, area_new_rast_2))
kable(area_df, caption = 'Total Flooded Area' , col.names = c('Image band', 'Area(m**2)'))

final_area =cellStats(final, sum) * (900)
kable(final_area, caption = 'Total Flooded Area', col.names = ('Area (m**2)'))

# Uncertainty
Uncertainty = sum(final)

# plot flood map
Uncertainty[is.na(Uncertainty)] = 0
mapview(Uncertainty)
mapview(Uncertainty, col = RColorBrewer::brewer.pal(blues9, 'Spectral'))

### Some of the cells have decimal values as the data is streamed in chunks meaning portions of the Web map will be work even though not all the data in the cells has yet to be fully rendered.























