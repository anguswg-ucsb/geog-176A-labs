


library(sf)
library(raster)
library(USAboundaries)
library(USAboundariesData)
library(tidyverse)
library(AOI)
library(elevatr)

mt = aoi_get('Mount Elbert') %>%
  aoi_buffer(10)

# elevation raster
elev = get_elev_raster(mt, z = 8) %>% crop(mt)
writeRaster(elev, filename = 'data/mount-elbert-elev-8.tif', overwrite = TRUE)

elev2 = get_elev_raster(mt, z = 12) %>% crop(mt)
writeRaster(elev2, filename = 'data/mount-elbert-elev-12.tif', overwrite = TRUE)

elev3 = get_elev_raster(mt, z = 10) %>% crop(mt)
writeRaster(elev3, filename = 'data/mount-elbert-elev-10.tif', overwrite = TRUE)

elev4 = get_elev_raster(mt, z = 9) %>% crop(mt)
writeRaster(elev4, filename = 'data/mount-elbert-elev-9.tif', overwrite = TRUE)


val = getValues(elev)
val2 = getValues(elev2)

thresh = function(x){ ifelse(x < mean(x), 1, 2)}

elev_thresh = calc(elev2, thresh)

plot(elev_thresh, col = viridis::viridis(4))

s = stack(elev, elev^2, elev*.5)
mean(s) %>% plot()

cellStats(s, mean)
plot(s)
quarts  = cellStats(elev2, fivenum)
(rcl = data.frame(quarts[1:4], quarts[2:5], 1:4))

reclassify(elev2, rcl, include.lowest=TRUE ) %>%
  plot(col = viridis::viridis(4))


v1 = values(elev)

v2 = values(elev2)

v4 = values(elev4)











r = raster(ncol=20, nrow=20, xmx=-80, xmn=-120, ymn=20, ymx=60)
values(r) = 1:ncell(r)
plot(r)

m = raster(ncol=10, nrow=10, xmx=-80, xmn=-120, ymn=20, ymx=60)
values(m) = 1:ncell(m)
plot(m)

m_val = getValues(m)
m_val[1:1]


get_cellres = function(raw){
  area = st_bbox(raw) %>%
    st_as_sfc() %>%
    st_transform(5070) %>%
    st_area()
  as.numeric(sqrt(area / ncell(raw)))
}

v = table(values(r))
res = get_cellres(r)
c = v[1] / (v[1] + v[3] + v[4])
f = (v[1] + v[3]) / (v[1] + v[4])

df = data.frame(v) %>%
  setNames(c("class","cells")) %>%
  mutate(zoom = z,
         des = c("True Hit", "True Miss", "False Hit", "False Miss"),
         cellres_m = res,
         femaareakm2 = as.numeric(st_area(st_union(fema))/1e6),
         ncell = sum(!is.na(values(r))),
         c = c,
         f = f,
         areakm2 = (res*res*cells)/1e6)

































library(climateR)



ca = USAboundaries::us_states() %>% filter(name == "California")

system.time({ prcp = climateR::getTerraClim(ca, "prcp",
                                            startDate = "2000-01-01", endDate = '2005-12-31') })

plot(cellStats(prcp$prcp, max), type = "l", ylab = "rainfall", xlab = "month since 2000-01")
lines(cellStats(prcp$prcp, min), type = "l",  col = "green", lwd = 2)
lines(cellStats(prcp$prcp, mean), type = "l", col = "darkred", lwd = 2)

plot(mean(prcp$prcp), col = blues9)
plot(ca, add =TRUE, col = NA, lwd = 2)




























# 10 degree resolution, bio variables
(climate <- getData('worldclim', var='bio', res=10))

# define region to explore (US West)
west = us_states() %>%
  filter(stusps %in% c("CA", "AZ", "UT", "OR", "WA", "NV", "ID")) %>%
  st_union() %>%
  st_transform(crs(climate)) %>%
  as_Spatial()

west_tmp = crop(climate$bio5, west)
plot(west_tmp, main="Max Temperature of Warmest Month")

hawaii = us_states() %>%
  filter(stusps %in% c("HI")) %>%
  st_union() %>%
  st_transform(crs(climate)) %>%
  as_Spatial()

hawaii_temp = crop(climate$bio5, hawaii)
plot(hawaii_temp, main="Max Temperature of Warmest Month")

plot(west)


west_tmp = crop(climate$bio5, west)
plot(west_tmp, main="Max Temperature of Warmest Month")














az = filter(us_states(), name == "Colorado") %>%
    st_transform(5070)

bb = st_bbox(az) %>%
  st_as_sfc()
class(bb)


plot(bb, border = rgb(0,0,1))
plot(az, add = TRUE, col = rgb(1,0,0, .5))
