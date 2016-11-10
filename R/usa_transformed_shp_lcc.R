#
# See: http://www.r-bloggers.com/moving-the-earth-well-alaska-hawaii-with-r/
#
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)

rm(list=ls())
setwd("~/Documents/Dlab/consults/robin_e/usa_simp_trans/")
us <- readOGR(".", "states")
#us <- readOGR(dsn="./states2geojson_6d.geojson", layer="OGRGeoJSON")
#proj4string(us) <- CRS("+init=EPSG:4269" ) #NAD83
us@proj4string


plot(us)
# This one is LCC EPSG 102004
us_aea <- spTransform(us, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_def"))
us_aea@data$id <- rownames(us_aea@data)
plot(us_aea)

# Move Alaska
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2400000, -1900000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))

proj4string(hawaii) <- proj4string(us_aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
usa_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15", "78"),]
usa_aea <- rbind(usa_aea, alaska, hawaii)
plot(usa_aea)

# Save IT

# save to shapefile
# write out a new shapefile (including .prj component)
writeOGR(usa_aea, ".", "states_simpt_lcc", driver="ESRI Shapefile")

#http://zevross.com/blog/2016/01/13/tips-for-reading-spatial-files-into-r-with-rgdal/
# writing file x:/myfile.geojson but I need to write a file called
# myfilegeojson and then rename it with the period
writeOGR(usa_aea, dsn="myfilegeojson", layer="", driver="GeoJSON")
file.rename("myfilegeojson", "us_states_aea.geojson")
