# Final Presentation Data Viz
# Evan Perry

library(tmap)
library(sf)
library(tidyverse)
library(tigris)
library(usmap)
library(RColorBrewer)
library(stringr)


options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

pointData <- read_csv("pointData.csv")
tractData <- read_csv("tractData.csv")
cellphoneData <- read_csv("cellphoneDataReduced.csv")
ia_tracts <- tracts("IA",cb = T, year = 2019)

city.shp <- urban_areas() %>% filter(GEOID10 == "23743")

cr <- pointData %>% 
  filter(substr(fips, 1, 5) == "19153", build.type == "Commercial") %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(city.shp), remove = F)

cr <- st_intersection(cr, city.shp)

tracts <- st_intersection(ia_tracts, city.shp)

tracts <- merge(tracts, tractData, by= "GEOID")

coe_palette <- c("#F5F5F5","#E3CCCF","#D1A3A9","#BF7B83","#AC525D","#9A2937","#880011")

tmap_mode("plot")
tm_shape(city.shp) +
  tm_polygons(border.col = "black", alpha = 0) +
  tm_shape(tracts) +
  tm_polygons("com.count", palette= "PuRd",
              style = "pretty",
              as.count = T, 
              border.col = "black", 
              alpha = 0) +
  tm_shape(cr) +
  tm_dots(col = "#880011", size = .1) + 
  tm_compass(type = "arrow", size = 1.2, position = c(0.1,0.85)) +
  tm_layout(frame = FALSE) +
  tm_scale_bar(width = .2)

tm_shape(tracts) +
  tm_polygons("com.count", palette= coe_palette,
              style = "pretty",
              as.count = T, 
              border.col = "gray", 
              title = "Green Commercial \nBuildings",
              legend.hist = F) +
  tm_shape(city.shp) +
  tm_polygons(border.col = "black", alpha = 0) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.85,0.15)) +
  tm_layout(frame = FALSE) +
  tm_scale_bar(width = .2)
