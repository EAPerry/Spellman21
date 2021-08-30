# Data Visualizations 7/20/21
# Spellman Fellow 2021
# Evan Perry

# DESCRIPTION:
# Create the data visualizations for the week 7 presentation.


#**************************************************************************
# Importing ---------------------------------------------------------------
#**************************************************************************


library(tmap)
library(sf)
library(tidyverse)
library(tigris)
library(censusapi)
library(usmap)
library(RColorBrewer)
library(tidycensus)
library(stargazer)
library(tmaptools)
library(geosphere)
library(raster)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

main.df <- read_csv("pointData.csv")
ct.final <- read_csv("tractData.csv")

temp.df <- with(ct.final, tibble(GEOID, GEOID10))

main.df <- merge(main.df, temp.df, by.x = "fips",by.y="GEOID")

rm(temp.df)

#**************************************************************************
# Data Visualization ------------------------------------------------------
#**************************************************************************

# Focus on Columbus, OH
# GEOID10 is 

temp <- urban_areas() %>% filter(grepl("Indianapolis",NAME10,fixed =T))

cityGEOID10 <- "41212"
city.center <- tibble(39.76914829495801, -86.15706661764135) %>% 
  rename(lat = "39.769148294958", long = "-86.1570666176413")

city.tracts <- ct.final %>% filter(GEOID10 == cityGEOID10) %>% 
  filter(GEOID != "18059410600")
city.shp <- tracts("IN",cb=T) %>% filter(GEOID %in% city.tracts$GEOID)
city.points <- main.df %>% 
  filter(GEOID10 == cityGEOID10, fips != "18059410600") %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(city.shp), remove = F)
cc.point <- city.center %>% 
  st_as_sf(coords = c("long", "lat"),crs = st_crs(city.shp), remove = F)


# Building Points by Type

tm_shape(city.shp) +
  tm_polygons(col = "ghostwhite", border.col = "black", border.alpha = 0.2) +
  tm_shape(city.points) +
  tm_dots(style="cat",
          palette= c("#f2ae72","#588c7e","#563f46"),
          size = 0.08,
          col = "build.type",
          legend.show = F) +
  tm_facets(by = "build.type", free.coords = F) + 
  tm_shape(cc.point) +
  tm_dots(col = "red", size = 0.08) +
  tm_compass(type = "arrow", size = 1, position = c(0.05,0.85)) +
  tm_scale_bar(width = .2) + 
  tm_layout(attr.position = c(0.05,0.1))


# Basic Census Tract Counts

city.shp <- merge(city.shp, city.tracts, by = "GEOID")

tm_shape(city.shp) +
  tm_polygons(
    "tract.count", 
    palette= "PuRd",
    style = "pretty",
    as.count = T,
    border.col = "black",
    title = "Green Buildings"
  ) +
  tm_shape(cc.point) +
  tm_dots(col = "red", size = 0.2) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.1,0.85)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside = T,
            legend.stack = "horizontal",
            legend.outside.size = .5,
            attr.position = c(0.1,0.05)
  ) +
  tm_scale_bar(width = .2)

# Prepping the linearized verisons

d2cc <- vector(length = nrow(city.points))

for (i in 1:nrow(city.points)){
  d2cc[i] <- distm(
    c(city.points$long[i],city.points$lat[i]), 
    c(city.center$long, city.center$lat), 
    fun = distGeo)
}

city.points<- cbind(city.points, d2cc)
rm(d2cc, i, city.center, temp)

# The Histogram

ggplot(city.points) +
  geom_histogram(mapping =aes(x=d2cc*.001, fill = build.type), binwidth = 1, show.legend = F) +
  facet_wrap("build.type") +
  theme_bw() +
  xlab("\nDistance from City Center (km)") +
  ylab("Count\n") +
  scale_fill_manual(values = c("#f2ae72","#588c7e","#563f46"), name = "build.type")

# The Density Plot

ggplot(city.points, mapping = aes(x=d2cc*.001, color = build.type)) +
  geom_density(lwd = 1, alpha = 0.1) +
  theme_bw() +
  labs(color = "Building Type") +
  xlab("\nDistance from City Center (km)") +
  ylab("Relative Frequency for Building Type\n") +
  scale_color_manual(values = c("#f2ae72","#588c7e","#563f46"))


# Commercial Counts for Census Tracts

tm_shape(city.shp) +
  tm_polygons(
    "com.count", 
    palette= "PuRd",
    style = "pretty",
    as.count = T,
    border.col = "black",
    title = "Green Commercial Buildings",
    legend.hist = TRUE
  ) +
  tm_shape(cc.point) +
  tm_dots(col = "red", size = 0.2) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.1,0.85)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "bottom",
            legend.outside = T,
            legend.stack = "horizontal",
            legend.hist.width = 1,
            legend.hist.height = 0.8,
            legend.hist.size = 0.7,
            legend.outside.size = .24
  ) +
  tm_scale_bar(width = .2)

# Median HH Income for Census Tracts

tm_shape(city.shp) +
  tm_polygons(
    "med_hh_income", 
    palette= "PuRd",
    style = "pretty",
    as.count = T,
    border.col = "black",
    title = "Median Household Income",
    legend.hist = TRUE
  ) +
  tm_shape(cc.point) +
  tm_dots(col = "red", size = 0.2) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.1,0.85)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "bottom",
            legend.outside = T,
            legend.stack = "horizontal",
            legend.hist.width = 1,
            legend.hist.height = 0.8,
            legend.hist.size = 0.7,
            legend.outside.size = .24
  ) +
  tm_scale_bar(width = .2)

# Screwing around

ggplot(city.shp , mapping = aes(x= log(med_hh_income), y = log(com.count + 1))) +
  geom_point()


none <- as_tibble(ct.final) %>% filter(com.count == 0, UATYP10 == "U")
some <- as_tibble(ct.final) %>% filter(com.count > 0, UATYP10 == "U")


# For several cities now

temp <- urban_areas() %>% filter(grepl("Dallas",NAME10,fixed =T))


cityGEOID10 <- c("41212","57628","16264","19234","77770","63217","51445","22042")
names <- c("Indianapolis", 
           "Minneapolis-St.Paul",
           "Chicago",
           "Columbus",
           "St. Louis",
           "New York",
           "Los Angeles",
           "Dallas-Fort Worth"
           )

lats <- c(
  39.76914829495801, 
  44.976650069118456, 
  41.87927956538127, 
  39.9632568976491, 
  38.62772430190209,
  40.71360512273683,
  34.0504914245081,
  32.780361976850656
)

longs <- c(
  -86.15706661764135, 
  -93.27252387671099, 
  -87.63590137226471, 
  -82.99941993863119, 
  -90.19000270220538,
  -74.0132078386312,
  -118.26075464657801,
  -96.80351386003677
)




city.centers <- tibble(lats, longs)

all.tracts <- st_read("allCensusTracts.shp",stringsAsFactors = F)

output.tb <- tibble(NULL,NULL)

for (i in 1:8){
  
  city.i.tracts <- ct.final %>% filter(GEOID10 == cityGEOID10[i]) 
  
  city.shp <- all.tracts %>% filter(GEOID %in% city.tracts$GEOID)
  
  city.points <- main.df %>% 
    filter(GEOID10 == cityGEOID10[i]) %>% 
    st_as_sf(coords = c("long", "lat"), crs = st_crs(city.shp), remove = F)
  
  d2cc <- vector(length = nrow(city.points))
  
  for (j in 1:nrow(city.points)){
    d2cc[j] <- distm(
      c(city.points$long[j],city.points$lat[j]), 
      c(city.centers$longs[i], city.centers$lats[i]), 
      fun = distGeo)
  }
  
  name.vector <- rep(names[i], nrow(city.points))
  max.d2cc <- max(d2cc)
  d2cc <- d2cc / max.d2cc
  
  temp.tb <- tibble(name.vector, d2cc)
  
  output.tb <- rbind(output.tb, temp.tb)
}

good.cities <- output.tb %>% filter(
  name.vector %in% c("Columbus","Minneapolis-St.Paul","Indianapolis","Dallas-Fort Worth")
)
bad.cities <- output.tb %>% filter(
  name.vector %in% c("New York","Chicago","St. Louis", "Los Angeles")
)

ggplot(good.cities, mapping = aes(x=d2cc*100, color = name.vector)) +
  geom_density(lwd = 1, alpha = 0.1) +
  theme_bw() +
  labs(color = "City") +
  xlab("\nDistance from City Center (% of Radius)") +
  ylab("Green Commercial Building Frequency\n") +
  scale_color_manual(values = c("#880011","#f2ae72","#588c7e","#563f46"))

ggplot(bad.cities, mapping = aes(x=d2cc*100, color = name.vector)) +
  geom_density(lwd = 1, alpha = 0.1) +
  theme_bw() +
  labs(color = "City") +
  xlab("\nDistance from City Center (% of Radius)") +
  ylab("Green Commercial Building Frequency\n") +
  scale_color_manual(values = c("#880011","#f2ae72","#588c7e","#563f46"))






