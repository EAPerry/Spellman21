# Data Visualizations 7/13/21
# Evan Perry
# Spellman Fellow Program

# DESCRIPTION:
# This script creates the data visualizations used in the my Spellman Fellow 
# presentation on 7/13/21. Some numbers may change slightly due to additional
# cleaning on the point and census tract level data.


# *************************************************************************
# Importing ---------------------------------------------------------------
# *************************************************************************

# install.packages("tmap")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("tigris")
# install.packages("censusapi")
# install.packages("usmap")
# install.packages("RColorBrewer")
# install.packages("tidycensus")
# install.packages("stargazer")

library(tmap)
library(sf)
library(tidyverse)
library(tigris)
library(censusapi)
library(usmap)
library(RColorBrewer)
library(tidycensus)
library(stargazer)

options(tigris_class = "sf")

main.df <- read_csv("pointData.csv")
ct.final <- read_csv("tractData.csv")

# *************************************************************************
# Building Type Bar Charts ------------------------------------------------
# *************************************************************************

# Energy Star 

estar.obs <- main.df %>% filter(estar.obs == 1)

estar.types <- count(estar.obs, estar.own.type)

estar.types <- estar.types %>% 
  mutate(prop = n / nrow(estar.obs))

estar.types <- estar.types %>% 
  filter(prop >= .03) 

estar.types <- estar.types %>% 
  rbind(c("Other", 
          nrow(estar.obs) - sum(estar.types$n), 
          1 - (sum(estar.types$n)/nrow(estar.obs))))

estar.types$estar.own.type <- factor(
  estar.types$estar.own.type,
  levels = c("Other","Multifamily Housing","Supermarket/Grocery Store",
             "Retail Store","Office","K-12 School"))

ggplot(estar.types, mapping = aes(y = estar.own.type, x = as.numeric(prop)*100)) +
  geom_bar(stat = "identity",
           fill = "#f2ae72",
           width = 0.6
  ) +
  theme_minimal() +
  xlab("\nPercentage of Energy Star Buildings") +
  ylab("Building Type\n") +
  theme(plot.title = element_blank())

rm(estar.types)

# LEED

leed.obs <- main.df %>% filter(leed.obs == 1)

leed.sort <- rep("", nrow(leed.obs))

for (i in 1:nrow(leed.obs)){
  if (grepl("school", leed.obs$leed.proj.type[i], ignore.case = T) | 
      grepl("learning", leed.obs$leed.proj.type[i], ignore.case = T) | 
      grepl("education", leed.obs$leed.proj.type[i], ignore.case = T)){
    leed.sort[i] <- "Schools"
  } else if (grepl("office", leed.obs$leed.proj.type[i], ignore.case = T)){
    leed.sort[i] <- "Office"
  } else if (grepl("retail", leed.obs$leed.proj.type[i], ignore.case = T) | 
             grepl("store", leed.obs$leed.proj.type[i], ignore.case = T)){
    leed.sort[i] <- "Retail"
  } else if (grepl("home", leed.obs$leed.proj.type[i], ignore.case = T) | 
             grepl("single", leed.obs$leed.proj.type[i], ignore.case = T) |
             grepl("family", leed.obs$leed.proj.type[i], ignore.case = T) |
             grepl("residence", leed.obs$leed.proj.type[i], ignore.case = T) |
             grepl("homes", leed.obs$leed.version[i], ignore.case = T)){
    leed.sort[i] <- "Residential"
  } else if (grepl("military", leed.obs$leed.proj.type[i], ignore.case = T) | 
             grepl("public", leed.obs$leed.proj.type[i], ignore.case = T) |
             grepl("government", leed.obs$leed.proj.type[i], ignore.case = T) |
             grepl("library", leed.obs$leed.proj.type[i], ignore.case = T)){
    leed.sort[i] <- "Government/Military"
  } else if (grepl("health", leed.obs$leed.proj.type[i], ignore.case = T)){
    leed.sort[i] <- "Healthcare"
  } else if (grepl("lodg", leed.obs$leed.proj.type[i], ignore.case = T) | 
             grepl("hotel", leed.obs$leed.proj.type[i], ignore.case = T)){
    leed.sort[i] <- "Lodging/Hotels"
  } else {
    leed.sort[i] <- "Other"
  }
}

leed.obs <- cbind(leed.obs,leed.sort)
rm(leed.sort, i)

leed.types <- count(leed.obs, leed.sort)

leed.types <- leed.types %>% 
  mutate(prop = n / nrow(leed.obs))

leed.types$leed.sort <- factor(leed.types$leed.sort, 
                               levels = c("Other", "Healthcare", "Lodging/Hotels","Government/Military",
                                          "Retail","Schools","Office","Residential"))

ggplot(leed.types, mapping = aes(y = leed.sort, x = as.numeric(prop)*100)) +
  geom_bar(stat = "identity", 
           fill = "#00647d",
           width = 0.6
  ) +
  theme_minimal() +
  xlab("\nPercentage of LEED Buildings") +
  ylab("Building Type\n") +
  theme(plot.title = element_blank())

rm(leed.types)

# *************************************************************************
# Census Tract Maps -------------------------------------------------------
# *************************************************************************

state.OI <- "MN"
county.OI <- "Hennepin"

ct.geography <- tracts(state.OI,county.OI,cb=TRUE)

ct.sf <- main.df %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(ct.geography)) %>% 
  filter(substr(fips,1,5) == fips(state.OI, county.OI))

ct.counts <- merge(
  ct.geography, ct.final, 
  by = "GEOID",
  all.x = TRUE)

tm_shape(ct.counts) +
  tm_polygons("ghostwhite", border.col = "black") +
  tm_shape(ct.sf) +
  tm_dots(col = "royalblue",
          size = 0.07) +
  tm_layout(frame = FALSE, fontfamily = "serif",attr.outside = TRUE,
            attr.position = c("right","top")) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.15,0.65)) +
  tm_scale_bar(width=0.3)


tm_shape(ct.counts) +
  tm_polygons("tract.count", 
              projection = 3855,
              breaks = c(0,1,5,10,15,20,25,30,35,40,45),
              style = "fixed",
              as.count = TRUE,
              palette = "YlGnBu",
              border.col = "black",
              title = "Green Building Count",
              legend.hist = TRUE,
              legend.hist.title = "Green Building Count \nHistogram",
              legend.hist.z = 0.2) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.15,0.65)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside = TRUE,
            fontfamily = "sans",
            legend.hist.width = 0.5,
            attr.outside = TRUE,
            attr.position = c("right","top"),
            legend.outside.size = 0.3,
            legend.hist.bg.color = "lightgray"
  ) +
  tm_scale_bar(width=0.3)



state.OI <- "IA"
county.OI <- "Linn"

ct.geography <- tracts(state.OI,county.OI,cb=TRUE)

ct.sf <- main.df %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(ct.geography)) %>% 
  filter(substr(fips,1,5) == fips(state.OI, county.OI))

ct.counts <- merge(
  ct.geography, ct.final, 
  by = "GEOID",
  all.x = TRUE)


tm_shape(ct.geography) +
  tm_polygons("white", border.col = "black") +
  tm_shape(ct.sf) +
  tm_dots(col = "royalblue",
          size = 0.1) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.15,0.65)) +
  tm_layout(frame = FALSE,
            fontfamily = "sans",
            attr.outside = TRUE,
            attr.position = c("right","top")
  ) +
  tm_scale_bar(width=0.3)


tm_shape(ct.counts) +
  tm_polygons("tract.count", 
              projection = 3855,
              style = "pretty",
              as.count = TRUE,
              palette = "YlGnBu",
              border.col = "black",
              title = "Green Building Count",
              legend.hist = TRUE,
              legend.hist.title = "Green Building Count \nHistogram",
              legend.hist.z = 0.2) +
  tm_compass(type = "arrow", size = 1.2, position = c(0.15,0.65)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside = TRUE,
            fontfamily = "sans",
            legend.hist.width = 0.5,
            attr.outside = TRUE,
            attr.position = c("right","top"),
            legend.outside.size = 0.3,
            legend.hist.bg.color = "lightgray"
  ) +
  tm_scale_bar(width=0.3)

rm(county.OI, state.OI, ct.counts, ct.geography, ct.sf)

# *************************************************************************
# States Maps -------------------------------------------------------------
# *************************************************************************

state.counts <- count(main.df, state) %>% 
  rename(state.n = n)

us.states <- states(cb = TRUE, resolution = "20m") %>%
  shift_geometry() %>% 
  filter(STUSPS != "PR") 

us.states <- merge(us.states, statepop, by.x = "STATEFP", by.y = "fips")
us.states <- merge(us.states, state.counts, by.x = "STUSPS", by.y = "state") 

us.states <- mutate(us.states,  n.capita = state.n/pop_2015 * 10000) %>% 
  filter(STUSPS != "DC")


tm_shape(us.states) +
  tm_polygons("state.n",
              #breaks = c(0, 1, 2, 4, 8, 16),
              style = "pretty",
              palette = "YlGnBu",
              title = "Green Building Count",
              legend.hist = TRUE,
              border.col = "black",
              lwd = .25,
              legend.hist.title = "Summary Histogram",
              legend.hist.z = 0.2) +
  tm_compass(type = "arrow", size = 1, position = c(0.05,0.85)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside = TRUE,
            fontfamily = "sans",
            legend.hist.width = 0.5,
            attr.position = c("right","bottom"),
            legend.outside.size = 0.4,
            legend.hist.bg.color = "lightgray",
            legend.text.size = .75
  ) +
  tm_scale_bar(width=0.2)


tm_shape(us.states) +
  tm_polygons("n.capita",
              #breaks = c(0, 1, 2, 4, 8, 16),
              style = "pretty",
              palette = "YlGnBu",
              title = "Green Buildings per\n 10,000 People",
              legend.hist = TRUE,
              border.col = "black",
              lwd = .25,
              legend.hist.title = "Summary Histogram",
              legend.hist.z = 0.2) +
  tm_compass(type = "arrow", size = 1, position = c(0.05,0.85)) +
  tm_layout(frame = FALSE,
            legend.outside.position = "right",
            legend.outside = TRUE,
            fontfamily = "sans",
            legend.hist.width = 0.5,
            attr.position = c("right","bottom"),
            legend.outside.size = 0.4,
            legend.hist.bg.color = "lightgray",
            legend.text.size = .75
  ) +
  tm_scale_bar(width=0.2)

rm(us.states, state.counts)


