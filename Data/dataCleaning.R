# Data Cleaning Script
# Spellman Fellow Program
# Evan Perry
# July 2021


# *************************************************************************
# Part 1: Point Level Data ------------------------------------------------
# *************************************************************************

# DESCRIPTION:
# This first piece of the script is intended to clean all the point level data.
# We read in the original data files from the LEED and Energy Star programs
# clean them, merge them, and geocode them. A few additional geographic
# identifiers are added in Part 2.

# *************************************************************************
## Importing --------------------------------------------------------------
# *************************************************************************

# install.packages("tmap")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("tigris")
# install.packages("censusapi")
# install.packages("tidygeocoder")
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
library(tidygeocoder)
library(usmap)
library(RColorBrewer)
library(tidycensus)
library(stargazer)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Data Files Downloaded on 06-23-2021

leed.df <- read_csv("PublicLeedProjectDirectory.csv")
estar.df <- read_csv("EnergyStarData.csv")


# *************************************************************************
## Preliminary Cleaning ---------------------------------------------------
# *************************************************************************

# Clear out any observations outside the US and without a complete address
leed.df <- leed.df %>% subset(Country == "US") %>%
  subset(Isconfidential == "No") %>%
  subset(!is.na(Street)) %>%
  subset(!is.na(City)) %>%
  subset(!is.na(State))

# Make a full address for matching/geocoding
leed.df <- leed.df %>%
  mutate(
    adr = toupper(paste(Street, City, State, sep = ", ")),
    adr = sub("\n", "", adr),
    adr = sub("\r", "", adr),
  )

# Put Address lines 1 & 2 into a single line

estar.street <- rep("", nrow(estar.df))

for (i in 1:nrow(estar.df)) {
  if (is.na(estar.df$Address2[i])) {
    estar.street[i] <- estar.df$Address1[i]
  }
  else {
    estar.street[i] <-
      paste(estar.df$Address1[i], estar.df$Address2[i], sep = " ")
  }
}


# Make a full address for matching/geocoding
estar.df <- estar.df %>%
  mutate(
    Street = estar.street,
    Street = sub("\n", "", Street),
    Street = sub("\r", "", Street),
    adr = toupper(paste(Street, City, State, sep = ", ")),
  )

rm(estar.street, i)


# *************************************************************************
## Merging Datasets -------------------------------------------------------
# *************************************************************************

main.df <- merge(leed.df, estar.df, by = "adr", all = TRUE)

internal.ID <- 1:nrow(main.df)

leed.obs <- rep(0, nrow(main.df))
estar.obs <- rep(0, nrow(main.df))

for (i in 1:nrow(main.df)) {
  if (!is.na(main.df$ID[i])) {
    leed.obs[i] <- 1
  }
  if (!is.na(main.df$'Property/Plant ID'[i])) {
    estar.obs[i] <- 1
  }
}

main.df <- cbind(internal.ID, main.df, leed.obs, estar.obs)
rm(leed.obs, estar.obs, i, internal.ID)

# Get City, State, Zip down to single columns

street <- rep("", nrow(main.df))
city <- rep("", nrow(main.df))
state <- rep("", nrow(main.df))
zipcode <- rep("", nrow(main.df))

for (i in 1:nrow(main.df)) {
  if (main.df$estar.obs[i] == 1) {
    street[i] <- toupper(main.df$Street.y[i])
    city[i] <- toupper(main.df$City.y[i])
    state[i] <- main.df$State.y[i]
    zipcode[i] <- main.df$Zip[i]
  } else {
    street[i] <- toupper(main.df$Street.x[i])
    city[i] <- toupper(main.df$City.x[i])
    state[i] <- main.df$State.x[i]
    zipcode[i] <- main.df$Zipcode[i]
  }
}

street <- gsub("[^[:alnum:]]", " ", street)

main.df <- cbind(main.df, street, city, state, zipcode) %>%
  mutate(address = paste(street, city, state, sep = ", ")) %>%
  select(
    -c(
      Isconfidential,
      adr,
      Address1,
      Address2,
      'Number of Years Certified',
      Street.x,
      Street.y,
      City.x,
      City.y,
      State.x,
      State.y,
      Zip,
      Zipcode,
      Country
    )
  ) %>%
  rename(
    leed.ID = ID,
    estar.ID = 'Property/Plant ID',
    leed.name = ProjectName,
    estar.name = 'Building Name',
    leed.version = LEEDSystemVersionDisplayName,
    leed.points = PointsAchieved,
    leed.cert.level = CertLevel,
    leed.cert.date = CertDate,
    leed.cert.active = IsCertified,
    leed.own.type = OwnerTypes,
    leed.proj.type = ProjectTypes,
    leed.floor.area = GrossSqFoot,
    unit = UnitOfMeasurement,
    leed.prop.area = TotalPropArea,
    leed.owner.name = OwnerOrganization,
    leed.regist.date = RegistrationDate,
    estar.own.type = 'Property Type',
    year.built = 'Year constructed',
    estar.score = 'Score(s)',
    estar.cert.date = 'Certification Years',
    estar.floor.area = 'Gross Floor Area',
    estar.owner = 'Building Owner',
    estar.manager = 'Property Manager'
  )

rm(street, city, state, zipcode, i)


# *************************************************************************
## Geocoding --------------------------------------------------------------
# *************************************************************************

# FAIR WARNING: Takes around 7.5 hours to run
# geo.df <- geo(address = main.df$address, method = "here", full_results = TRUE, timeout = 700)
# geo.df <- geo.df %>% select(
#   address,
#   lat,
#   long,
#   title,
#   id,
#   resultType,
#   localityType,
#   here_address.label,
#   here_address.countryCode,
#   here_address.stateCode,
#   here_address.county,
#   here_address.postalCode,
#   scoring.queryScore,
#   here_address.street)

# Geocoding Results Saved to Separate CSV
# write.csv(geo.df, "geocodeResults.csv", row.names = FALSE)

# Note: We select specific columns from the output of geocoding as not all
# columns of the output can be encoded to a CSV.

geo.df <- read.csv("geocodeResults.csv")

geo.df <- geo.df %>% rename(
  here.title = title,
  here.id = id,
  here.result.type = resultType,
  here.locality.type = localityType,
  here.match = here_address.label,
  here.country = here_address.countryCode,
  here.state = here_address.stateCode,
  here.county = here_address.county,
  here.zip = here_address.postalCode,
  here.queryScore = scoring.queryScore,
  here.street = here_address.street,
  adr = address
)


# Before combining the total.dt and geo.dt we need to check their rows are the same
all(geo.df$adr == main.df$address)
# All good

main.df <- cbind(main.df, geo.df) %>%
  select(-c(adr))

main.df <- main.df %>%
  filter(
    here.result.type != "locality",
    here.result.type != "administrativeArea",!is.na(lat),!is.na(long)
  )

main.df <- main.df %>%
  mutate(latlong = paste(lat, long, sep = ", "))

main.df <- distinct(main.df, latlong, .keep_all = T)


# *************************************************************************
## Final Cleaning ---------------------------------------------------------
# *************************************************************************


col.order <- c(
  "internal.ID",
  "leed.obs",
  "estar.obs",
  "lat",
  "long",
  "street",
  "city",
  "state",
  "zipcode",
  "address",
  "here.match",
  "here.result.type",
  "leed.ID",
  "estar.ID",
  "leed.name",
  "estar.name",
  "leed.own.type",
  "leed.proj.type",
  "estar.own.type",
  "leed.points",
  "estar.score",
  "leed.version",
  "leed.cert.level",
  "leed.floor.area",
  "unit",
  "leed.prop.area",
  "estar.floor.area",
  "leed.cert.date",
  "leed.regist.date",
  "estar.cert.date",
  "leed.cert.active",
  "year.built",
  "leed.owner.name",
  "estar.owner",
  "estar.manager",
  "here.title",
  "here.id",
  "here.street",
  "here.zip",
  "here.county",
  "here.country",
  "here.state",
  "here.locality.type",
  "here.queryScore"
)

main.df <- main.df[, col.order]

main.df <- main.df %>%
  mutate(temp.address = gsub("[^[:alnum:]]", " ", address))

stan.address <- rep(0, nrow(main.df))

for (i in 1:nrow(main.df)) {
  stan.address[i] <-
    paste(strsplit(trimws(main.df$temp.address[i]), "\\s+")[[1]], collapse = " ")
}

main.df <- cbind(main.df, stan.address) %>%
  select(-c(temp.address))

rm(col.order, stan.address, i)

main.df <- distinct(main.df, stan.address, .keep_all = TRUE) %>%
  filter(here.country == "USA")


# *************************************************************************
# Part 2: Tract Level Data ------------------------------------------------
# *************************************************************************

# DESCRIPTION:
# The purpose of this script is to produce a final table of census tract level
# data for the project. This data should include green building counts, as well
# as other forms of enriched data. For instance, we need demographic data and
# climate data. We should also be able to classify neighborhoods as being urban.


# *************************************************************************
## Getting FIPS -----------------------------------------------------------
# *************************************************************************

states <- c(state.abb, "DC")

state.tb.list <- vector("list", length(states))

for (i in 1:length(states)) {
  state.i <- main.df %>%
    filter(state == states[i])
  
  state.i.tracts <- tracts(state = states[i], cb = TRUE)
  
  temp.tb <- state.i %>%
    st_as_sf(coords = c("long", "lat"),
             crs = st_crs(state.i.tracts))
  
  intersect <- st_intersects(temp.tb, state.i.tracts)
  
  state.i <- state.i %>%
    mutate(
      intersection = as.integer(intersect),
      fips = if_else(is.na(intersection), "",
                     state.i.tracts$GEOID[intersection])
    )
  
  state.tb.list[[i]] <- state.i
}

main.df <- bind_rows(state.tb.list) %>%
  select(-c(intersection))

rm(states,
   temp.tb,
   state.i.tracts,
   state.i,
   i,
   intersect,
   state.tb.list)

main.df <- main.df %>%
  filter(
    !grepl("TBD", main.df$street, fixed = TRUE),
    !grepl("NOT ESTABLISHED", main.df$street, fixed = TRUE),
    state == here.state,
    fips != ""
  )


#**************************************************************************
## Forming Census Level Counts --------------------------------------------
#**************************************************************************

# Goal:
# Form counts for all green buildings for all census tracts. Then, classify each
# building in the sample as either public, residential, or commercial. Form
# counts for each of these types of buildings for each census tract.

main.df <- main.df %>%
  mutate(latlong = paste(lat, long, sep = ", "))

main.df <- distinct(main.df, latlong, .keep_all = T)

main.tracts.df <- count(main.df, fips) %>%
  rename(tract.count = n)

build.type <- vector(length = nrow(main.df))

for (i in 1:nrow(main.df)) {
  if (main.df$estar.obs[i] == 1) {
    if (main.df$estar.own.type[i] == "Multifamily Housing" |
        main.df$estar.own.type[i] == "Senior Care Community") {
      build.type[i] <- "Residential"
    } else if (main.df$estar.own.type[i] == "K-12 School" |
               main.df$estar.own.type[i] == "Courthouse") {
      build.type[i] <- "Public"
    } else if (main.df$estar.own.type[i] == "Office") {
      if (grepl("public", main.df$estar.owner[i], ignore.case = T) |
          grepl("city", main.df$estar.owner[i], ignore.case = T) |
          grepl("county", main.df$estar.owner[i], ignore.case = T) |
          grepl("government", main.df$estar.owner[i], ignore.case = T) |
          grepl("state of", main.df$estar.owner[i], ignore.case = T)) {
        build.type[i] <- "Public"
      } else {
        build.type[i] <- "Commercial"
      }
    } else {
      build.type[i] <- "Commercial"
    }
  } else {
    if (grepl("government", main.df$leed.own.type[i], ignore.case = T) |
        grepl("state", main.df$leed.own.type[i], ignore.case = T) |
        grepl("school", main.df$leed.own.type[i], ignore.case = T) |
        grepl("government", main.df$leed.proj.type[i], ignore.case = T)) {
      build.type[i] <- "Public"
    } else if (grepl("home", main.df$leed.proj.type[i], ignore.case = T) |
               grepl("residence", main.df$leed.proj.type[i], ignore.case = T) |
               grepl("home", main.df$leed.version[i], ignore.case = T)) {
      build.type[i] <- "Residential"
    } else {
      build.type[i] <- "Commercial"
    }
  }
}

main.df <- cbind(main.df, build.type)

pub.count <- count(main.df %>% filter(build.type == "Public"), fips)
res.count <-
  count(main.df %>% filter(build.type == "Residential"), fips)
com.count <-
  count(main.df %>% filter(build.type == "Commercial"), fips)

main.tracts.df <-
  merge(main.tracts.df, pub.count, by = "fips", all.x = T) %>%
  rename(pub.count = n)
main.tracts.df <-
  merge(main.tracts.df, res.count, by = "fips", all.x = T) %>%
  rename(res.count = n)
main.tracts.df <-
  merge(main.tracts.df, com.count, by = "fips", all.x = T) %>%
  rename(com.count = n)


rm(build.type, i, pub.count, res.count, com.count)


#**************************************************************************
## Finding Urban Areas ----------------------------------------------------
#**************************************************************************


# Goal:
# Classify each census tract as urban or not based on its inclusion in the
# Census' urban areas. These are defined at the census block level, so we choose
# to classify a census tract as urban if and only if each block within the
# census tract is within an urban area.


# Task 1: Get a spatial object with all census tracts in U.S.
# We write a separate shapefile for this so it is easier to access.

# all.ct <- tracts("DC", cb=T)
#
# for (i in 1:length(state.abb)){
#   state.i.tracts <- tracts(state.abb[i], cb=T)
#   all.ct <- rbind(all.ct, state.i.tracts)
# }
#
# rm(i, state.i.tracts)
#
# all.ct <- subset(all.ct, select=-c(ALAND,AWATER))
# NOTE: Removed ALAND and AWATER due to warning messages
# I reincorporate in ALAND later... but without any warnings
#
# st_write(all.ct, "allCensusTracts.shp")

all.ct <- st_read("allCensusTracts.shp")


# Task 2: Get a spatial object with just those "urban" census tracts.

# See: https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural/2010-urban-rural.html
UA <- urban_areas()

# Get just those census tracts fully within an urbanized area
tracts.within <- st_join(all.ct, UA, join = st_within)
# Problem: Doesn't produce contiguous cities

# Get census tracts that have at least one census block within an urban area
# tracts.intersects <- st_join(all.ct, UA, join = st_intersects)
# Problem: Duplicates. Many CTs have blocks in several urban areas.

rm(UA, all.ct)

#**************************************************************************
## Pulling Demographic Data -----------------------------------------------
#**************************************************************************

# Goal:
# Incorporate additional Census data in to the data set.


# Task 1: Identify useful data to pull.
# See: https://api.census.gov/data/2019/acs/acs5/groups.html

# "B01003_001E" TOTAL POPULATION
# "B01002_001E" MEDIAN AGE
# "B02001_001E" TOTAL RACE
# "B02001_002E" WHITE ALONE
# "B02001_003E" BLACK OR AA ALONE
# "B02001_004E" NATIVE AMERICAN/ALASKAN alone
# "B02001_005E" ASIAN ALONE
# "B02001_006E" NATIVE HAWAIIAN ALONE
# "B02001_007E" SOME OTHER RACE ALONE
# "B02001_008E" TWO OR MORE RACES
# "B19013_001E" MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)
# "B19083_001E" GINI INDEX OF INCOME INEQUALITY
# "B19301_001E" PER CAPITA INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)
# "B25001_001E" TOTAL HOUSING UNITS
# "B25035_001E"	MEDIAN YEAR STRUCTURE BUILT
# "B25040_001E" HOUSE HEATING FUEL TOTAL
# "B25040_002E" HOUSE HEATING FUEL UTILITY GAS
# "B25040_004E" HOUSE HEATING FUEL ELECTRICITY
# "B25040_005E" HOUSE HEATING FUEL FUEL OIL, KEROSENE, ETC.
# "B25040_006E" HOUSE HEATING FUEL COAL OR COKE
# "B25040_008E" HOUSE HEATING FUEL SOLAR ENERGY
# "B25058_001E" MEDIAN CONTRACT RENT
# "B25064_001E" MEDIAN GROSS RENT
# "B25069_001E" INCLUSION OF UTILITIES IN RENT TOTAL
# "B25069_002E" INCLUSION OF UTILITIES IN RENT NOT INCLUDED
# "B25069_003E" INCLUSION OF UTILITIES IN RENT INCLUDED
# "B25077_001E" MEDIAN VALUE (DOLLARS)
# "B25002_001E" TOTAL HH BY OCCUPANCY STATUS
# "B25002_003E" VACANT HH


# Task 2: Use the censusAPI to pull all of this data for all the census tracts
# in each state.

census.df <- getCensus(
  name = "acs/acs5",
  vintage = 2019,
  vars = c(
    "B01003_001E",
    "B01002_001E",
    "B02001_001E",
    "B02001_002E",
    "B02001_003E",
    "B02001_004E",
    "B02001_005E",
    "B02001_006E",
    "B02001_007E",
    "B02001_008E",
    "B19013_001E",
    "B19083_001E",
    "B19301_001E",
    "B25001_001E",
    "B25035_001E",
    "B25040_001E",
    "B25040_002E",
    "B25040_004E",
    "B25040_005E",
    "B25040_006E",
    "B25040_008E",
    "B25058_001E",
    "B25064_001E",
    "B25069_001E",
    "B25069_002E",
    "B25069_003E",
    "B25077_001E",
    "B25002_001E",
    "B25002_003E"
  ),
  region = "tract:*",
  regionin = "state:11",
  show_call = T
)

for (i in 1:length(state.abb)) {
  state.i <- getCensus(
    name = "acs/acs5",
    vintage = 2019,
    vars = c(
      "B01003_001E",
      "B01002_001E",
      "B02001_001E",
      "B02001_002E",
      "B02001_003E",
      "B02001_004E",
      "B02001_005E",
      "B02001_006E",
      "B02001_007E",
      "B02001_008E",
      "B19013_001E",
      "B19083_001E",
      "B19301_001E",
      "B25001_001E",
      "B25035_001E",
      "B25040_001E",
      "B25040_002E",
      "B25040_004E",
      "B25040_005E",
      "B25040_006E",
      "B25040_008E",
      "B25058_001E",
      "B25064_001E",
      "B25069_001E",
      "B25069_002E",
      "B25069_003E",
      "B25077_001E",
      "B25002_001E",
      "B25002_003E"
    ),
    region = "tract:*",
    regionin = paste("state:", fips(state.abb[i]), sep = ""),
    show_call = T
  )
  
  census.df <- rbind(census.df, state.i)
}

census.df <- census.df %>%
  rename(
    pop = B01003_001E,
    med_age = B01002_001E,
    race_total = B02001_001E,
    white = B02001_002E,
    black = B02001_003E,
    native_am = B02001_004E,
    asian = B02001_005E,
    native_ha = B02001_006E,
    other_race = B02001_007E,
    mixed_race = B02001_008E,
    med_hh_income = B19013_001E,
    gini_index = B19083_001E,
    per_capita_income = B19301_001E,
    housing_units = B25001_001E,
    med_year_built = B25035_001E,
    heat_total = B25040_001E,
    heat_gas = B25040_002E,
    heat_elec = B25040_004E,
    heat_oil = B25040_005E,
    heat_coal = B25040_006E,
    heat_solar = B25040_008E,
    med_contract_rent = B25058_001E,
    med_gross_rent = B25064_001E,
    util_in_rent_total = B25069_001E,
    util_in_rent = B25069_002E,
    util_not_in_rent = B25069_003E,
    med_value = B25077_001E,
    hh_occupancy_total = B25002_001E,
    hh_occupancy_vacant = B25002_003E
  )

census.df <- census.df %>%
  mutate(
    GEOID = paste(state, county, tract, sep = ""),
    prop_white = white / race_total,
    prop_nonwhite = 1 - (white / race_total),
    prop_black = black / race_total,
    prop_native_am = native_am / race_total,
    prop_asian = asian / race_total,
    prop_native_ha = native_ha / race_total,
    prop_other = other_race / race_total,
    prop_mixed = mixed_race / race_total,
    prop_heat_gas = heat_gas / heat_total,
    prop_heat_elec = heat_elec / heat_total,
    prop_heat_oil = heat_oil / heat_total,
    prop_heat_coal = heat_coal / heat_total,
    prop_heat_solar = heat_solar / heat_total,
    prop_util_in_rent = util_in_rent / util_in_rent_total,
    prop_util_out_rent = util_not_in_rent / util_in_rent_total,
    vacany_rate = hh_occupancy_vacant / hh_occupancy_total
  )


rm(i, state.i)

#**************************************************************************
## The Final Data Sets ----------------------------------------------------
#**************************************************************************


ct.final <- as_tibble(st_drop_geometry(tracts.within))

ct.final <- with(ct.final, tibble(GEOID, GEOID10, NAME10, UATYP10, ALAND10))

ct.final <-
  merge(
    ct.final,
    main.tracts.df,
    by.x = "GEOID",
    by.y = "fips",
    all.x = T
  )

ct.final <- ct.final %>% mutate(
  tract.count = ifelse(is.na(tract.count), 0, tract.count),
  pub.count = ifelse(is.na(pub.count), 0, pub.count),
  res.count = ifelse(is.na(res.count), 0, res.count),
  com.count = ifelse(is.na(com.count), 0, com.count)
)

ct.final <- merge(ct.final, census.df, by = "GEOID")

# Lastly, some light data cleaning:

ct.final <- ct.final %>%
  mutate(
    med_age = na_if(med_age,-666666666),
    med_hh_income = na_if(med_hh_income,-666666666),
    gini_index = na_if(gini_index,-666666666),
    per_capita_income = na_if(per_capita_income,-666666666),
    med_year_built = na_if(med_year_built,-666666666),
    med_contract_rent = na_if(med_contract_rent,-666666666),
    med_gross_rent = na_if(med_gross_rent,-666666666),
    med_value = na_if(med_value,-666666666)
  )

# This Planning Database is fantastic
# https://www.census.gov/data/developers/data-sets/planning-database.2019.html

temp <- read_csv("planningData2019.csv") %>% 
  select("GIDTR","LAND_AREA")

ct.final <- merge(ct.final, temp, by.x="GEOID",by.y="GIDTR",all.x = T)

rm(temp)

write.csv(ct.final, "tractData.csv", row.names = F)
write.csv(main.df, "pointData.csv", row.names = F)

