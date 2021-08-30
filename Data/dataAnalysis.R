# Fianl Data Analysis Script
# Spellman Fellow Program
# Evan Perry
# July 2021

# install.packages("tmap")
# install.packages("sf")
# install.packages("tidyverse")
# install.packages("tigris")
# install.packages("usmap")

library(tmap)
library(sf)
library(tidyverse)
library(tigris)
library(usmap)
library(RColorBrewer)
library(stargazer)
library(stringr)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# *******************************************
# Data Importing and Cleaning Some More -----
# *******************************************

pointData <- read_csv("pointData.csv")
tractData <- read_csv("tractData.csv")
cellphoneData <- read_csv("cellphoneDataReduced.csv")
cities <- read_csv("ua_list_ua.csv") %>%  select("UACE","POP") %>% 
  mutate(
    UACE = str_pad(UACE, width = 5, pad = "0")
  )

c_pointData <- pointData %>% filter(build.type == "Commercial") %>% 
  mutate(
    leed.floor.area = ifelse(is.na(leed.floor.area), 0, leed.floor.area),
    estar.floor.area = ifelse(is.na(estar.floor.area), 0, estar.floor.area)
  )

green.floor.area <- with(c_pointData, pmax(leed.floor.area, estar.floor.area))

c_pointData <- cbind(c_pointData, green.floor.area)

c_pointData <- c_pointData %>% group_by(fips) %>% 
  summarise(sum(green.floor.area))

main_data <- merge(cellphoneData, c_pointData, by.x = "tract_geoid", by.y="fips", all.x = T) %>% 
  rename(
    green.space = "sum(green.floor.area)",
    work_cells = "sum(work_hours_count)"
  ) %>% 
  mutate(
    green.space = ifelse(is.na(green.space), 0, green.space),
    work_cells = work_cells
  ) %>% 
  filter(work_cells != 0)

main_data <- merge(main_data, tractData, by.x ="tract_geoid", by.y = "GEOID") %>% 
  filter(LAND_AREA > 0, UATYP10 == "U") %>% 
  mutate(
    work_density = work_cells/LAND_AREA
  )

main_data <- merge(main_data, cities, by.x = "GEOID10", by.y = "UACE")

rm(green.floor.area, pointData, tractData, cellphoneData, c_pointData, cities)


# *******************************************
# How to handle the 0 log problem? ----------
# *******************************************

strat1 <- main_data %>% mutate(
  green.space = green.space + 1,
  green.space.per.worker = green.space/work_cells
)

# Literally just give up
# strat2 <- main_data %>% filter(
#   green.space != 0
# ) %>% mutate(
#   green.space.per.worker = green.space/work_cells
# )

# *******************************************
# Final calculations ------------------------
# *******************************************

df <- strat1 %>% 
  mutate(
    lnGS = log(green.space),
    lnWorkers = log(work_cells),
    lnWorkerDensity = log(work_density),
    lnHHIncome = log(med_hh_income),
    lnHHvalue = log(med_value),
    lnGSperWorker = log(green.space.per.worker),
    lnAge = log(med_age),
    work_cells = work_cells/100
  )

temp_df <- df %>% group_by(GEOID10) %>% 
  count()

df <- merge(df, temp_df, by = "GEOID10") %>% 
  filter(n >= 15)

# *******************************************
# Running Regressions -----------------------
# *******************************************

spec1 <- lm(lnGS ~ lnWorkers + lnWorkerDensity:GEOID10, df)
spec2 <- lm(lnGS ~ lnWorkers + lnWorkerDensity:GEOID10 + lnHHvalue + lnAge, df)

final_reg <- spec2

names <- row.names(summary(final_reg)$coefficients)
coefs <- summary(final_reg)$coefficients[,1]
pvalues <- summary(final_reg)$coefficients[,4]

df2 <- tibble(names, coefs, pvalues) %>% mutate(
  sig = ifelse(pvalues < .01, 3, 
          ifelse(pvalues < .05, 2, 0))
) %>% filter(
  !(names %in% c("(Intercept)","lnWorkers","lnHHvalue","lnAge"))
)

summary(spec2)

ggplot(df2, aes(x = coefs, fill = as.factor(sig)))+
  geom_histogram(bins = 20)


# *******************************************
# Making the table --------------------------
# *******************************************

stargazer(spec1, spec2,
   covariate.labels = c("Log Number of Workers", "Log Median Housing Value", "Log Median Resident Age", "t"),
   title = "\\textsc{Testing for Agglomeration Effects}",
   dep.var.caption = "",
   dep.var.labels = "Log Green Real Estate (sq. ft.)",
   omit.stat = c("ser", "f"),
   digits = 3,
   style = "aer",
   keep = 1:4
)

# *******************************************

# In the final poster, I create the histogram in pgfplots, where it is somewhat 
# easier (although messier) to find the coordinates to the histograms. The 
# following code gives the coordinates needed for the histogram to be made in
# pgfplots.

# df3 <- df2 %>% filter(pvalues < .05)
# df4 <- df2 %>% filter(pvalues < .01)
# 
# r1 <- hist(df2$coefs, breaks = 20)
# 
# x <- r1$breaks
# y <- c(r1$counts, 0)
# 
# for (i in 1:length(x)){
#   cat('(', x[i], ",", y[i], ')', sep="")
# }
# 
# r2 <- hist(df3$coefs, breaks = 15)
# 
# x <- r2$breaks
# y <- c(r2$counts, 0)
# 
# for (i in 1:length(x)){
#   cat('(', x[i], ",", y[i], ')', sep="")
# }
#
# r3 <- hist(df4$coefs, breaks = 15)
# 
# x <- r3$breaks
# y <- c(r3$counts, 0)
# 
# for (i in 1:length(x)){
#   cat('(', x[i], ",", y[i], ')', sep="")
# }


