library(tidyverse)
library(tigris)
library(sf)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Part 1
od <- read.csv('wa_od_main_JT00_2015.csv')
rac <- read.csv('wa_rac_S000_JT00_2015.csv')
wac <- read.csv('wa_wac_S000_JT00_2015.csv')

# Read in Bellevue city boundary shape file
wa_tracts <- st_read("cb_2017_53_tract_500k.shp")

# List of all the Census Tracts in Bellevue
bellevue_tracts <- c(227.01, 227.03, 228.01, 228.03, 229.01, 230, 231, 232.01,
                     232.02, 233, 234.01, 234.03, 234.04, 235, 236.01, 236.03,
                     236.04, 237, 238.01, 238.03, 238.04, 239, 240, 247.01, 247.02, 248, 249.01,
                     249.02, 249.03, 250.01, 250.03, 250.05, 250.06)

# Find all the Census Tracts in Bellevue in the wa_tracts shapefile
bellevue <- wa_tracts %>% 
  filter(NAME %in% bellevue_tracts)

# Part 2 
# setting and cleaning the table 
od$w_geocode <- as.character(od$w_geocode)
od$h_geocode <- as.character(od$h_geocode)
od$w_geocode[which(length(od$w_geocode) == 14)] <- paste0("0",od$w_geocode[which(length(od$w_geocode) == 14)])
od$h_geocode[which(length(od$h_geocode) == 14)] <- paste0("0",od$h_geocode[which(length(od$h_geocode) == 14)])

# Creating columns for worktract and hometract
od$work_tract <- substr(od$w_geocode, 1, 11)
od$home_tract <- substr(od$h_geocode, 1, 11)

# Aggregating WTU occupations by work_tract
## Trade, Transportation, and Utilities jobs
wtutract <- aggregate(. ~ work_tract + home_tract, data = od[,c(11,14,15)], FUN=sum)
## Goods Producing industry jobs
mantract <- aggregate(. ~ work_tract + home_tract, data = od[,c(10,14,15)], FUN=sum)
## Total number of jobs
total <- aggregate(. ~ work_tract + home_tract, data = od[,c(3,14,15)], FUN=sum)
## All the data compiled together
overall <- aggregate(.~ work_tract + home_tract, data = od[,c(3,10,11,14,15)], FUN=sum)

# renaming a work tract column
colnames(bellevue)[1] <- "work_tract"

# Joing the data to only show Bellevue data
wt_tract <- merge(bellevue, wtutract, by= "work_tract")
man_tract <- merge(bellevue, mantract, by= "work_tract")
total_tract <- merge(bellevue, total, by= "work_tract")
overall_bell <- merge(bellevue, overall, by= "work_tract")

