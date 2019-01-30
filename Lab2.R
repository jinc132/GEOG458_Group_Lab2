library(tidyverse)
library(tigris)
library(sf)
library(raster)
library(rgeos)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Part 1
od <- read.csv('wa_od_main_JT00_2015.csv')
rac <- read.csv('wa_rac_S000_JT00_2015.csv')
wac <- read.csv('wa_wac_S000_JT00_2015.csv')

# Read in Bellevue city boundary shape file
shape <- st_read("CITYBDY.shp")

# Create the King County census tract shape file
wa_tract <- tracts(state = '53', county = '061')
shape <- shape[shape$NAME == "BELLEVUE",]

copy <- od

#Part 2 