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
shape <- st_read("CITYBDY.shp")
shape <- shape %>%
  filter(NAME = "BELLEVUE")

wa_tract <- tracts(state = '53', county = '061')
shape <- shape[shape$NAME == "BELLEVUE",]

# Creates a copy of original 
copy <- od

