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
wa_tracts <- st_read("cb_2017_53_tract_500k.shp")

# List of all the Census Tracts in Bellevue
bellevue_tracts <- c(227.01, 227.03, 228.01, 228.03, 229.01, 230, 231, 232.01,
                    232.02, 233, 234.01, 234.03, 234.04, 235, 236.01, 236.03,
                    236.04, 237, 238.01, 238.03, 238.04, 239, 240, 247.01, 247.02, 248, 249.01,
                    249.02, 249.03, 250.01, 250.03, 250.05, 250.06)

# Find all the Census Tracts in Bellevue in the wa_tracts shapefile
bellevue <- wa_tracts %>% 
  filter(NAME %in% bellevue_tracts)

# Create the King County census tract shape file
wa_tract <- tracts(state = '53', county = '061')
shape <- shape[shape$NAME == "BELLEVUE",]

copy <- od


#Part 2 