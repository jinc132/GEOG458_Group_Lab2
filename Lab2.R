library(tidyverse)
library(tigris)
library(sf)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Part 1
od <- read.csv('wa_od_main_JT00_2015.csv')
rac <- read.csv('wa_rac_S000_JT00_2015.csv')
wac <- read.csv('wa_wac_S000_JT00_2015.csv')

wa_tract <- tracts(state = '53', county = '033')
cb <- core_based_statistical_areas(cb = TRUE)

bell <- block_groups("Washington", "Bellevue")

# Creates a copy of original 
copy <- od

