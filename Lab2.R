library(tidyverse)
#install.packages('tigris')
install.packages('sf')
library(tigris)

options(tigris_class = "sf")

# Part 1
od <- read.csv('wa_od_main_JT00_2015.csv')
rac <- read.csv('wa_rac_S000_JT00_2015.csv')
wac <- read.csv('wa_wac_S000_JT00_2015.csv')

