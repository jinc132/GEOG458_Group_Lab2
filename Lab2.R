library(tidyverse)
library(tigris)
library(sf)
library(reshape2)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

# Part 1
od <- read.csv('wa_od_main_JT00_2015.csv')
rac <- read.csv('wa_rac_S000_JT00_2015.csv')
wac <- read.csv('wa_wac_S000_JT00_2015.csv')
bellevue_tnums <- read.csv('Data USA - Geo Map of Income by Location in Bellevue, Wa.csv')

# Retrieve all the tract numbers and clean it of duplicates
tnums <-  gsub("Census Tract ","", bellevue_tnums$geo_name)
tnums <- unique(tnums)

# Read in Bellevue city boundary shape file
wa_tracts <- st_read("cb_2017_53_tract_500k.shp")

# Find all the Census Tracts in Bellevue in the wa_tracts shapefile
bellevue <- wa_tracts %>% 
  filter(NAME %in% tnums)


# Part 2
## OD data
# setting and cleaning the table 
od$w_geocode <- as.character(od$w_geocode)
od$h_geocode <- as.character(od$h_geocode)
od <- od %>%
  mutate(w_geocode = replace(w_geocode, length(w_geocode) == 14, "0"),
         h_geocode = replace(h_geocode, length(w_geocode) == 14, "0"))

# Creating columns for worktract and hometract
od$work_tract <- substr(od$w_geocode, 1, 11)
od$home_tract <- substr(od$h_geocode, 1, 11)

# renaming a work tract column
colnames(bellevue)[5] <- paste("work_tract")

# Joining the data to only show Bellevue data
overall_bell <- merge(bellevue, od, by ="work_tract", all.x = TRUE)

# Creating block level and check for duplicates
block<- unique(od[,1:2])
nrow(block) == nrow(od)

## Added a count column for the number of sending blocks
block$block_ct <- 1

## Get number of blocks "sending" jobs to work block
work_block <- aggregate(. ~ w_geocode, data=block[,-2], FUN=sum)

## Use sum job variable S000 to get count of "jobs" to work block
jobs_wb <- aggregate(. ~ w_geocode, data=od[,c(1:3)], FUN=sum)

## Merge the two dataframes together by work block FIPS
work_block <- merge(work_block, jobs_wb, by="w_geocode")

## Creates a file with home_tract and w_geocode
home_block <- unique(od[,c(1,15)])

## Added a count column for the number of sending tracts
home_block$tract_ct <- 1

## Number of tracts "sending" jobs to work block
home_block <- aggregate(. ~ w_geocode, data=home_block[,c(1,3)], FUN=sum)
final_block <- merge(work_block, home_block, by="w_geocode")

# Produce a matrix with a worktract as rows and hometract as columns
bell_relate <- overall_bell %>%
  select(work_tract, home_tract, S000, SA01, SA02, SE01, SE02, SE03, SI01, SI02, SI03)

total_tract <- dcast(bell_relate, work_tract ~ home_tract, value.var = "S000")
total_tract[is.na(total_tract)] <- 0

write.table(total_tract,"total_tract.csv")

# WAC
wac$w_geocode <- as.character(wac$w_geocode)
wac <- wac %>%
  mutate(w_geocode = replace(w_geocode, length(w_geocode) == 14, "0"))

# Make columns for work and home tract
wac$work_tract <- substr(wac$w_geocode, 1, 11)

# Aggregate the data (total number of: jobs, utilites, manufacturing, 
# wholesale trade, transportation and warehousing)
wt_wac <- aggregate(. ~ wac$work_tract,  data= wac[,c(2,11,13,14,16)], FUN=sum)
colnames(wt_wac)[1] <- "work_tract"

# Merge the data so its data pertains to bellevue tract area
wac_bell <- merge(bellevue, wt_wac, by ="work_tract", all.x = TRUE)

## RAC
rac$h_geocode <- as.character(rac$h_geocode)
rac <- rac %>%
  mutate(h_geocode = replace(h_geocode, length(h_geocode) == 14, "0"))

# Make columns for home tract
rac$home_tract <- substr(rac$h_geocode, 1, 11)

# Aggregate the data (total number of: jobs, utilites, manufacturing, 
# wholesale trade, transportation and warehousing)
ht_rac <- aggregate(. ~rac$home_tract,  data= rac[,c(2,11,13,14,16)], FUN=sum)



