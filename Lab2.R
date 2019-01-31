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

# Returns the merged data
mergeIt <- function(data) {
  return (merge(bellevue, data, by = "work_tract", all.x = TRUE))
}

# Part 2 
## OD data
# setting and cleaning the table 
od$w_geocode <- as.character(od$w_geocode)
od$h_geocode <- as.character(od$h_geocode)
od$w_geocode[which(length(od$w_geocode) == 14)] <- paste0("0",od$w_geocode[which(length(od$w_geocode) == 14)])
od$h_geocode[which(length(od$h_geocode) == 14)] <- paste0("0",od$h_geocode[which(length(od$h_geocode) == 14)])

# Creating columns for worktract and hometract
od$work_tract <- substr(od$w_geocode, 1, 11)
od$home_tract <- substr(od$h_geocode, 1, 11)

# Returns an aggregate dataset with the specified data
aggregateIt <- function(indexs) {
  return (aggregate(. ~ work_tract + home_tract, data = indexs, FUN=sum))
}

# Aggregating WTU occupations by work_tract
## Trade, Transportation, and Utilities jobs
wtutract <- aggregateIt(od[,c(11,14,15)])
## Goods Producing industry jobs
mantract <- aggregateIt(od[,c(10,14,15)])
## Total number of jobs
total <- aggregateIt(od[,c(3,14,15)])
## All the data compiled together
overall <- aggregateIt(od[,c(3,10,11,14,15)])

# renaming a work tract column
colnames(bellevue)[5] <- "work_tract"

# Joining the data to only show Bellevue data
wtu_tract <- mergeIt(wtutract)
man_tract <- mergeIt(mantract)
total_tract <- mergeIt(total)
overall_bell <- mergeIt(overall)

overall_bell["WTUShare"] <- overall_bell$SI02 / overall_bell$S000
overall_bell["MANShare"] <- overall_bell$SI01 / overall_bell$S000

# Creating block level 
block<- unique(od[,1:2])
nrow(check) == nrow(od)

# Added a count column for the number of sending blocks
block$count <- 1

# Get number of blocks "sending" jobs to work block
send_wb <- aggregate(. ~ w_geocode, data=block[,-2], FUN=sum)

# Use sum job variable S000 to get count of "jobs" to work block
jobs_wb <- aggregate(. ~ w_geocode, data=od[,c(1,3)], FUN=sum)

# Merge the two dataframes together by work block FIPS
work_block <- merge(send_wb, jobs_wb, by="w_geocode")

# Creates a file with home_tract and w_geocode
s <- unique(od[,c(1,15)])

# Added a count column for the number of sending tracts
s$tract_ct <- 1

# Number of tracts "sending" jobs to work block
s <- aggregate(. ~ w_geocode, data=s[,c(1,3)], FUN=sum)
work_block <- merge(work_block, s, by="w_geocode")

createMatrix <- function(data, key) {
  d <- acast(data, work_tract ~ home_tract, value.var = key)
  d[is.na(d)] <- 0
  return (d)
}

# Produce a matrix with a worktract as ROW and hometract as COLUMN
wtod_tract <- createMatrix(wtu_tract, "SI02")
manod_tract <- createMatrix(man_tract, "SI01")
total_tract <- createMatrix(total_tract, "S000")

## WAC

