library(tidyverse)
library(sf)
library(reshape2)
install.packages("tmap")
install.packages("tmaptools")
library(tmap)
library(tmaptools)

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

# Creates and returns a dataframe of the relationship between hometract and
# work tract with the specified key.
CreateDf <- function(key) {
  df <- dcast(bell_relate, work_tract ~ home_tract, value.var = key)
  df[is.na(df)] <- 0
  return (df)
}

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
bell_block <- overall_bell %>%
  select(w_geocode, h_geocode, S000, work_tract, home_tract)
st_geometry(bell_block) <- NULL

block<- unique(bell_block[,1:2])
nrow(block) == nrow(bell_block)

## Added a count column for the number of sending blocks
block$block_ct <- 1

## Get number of blocks "sending" jobs to work block
work_block <- aggregate(. ~ w_geocode, data=block[,-2], FUN=sum)

## Use sum job variable S000 to get count of "jobs" to work block
jobs_wb <- aggregate(. ~ w_geocode, data=bell_block[,c(1,3)], FUN=sum)

## Merge the two dataframes together by work block FIPS
work_block <- merge(work_block, jobs_wb, by="w_geocode")

## Creates a data frame with home_tract and w_geocode
block_w <- unique(bell_block[,c(1,5)])

## Added a count column for the number of sending tracts
block_w$tract_ct <- 1

## Number of tracts "sending" jobs to work block and home block
block_w <- aggregate(. ~ w_geocode, data=block_w[,c(1,3)], FUN=sum)
final_wblock <- merge(work_block, block_w, by="w_geocode")

# Produce a dataframe with a worktract as rows and hometract as columns
bell_relate <- overall_bell %>%
  select(work_tract, home_tract, S000, SA01, SA02, SA03, SE01, SE02, SE03, SI01, SI02, SI03)

## Total number of jobs dataframe
tract_tot <- CreateDf("S000")
## Number of jobs of works age 29 or younger 
tract_y <- CreateDf("SA01")
## Number of jobs for worker age 30 to 54
tract_m <- CreateDf("SA02")
## Number of jobs for workers age 55 or older
tract_o <- CreateDf("SA03")
## Number of jobs for with earnings $1250/month or less
tract_lin <- CreateDf("SE01")
## Number of jobs with earnings $1251/month to $3333/month
tract_min <- CreateDf("SE02")
## Number of jobs with earnings greater than $3333/month
tract_hin <- CreateDf("SE03")
## Number of jobs in goods producing industry sector
tract_gds <- CreateDf("SI01")
## Number of jobs in trade, transportation, and utilties industry sector
tract_tu <- CreateDf("SI02")
## Number of jobs in all other services industry sectors
tract_all <- CreateDf("SI03")

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

one <- ExtractTracts(tract_all)
two <- ExtractTracts(tract_gds)
three <- ExtractTracts(tract_hin)
four <- ExtractTracts(tract_lin)
five <- ExtractTracts(tract_m)
six <- ExtractTracts(tract_min)
sev<- ExtractTracts(tract_o)
eit <- ExtractTracts(tract_tot)
nine <- ExtractTracts(tract_tu)
ten <- ExtractTracts(tract_y)

ExtractTracts <- function(data) {
  data$work_tract <- as.character(data$work_tract)
  data$name <- substr(data$work_tract, nchar(data$work_tract) - 4, nchar(data$work_tract))
  selected <- data[grep('23201|23202|23403|23804', data$name),]
  
  return(selected)
}

one <- ExtractTracts(tract_all)
two <- ExtractTracts(tract_gds)
three <- ExtractTracts(tract_hin)
four <- ExtractTracts(tract_lin)
five <- ExtractTracts(tract_m)
six <- ExtractTracts(tract_min)
sev <- ExtractTracts(tract_o)
eit <- ExtractTracts(tract_tot)
nine <- ExtractTracts(tract_tu)
ten <- ExtractTracts(tract_y)

ExtractTracts <- function(data) {
  data$work_tract <- as.character(data$work_tract)
  data$name <- substr(data$work_tract, nchar(data$work_tract) - 4, nchar(data$work_tract))
  selected <- data[grep('23201|23202|23403|23804', data$name),]
  
  return(selected)
}
master <- data.frame(one$work_tract)
colnames(master) <- "work_tract"
master$S000 <- rowSums(one[c(2:1455)])
master$SA01 <- rowSums(two[c(2:1455)])
master$SA02 <- rowSums(three[c(2:1455)])
master$SA03 <- rowSums(four[c(2:1455)])
master$SE01 <- rowSums(five[c(2:1455)])
master$SE02 <- rowSums(six[c(2:1455)])
master$SE03 <- rowSums(sev[c(2:1455)])
master$SI01 <- rowSums(eit[c(2:1455)])
master$SI02 <- rowSums(nine[c(2:1455)])
master$SI03 <- rowSums(ten[c(2:1455)])

wa_tracts$work_tract <- wa_tracts$GEOID
cleaneddata <- merge(wa_tracts, master, by = "work_tract", all.x=TRUE)
finalcomplete <- cleaneddata %>% filter(work_tract == "53033023201" |work_tract == "53033023202" |work_tract == "53033023403" |work_tract == "53033023804")
#53033023201, 53033023202, 53033023403, 53033023804

qtm(finalcomplete, fill = "blue", title = "Census Tracts in Downtown - Work Tracts")

tm_shape(cleaneddata) + tm_polygons("work_tract") + tm_shape(cleaneddata) + tm_polygons("S000") + tmap_options(max.categories = 1454)


