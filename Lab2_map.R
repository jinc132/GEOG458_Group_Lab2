library(tidyverse)
library(sf)
library(reshape2)
library(tmap)
library(tmaptools)
library(leaflet)


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
wa_tracts <- "cb_2017_53_tract_500k.shp"

wa_state <- read_shape(file=wa_tracts, as.sf = TRUE)

# Find all the Census Tracts in Bellevue in the wa_tracts shapefile
bellevue <- wa_state %>% 
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
wt_wac <- aggregate(. ~ wac$work_tract,  data= wac[,c(2:52)], FUN=sum)
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


# MAPPING AT BLOCK LEVEL
bellevue$work_tract <- as.character(bellevue$work_tract)

wac_bellevue_block <- merge(x = bellevue, y = wac, by = "work_tract", all.x = TRUE)
wac_bellevue_block$work_tract <- as.character(wac_bellevue_block$work_tract)


tract_level_map <- tm_shape(wac_bell) +
  tm_fill("CNS10", title = "# of Jobs (count)", breaks = c(0, 10, 50, 100, 200, 300, Inf), palette = c("#f1eef6", "#d0d1e6", "#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d")) +
  tm_borders(alpha=0.5) + tm_text("NAME", size = 0.5) + 
  tm_legend(legend.position = c("left", "bottom"),
            legend.title.size=0.8,
            main.title = "# of Jobs in Finance & Insurance\n (Tract Level) in Bellevue", 
            main.title.position = "center",
            main.title.size = 0.8)
 tmap_save(tract_level_map, filename="blocklevel.jpg")
 
 block_level_map <- tm

# MAPPING AT TRACT LEVEL


# # Map at the Block level
# # Join OD data file with WAC data file
# # To do this have to join work census block geocode: w_geocode
# # However, there is no geometry so we have to make a new column
# wac$GEOID <- NA
# wac$GEOID <- wac$w_geocode
# wac$GEOID <- strtrim(wac$GEOID, 11)
# 
# 
# as.data.frame(wa_tracts)
# # Now we have to join the OD data file with the WAC data file
# #wac_join_od <- merge(od, wac, by = "w_geocode")
# wac_join_watracts <- merge(x = wa_tracts, y = wac, by = "GEOID", all.y = TRUE)
# # wac_join_od <- merge(x = od, y = wac, by = "w_geocode", all = TRUE)
# # Map CS01 and CS02

