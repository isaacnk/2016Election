library(sp) #Working with spatial data
library(rgdal) #Working with spatial data
library(tidyverse) #Data wrangling and cleaning
library(tmap) #Data visualization

election.results <- read.csv("countypres_2000-2016.csv")
us.counties <- readOGR("2016-us-election/county_shapefiles")

glimpse(election.results)

glimpse(us.counties@data)

#Select only 2016 election data
election.2016 <- election.results %>%
  dplyr::filter(year == 2016)

#Recast candidate from factor to string
election.2016$candidate <- as.character(election.2016$candidate)

#Remove other candidates
election.2016.2 <- election.2016 %>% 
  dplyr::filter(candidate %in% c("Hillary Clinton", "Donald Trump"))

hillary <- election.2016.2 %>%
  filter(candidate == "Hillary Clinton") %>% 
  mutate(pct_dem = candidatevotes / totalvotes)

trump <- election.2016.2 %>%
  filter(candidate == "Donald Trump") %>% 
  mutate(pct_rep = candidatevotes / totalvotes)

tail(hillary)
tail(trump)

### Remove entries with NA FIPS
trump <- trump[which(!is.na(trump$FIPS)),]
hillary <- hillary[which(!is.na(hillary$FIPS)),]

final.2016 <- data.frame(FIPS = hillary$FIPS, pct_hillary = hillary$pct_dem, pct_trump = trump$pct_rep)
glimpse(final.2016)

fips <- as.integer(paste(us.counties@data$STATEFP, us.counties@data$COUNTYFP, sep = ""))
us.counties$FIPS <- fips

county.elections <- sp::merge(us.counties, final.2016, by = "FIPS")

writeOGR(county.elections,".", "county_level_election_results_2016", driver = "ESRI Shapefile")





