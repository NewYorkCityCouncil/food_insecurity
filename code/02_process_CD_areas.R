source("code/00_load_dependencies.R")
library(councilverse)

################################################################################
# Created by: Anne Driscoll
# Last edited on: 5/13/2024
#
# This file processes community district SNAP data + council district CFC data
################################################################################


################################################################################
# read in current & pre-pandemic SNAP data
################################################################################

snap = fromJSON('https://data.cityofnewyork.us/resource/5awp-wfkt.json?$limit=1500000')

current_snap_population = snap %>%
  filter(month == '2024-09-01T00:00:00.000')

# recode and fix datatypes as numeric
current_snap_population = current_snap_population %>%
  mutate(cd = gsub("M", "1", community_district), 
         cd = gsub("B", "2", cd),
         cd = gsub("K", "3", cd),  
         cd = gsub("Q", "4", cd), 
         cd = gsub("S", "5", cd), 
         bc_snap_recipients = as.numeric(bc_snap_recipients), 
         bc_snap_households = as.numeric(bc_snap_households)) 

pre_snap_population = snap %>%
  filter(month == '2019-09-01T00:00:00.000')

# recode and fix datatypes as numeric
pre_snap_population = pre_snap_population %>%
  mutate(cd = gsub("M", "1", community_district), 
         cd = gsub("B", "2", cd),
         cd = gsub("K", "3", cd),  
         cd = gsub("Q", "4", cd), 
         cd = gsub("S", "5", cd), 
         bc_snap_recipients = as.numeric(bc_snap_recipients), 
         bc_snap_households = as.numeric(bc_snap_households)) 

# nyc dcp planning population data source
# https://www.nyc.gov/site/planning/planning-level/nyc-population/2020-census.page
cd_population = readxl::read_xlsx(file.path("data", "input", 
                                            "nyc_decennialcensusdata_2010_2020_change.xlsx"), 
                                  sheet = "2010, 2020, and Change", skip = 3) %>%
  filter(`CD Type` == "CD") %>%
  select(GeoID, Pop_20) %>%
  rename(cd = GeoID, pop = Pop_20)

# get neighborhood names for each community district
cd_names = read_csv("https://data.cityofnewyork.us/resource/ruf7-3wgc.csv") %>%
  select(community_board_1, neighborhoods) %>%
  rename(cd = community_board_1)

# get spatial boundaries, join, create normalized values & percent change
community_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326)) %>% 
  merge(current_snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(pre_snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(cd_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  mutate(perc_snap_cur = bc_snap_recipients.x/pop, 
         perc_snap_pre = bc_snap_recipients.y/pop, 
         borough_letter = substr(borough.x, 1, 1), 
         borough_letter = ifelse(borough.x == "Brooklyn", "K", borough_letter), 
         perc_change_snap = round(((bc_snap_recipients.x-bc_snap_recipients.y)/bc_snap_recipients.y)*100, 0), 
         perc_point_change_snap = round((perc_snap_cur-perc_snap_pre)*100, 1)) %>%
  drop_na(perc_snap_cur) %>%
  merge(cd_names, by.x = "BoroCD", by.y = "cd", all.x = T)


saveRDS(community_districts, 
        file.path("data", "output", "community_district_data.RDS"))

################################################################################
# read in CFC data
################################################################################

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS")) %>%
  st_as_sf(coords = c("lon","lat")) %>% # create points
  st_set_crs(st_crs(4326))

# count the number of cfc locations that fall in each community district and normalize by population
community_districts$cfc_count = lengths(st_intersects(community_districts, cfc_locations))
community_districts = community_districts %>%
  mutate(cfc_per100k = cfc_count/pop*100000)

# get number of cfc locations by council district for static map for committee report 
council_districts$cfc_count = lengths(st_intersects(council_districts, cfc_locations))

saveRDS(council_districts, 
        file.path("data", "output", "council_district_data.RDS"))
