source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/13/2023
#
# This file processes community district SNAP data + council district CFC data
################################################################################


################################################################################
# read in SNAP data
################################################################################

current_snap_population = fromJSON("https://data.cityofnewyork.us/resource/jye8-w4d7.json?$query=SELECT%20%60month%60%2C%20%60boro%60%2C%20%60cd%60%2C%20%60bc_snap_recipients%60%2C%20%60bc_snap_households%60%0AWHERE%20%60month%60%20%3D%20%222022-12-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0AORDER%20BY%20%60month%60%20DESC%20NULL%20LAST")
current_snap_population = current_snap_population %>%
  mutate(og_cd =cd, 
         cd = gsub("M", "1", cd), 
         cd = gsub("B", "2", cd),
         cd = gsub("K", "3", cd),  
         cd = gsub("Q", "4", cd), 
         cd = gsub("S", "5", cd), 
         bc_snap_recipients = as.numeric(bc_snap_recipients), 
         bc_snap_households = as.numeric(bc_snap_households)) 

pre_pandemic_snap_population = fromJSON("https://data.cityofnewyork.us/resource/jye8-w4d7.json?$query=SELECT%20%60month%60%2C%20%60boro%60%2C%20%60cd%60%2C%20%60bc_snap_recipients%60%2C%20%60bc_snap_households%60%0AWHERE%20%60month%60%20%3D%20%222019-12-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0AORDER%20BY%20%60month%60%20DESC%20NULL%20LAST")
pre_pandemic_snap_population = pre_pandemic_snap_population %>%
  mutate(og_cd = cd, 
         cd = gsub("M", "1", cd), 
         cd = gsub("B", "2", cd),
         cd = gsub("K", "3", cd),  
         cd = gsub("Q", "4", cd), 
         cd = gsub("S", "5", cd), 
         bc_snap_recipients = as.numeric(bc_snap_recipients), 
         bc_snap_households = as.numeric(bc_snap_households)) 


cd_population = readxl::read_xlsx(file.path("data", "input", 
                                            "nyc_decennialcensusdata_2010_2020_change.xlsx"), 
                                  sheet = "2010, 2020, and Change", skip = 3) %>%
  filter(`CD Type` == "CD") %>%
  select(GeoID, Pop_20) %>%
  rename(cd = GeoID, pop = Pop_20)

cd_names = read_csv("https://data.cityofnewyork.us/resource/ruf7-3wgc.csv") %>%
  select(community_board_1, neighborhoods) %>%
  rename(cd = community_board_1)

community_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326)) %>% 
  merge(current_snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(pre_pandemic_snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(cd_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  mutate(perc_snap_cur = bc_snap_recipients.x/pop, 
         perc_snap_pre = bc_snap_recipients.y/pop, 
         borough_letter = substr(boro.x, 1, 1), 
         borough_letter = ifelse(boro.x == "Brooklyn", "K", borough_letter), 
         perc_change_snap = round(((bc_snap_recipients.x-bc_snap_recipients.y)/bc_snap_recipients.y)*100, 0)) %>%
  drop_na(perc_snap_cur) %>%
  merge(cd_names, by.x = "BoroCD", by.y = "cd", all.x = T)


################################################################################
# read in CFC data
################################################################################

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS")) %>%
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))

community_districts$cfc_count = lengths(st_intersects(community_districts, cfc_locations))
community_districts = community_districts %>%
  mutate(cfc_per100k = cfc_count/pop*100000) %>%
  merge(cd_names, by.x = "BoroCD", by.y = "cd", all.x = T)

council_districts$cfc_count = lengths(st_intersects(council_districts, cfc_locations))


################################################################################
# save data for mapping
################################################################################

saveRDS(community_districts, 
        file.path("data", "output", "community_district_data.RDS"))

saveRDS(council_districts, 
        file.path("data", "output", "council_district_data.RDS"))
