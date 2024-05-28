source("code/00_load_dependencies.R")
remotes::install_github("walkerke/tidycensus")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 5/21/2024
#
# This file creates NTA level data on food insecurity across the city based 
# on the Mayor's Office of Food Policy data
# https://public.tableau.com/app/profile/claire.reynolds6296/viz/SupplyGap2023/NeighborhoodPrioritization2023
################################################################################

################################################################################
# read in data 
################################################################################

old_food_security_data = read_csv(file.path("data", "input", "Neighborhood Prioritization Map 2018.csv")) %>%
  select(Ntacode,  `Food Insecurity Rate_2018`) %>%
  rename(food_insecurity_2018 = `Food Insecurity Rate_2018`, 
         NTA = Ntacode) %>%
  mutate(food_insecurity_2018 = gsub("%", "", food_insecurity_2018), 
         food_insecurity_2018 = as.numeric(food_insecurity_2018)/100)

new_food_security_data = read_csv(file.path("data", "input", "Neighborhood Prioritization Map 2023.csv")) %>%
  select(NTA, `Food Insecure Perc`) %>%
  rename(food_insecurity_2023 = `Food Insecure Perc`) %>%
  mutate(food_insecurity_2023 = gsub("%", "", food_insecurity_2023), 
         food_insecurity_2023 = as.numeric(food_insecurity_2023)/100)


################################################################################
# calculate 2018 food insecurity in 2020 NTA geography
################################################################################

nta_2010_pop = fromJSON("https://data.cityofnewyork.us/resource/rnsn-acs2.json") %>% 
  rename(NTA = geographic_area_neighborhood_tabulation_area_nta_code) %>%
  select(NTA, total_population_2010_number)

nta_2010 = st_read("https://data.cityofnewyork.us/resource/q2z5-ai38.geojson") %>%
  st_transform(st_crs(4326)) %>% 
  merge(old_food_security_data, by.x = "ntacode", by.y = "NTA", all.x = T) %>%
  merge(nta_2010_pop, by.x = "ntacode", by.y = "NTA", all.x = T) %>%
  st_make_valid()

nta_2020 = st_read("https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson") %>%
  st_transform(st_crs(4326)) %>% 
  merge(new_food_security_data, by.x = "nta2020", by.y = "NTA", all.x = T) %>%
  st_make_valid()

# get 2010 NTA food insecure in 2020 geo ---------------------------------------
nta_intersection = st_intersection(nta_2020 %>% 
                                     mutate(area_2020 = as.numeric(st_area(.))), 
                                   nta_2010 %>% 
                                     mutate(area_2010 = as.numeric(st_area(.)))) %>% 
  st_make_valid() %>% 
  mutate(intersection_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  mutate(intersection_area = intersection_area/area_2020,
         intersection_popuation = as.numeric(total_population_2010_number) * intersection_area) %>%
  rename(nta2010 = ntacode) %>% 
  arrange(nta2020) %>% 
  select(nta2010, nta2020, intersection_popuation, intersection_area)

nta_2010_adjusted = nta_2010 %>%
  st_drop_geometry() %>% 
  select(ntacode, food_insecurity_2018) %>%  
  merge(nta_intersection, by.x = "ntacode", by.y = "nta2010", all.x = T) %>%
  group_by(nta2020) %>% 
  summarise(food_insecurity_2018 = weighted.mean(food_insecurity_2018, intersection_area))

nta_2020 = nta_2020 %>% 
  select(nta2020, ntaname, food_insecurity_2023) %>% 
  merge(nta_2010_adjusted, all.x=T) %>% 
  mutate(perc_point_change_food_insecurity = food_insecurity_2023 - food_insecurity_2018)

################################################################################
# get 2020 NTA population
################################################################################

pop = tidycensus::get_decennial(geography = "block", 
                                state = "NY",
                                county = c("061", "081", "085", "005", "047"),
                                variables = "P1_001N", 
                                year = 2020, 
                                geometry = T) %>%
  st_transform(st_crs(4326))

# figure out what blocks are within the pool isochrone
block_intersection = pop %>%
  st_make_valid() %>%
  mutate(area = as.numeric(st_area(.))) %>% 
  st_intersection(nta_2020) %>% 
  st_make_valid() %>%
  mutate(area_overlap = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  mutate(perc_overlap = area_overlap/area) %>%
  select(GEOID, nta2020, perc_overlap)
  
pop = pop %>% 
  st_drop_geometry() %>% 
  select(GEOID, value) %>% 
  rename(population = value) %>% 
  merge(block_intersection, by = "GEOID") %>% 
  group_by(nta2020) %>% 
  summarise(population = sum(population*perc_overlap))

nta_2020 = nta_2020 %>%
  merge(pop, by = "nta2020", all = T)

################################################################################
# calculate CFC locations by NTA
################################################################################

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS")) %>%
  st_as_sf(coords = c("lon","lat")) %>% # create points
  st_set_crs(st_crs(4326))

# count the number of cfc locations that fall in each community district and normalize by population
nta_2020$cfc_count = lengths(st_intersects(nta_2020, cfc_locations))
nta_2020 = nta_2020 %>%
  mutate(cfc_per100k = cfc_count/population*100000, 
         cfc_per10k_fi = cfc_count/(population*food_insecurity_2023)*10000)



################################################################################
# save data
################################################################################

saveRDS(nta_2020, 
        file.path("data", "output", "nta_data.RDS"))
