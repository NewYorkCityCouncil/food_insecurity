source("code/00_load_dependencies.R")
remotes::install_github("walkerke/tidycensus")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 5/21/2024
#
# This file creates a map of food insecurity across the city based on the 
# Mayor's Office of Food Policy data
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
# calculate change for different NTA shapes
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
# create interactive version of SNAP map
################################################################################

# prep for plotting ------------------------------------------------------------
d = c(min(nta_2020$food_insecurity_2023, na.rm=T), 
      max(nta_2020$food_insecurity_2023, na.rm=T))

quantile(nta_2020$food_insecurity_2023, c(0, .2, .4, .6, .8, 1), na.rm=T)*100
pal = colorBin(
  palette = pal_nycc("warm"),
  bins = c(.05, .09, .14, .18, .35),
  domain = d,
  na.color = "grey"
)

nta_2020 = nta_2020 %>%
  mutate(label = paste0("<strong>NTA</strong>: ", nta2020, "<br>",
                        "<strong>NTA name</strong>: ", ntaname, "<br>",
                        "<strong>% food insecure in 2023:</strong> ", round(food_insecurity_2023*100, 1), "%<br>", 
                        "<strong>% point change in food insecurity 2018-2023</strong>: ", 
                            round(perc_point_change_food_insecurity*100, 1), "%")) 

d = c(min(nta_2020$perc_point_change_food_insecurity, na.rm=T), 
      max(nta_2020$perc_point_change_food_insecurity, na.rm=T))
quantile(nta_2020$perc_point_change_food_insecurity, c(0, .2, .4, .6, .8, 1), na.rm=T)

pal2 = colorBin(
  palette = rev(pal_nycc("diverging")),
  bins = c(-5, -3, -2, -1, 1, 2, 3, 5),
  domain = d*100,
  na.color = "grey"
)

map = leaflet() %>% 
  addPolygons(data = nta_2020, weight = 0, color = ~pal2(perc_point_change_food_insecurity*100), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% point change in food insecurity since 2018") %>% 
  addPolygons(data = nta_2020, weight = 0, color = ~pal(food_insecurity_2023), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% food insecure 2023") %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = nta_2020$food_insecurity_2023,
                       title = paste0("% food insecure by <br>", 
                                      "Neighborhood Tabulation Area"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T, 
                       group = "% food insecure 2023") %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                       values = nta_2020$perc_point_change_food_insecurity,
                       title = paste0("% point change in food insecurity <br>", 
                                      "from 2018 to 2023"), 
                       labFormat = labelFormat(suffix = "%"),
                       opacity = 1, decreasing = T, 
                       group = "% point change in food insecurity since 2018") %>%
  addLayersControl(
    baseGroups = c("% food insecure 2023", "% point change in food insecurity since 2018"),
    options = layersControlOptions(collapsed = F, autoZIndex = T))

saveWidget(map, file=file.path('visuals', 
                               "percent_individuals_food_insecure.html"))



