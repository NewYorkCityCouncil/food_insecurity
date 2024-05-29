source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/12/2023
#
# This file creates a map of CFC locations by council district
################################################################################


################################################################################
# read in data
################################################################################

council_districts = read_rds(file.path("data", "output", 
                                       "council_district_data.RDS"))

community_districts = read_rds(file.path("data", "output", 
                                         "community_district_data.RDS"))

ntas = read_rds(file.path("data", "output", "nta_data.RDS")) %>%
  mutate(cfc_per10k_fi = ifelse(cfc_count == 0, 0, cfc_per10k_fi))

parks = st_read("https://data.cityofnewyork.us/resource/enfh-gkve.geojson?$where=acres>=100&$limit=999999999999") 

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS")) %>%
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))


################################################################################
# prep for plotting community district
################################################################################

# prep for plotting
d = c(min(ntas$cfc_per10k_fi, na.rm=T), 
      max(ntas$cfc_per10k_fi, na.rm=T))

breaks = classInt::classIntervals(ntas$cfc_per10k_fi, n = 6, style = 'jenks', na.rm=T)$brks
breaks[length(breaks)] = breaks[length(breaks)] + 0.5
breaks = round(breaks, 0)
quantile(ntas$cfc_per10k_fi, c(0, .2, .4, .6, .8, 1), na.rm=TRUE)

pal = colorBin(
  palette = rev(pal_nycc("warm")),
  bins = c(0, 2, 4, 6, 30),
  domain = ntas$cfc_per10k_fi,
  na.color = "grey"
)

pal2 = colorFactor(
  palette = pal_nycc("cool"), 
  domain = c("Multiple", "Food Pantry", "Soup Kitchen"),
  na.color = "transparent"
)

cfc_locations = cfc_locations %>%
  mutate(label = paste0("<strong>Program Name:</strong> ", PROGRAM, "<br>",
                        "<strong>Program Type:</strong> ", TYPE, "<br>",
                        "<strong>Open Times:</strong> ", DAYS, "<br>",
                        "<strong>Address:</strong> ", address))


################################################################################
# plot interactive map - # of CFC locations per 10k food insecure NTA residents
################################################################################

# plot
map = leaflet() %>% 
  addPolygons(data = ntas , 
              weight = 0, color = ~pal(cfc_per10k_fi), 
              fillOpacity = 1, smoothFactor = 0, 
              popup = ~paste0("<strong>", round(cfc_per10k_fi, 1), 
                              "</strong> CFC locations per 10k food insecure residents<br>", 
                              "<strong>CFC locations:</strong> ", cfc_count, "<br>", 
                              "<strong>Total population:</strong> ", round(population, 0), "<br>", 
                              "<strong>Food insecure population:</strong> ", 
                              round(population*food_insecurity_2023, 0)), 
              group = "NTA Aggregation") %>% 
  addPolygons(data = parks, color = "white", fillColor = "white", weight = 0, 
              fillOpacity = 1, smoothFactor = 0) %>%
  addCircles(data = cfc_locations[cfc_locations$TYPE == "Food Pantry", ], 
             radius = 100, fillColor = ~pal2(TYPE), 
             opacity = 1, fillOpacity = 1, color = "black", weight = 0.75, 
             popup = ~label, group = "Individual Locations") %>% 
  addCircles(data = cfc_locations[cfc_locations$TYPE == "Soup Kitchen", ], 
             radius = 100, fillColor = ~pal2(TYPE), 
             opacity = 1, fillOpacity = 1, color = "white", weight = 0.75, 
             popup = ~label, group = "Individual Locations") %>%
  addCircles(data = cfc_locations[cfc_locations$TYPE == "Multiple", ], 
             radius = 100, fillColor = ~pal2(TYPE), 
             opacity = 1, fillOpacity = 1, color = "white", weight = 0.75, 
             popup = ~label, group = "Multiple") %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = ntas$cfc_per10k_fi,
                       title = paste0("# of CFC locations per 10k <br>", 
                                      "food insecure NTA residents"), 
                       opacity = 1, decreasing = T) %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                       values = cfc_locations$TYPE,
                       title = paste0("CFC Location Type"), 
                       opacity = 1, decreasing = T) %>%
  addCouncilStyle(add_dists = F) %>%
  addLayersControl(
    overlayGroups = c("Individual Locations", "NTA Aggregation"),
    options = layersControlOptions(collapsed = T))

saveWidget(map, file=file.path('visuals', 
                               "number_CFC_locations_per_10k_fi.html"))


################################################################################
# plot static map - # of CFC locations per 10k food insecure NTA residents
################################################################################

# plot
map = leaflet()  %>% 
  addPolygons(data = ntas, weight = 0, color = ~pal(cfc_per10k_fi), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addPolygons(data = parks, color = "white", fillColor = "white", weight = 0, 
              fillOpacity = 1, smoothFactor = 0) %>%
  addCouncilStyle(add_dists = F, highlight_dists = c(10, 14, 13, 4, 2, 1, 50, 51, 43, 22, 46:48, 19, 23, 30, 24, 20)) %>%
  addSourceText("Source: NYC Open Data, NYC Human Resources Administration") %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("# of CFC locations per 10k <br>", 
                                      "food insecure NTA residents"), 
                       opacity = 1, decreasing = T) 

mapview::mapshot(map, 
                 file = file.path("visuals", "number_CFC_locations_per_10k_fi.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# prep for plotting council district
################################################################################

# prep for plotting
quantile(ntas$cfc_count)
pal = councildown::colorBin(
  palette = "cool",
  bins = c(-0.01, 0.00001, 1, 2, 4, 6, 15),
  domain = ntas$cfc_count,
  na.color = "lightgrey"
)


################################################################################
# plot static map - # of CFC locations per Community District
################################################################################

# plot
map = leaflet() %>% 
  addPolygons(data = ntas, weight = 0, color = ~pal(cfc_count), 
              fillOpacity = 1, smoothFactor = 0, 
              popup = ~paste0("<strong>CFC locations:</strong> ", cfc_count, "<br>", 
                              "<strong>Total population:</strong> ", round(population, 0))) %>% 
  addCouncilStyle(add_dists = T, 
                  highlight_dists = c(7, 9, 27, 28, 36, 40, 41, 42, 45)) %>%
  addSourceText("Source: NYC Open Data, NYC Human Resources Administration") %>%
  addLegend_decreasing(position = "topleft", #pal = pal, 
                       colors = rev(c("#e3eaf7", "#a5adcd", "#8691b9", "#6775a5", "#485a91", "#23417d")),
                       na.label = "no CFC locations",
                       title = "# of CFC locations by NTA", 
                       labels = rev(c("0", "1", "2", "3 - 4", "5 - 6", "7 - 13")),
                       opacity = 1, decreasing = F) 

mapview::mapshot(map, 
                 file = file.path("visuals", "number_CFC_locations.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)
saveWidget(map, file=file.path('visuals', "number_CFC_locations.html"))



################################################################################
# plot static map - NTAs w high FI pop + low CFC locations - SIMPLE
################################################################################

fi_pop = mean(ntas$ins_pop, na.rm=T)
cfc_temp = mean(ntas$cfc_per10k_fi, na.rm=T)

ntas = ntas %>% 
  mutate(category = case_when(ins_pop > fi_pop & cfc_per10k_fi <= cfc_temp ~ "ABOVE average # of food insecure people, BELOW average CFC access",
                              ins_pop > fi_pop & cfc_per10k_fi > cfc_temp ~ "ABOVE average # of food insecure people, ABOVE average CFC access",
                              ins_pop <= fi_pop & cfc_per10k_fi <= cfc_temp ~ "BELOW average # of food insecure people, BELOW average CFC access",
                              ins_pop <= fi_pop & cfc_per10k_fi > cfc_temp ~ "BELOW average # of food insecure people, ABOVE average CFC access",
                              T ~ NA), 
         category = factor(category, levels = c("ABOVE average # of food insecure people, BELOW average CFC access", 
                                                "ABOVE average # of food insecure people, ABOVE average CFC access", 
                                                "BELOW average # of food insecure people, BELOW average CFC access", 
                                                "BELOW average # of food insecure people, ABOVE average CFC access"))) 

pal2 = colorFactor(
  palette = c("#EB9E87", "#D93403", "#58A8F0", "#A1CBF1"), 
  domain = levels(ntas$category),
  na.color = "grey"
)

# plot
map = leaflet() %>% 
  addPolygons(data = ntas, weight = 0, color = ~pal2(category), 
              fillOpacity = 1, smoothFactor = 0, 
              popup = ~paste0("<strong>", round(cfc_per10k_fi, 1), 
                              "</strong> CFC locations per 10k food insecure residents<br>", 
                              "<strong>CFC locations:</strong> ", cfc_count, "<br>", 
                              "<strong>Total population:</strong> ", round(population, 0), "<br>", 
                              "<strong>Food insecure population:</strong> ", 
                              round(population*food_insecurity_2023, 0)))  %>% 
  addPolygons(data = parks, color = "grey", fillColor = "grey", weight = 0, 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addSourceText("Source: NYC Open Data") %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                       values = unique(ntas$category),
                       title = paste0("Areas with high food insecurity in need of additional support"), 
                       opacity = 1, decreasing = T)

saveWidget(map, file=file.path('visuals', "CFC_locations_support.html"))
mapview::mapshot(map, 
                 file = file.path("visuals", "CFC_locations_support.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)



