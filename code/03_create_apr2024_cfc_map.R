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

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS")) %>%
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))


################################################################################
# prep for plotting community district
################################################################################

# prep for plotting
d = c(min(community_districts$cfc_per100k, na.rm=T), 
      max(community_districts$cfc_per100k, na.rm=T))

breaks = classInt::classIntervals(community_districts$cfc_per100k, n = 6, style = 'jenks')$brks
breaks[length(breaks)] = breaks[length(breaks)] + 0.5
breaks = round(breaks, 0)

pal = councildown::colorBin(
  palette = "cool",
  bins = breaks,
  domain = d,
  na.color = "transparent"
)

pal2 = colorFactor(
  palette = pal_nycc("warm"), 
  domain = c("Multiple", "Food Pantry", "Soup Kitchen"),
  na.color = "transparent"
)

cfc_locations = cfc_locations %>%
  mutate(label = paste0("<strong>Program Name:</strong> ", PROGRAM, "<br>",
                        "<strong>Program Type:</strong> ", TYPE, "<br>",
                        "<strong>Open Times:</strong> ", DAYS, "<br>",
                        "<strong>Address:</strong> ", address))


################################################################################
# plot interactive map - # of CFC locations in Community District
################################################################################

# plot
map = leaflet() %>% 
  addPolygons(data = community_districts %>% 
                filter(!is.na(cfc_per100k)), 
              weight = 0, color = ~pal(cfc_per100k), 
              fillOpacity = 1, smoothFactor = 0, 
              popup = ~paste0(round(cfc_per100k, 1), 
                              " CFC locations per 100k residents"), 
              group = "Community District Aggregation") %>% 
  addCircles(data = cfc_locations[cfc_locations$TYPE == "Food Pantry", ], 
             radius = 100, fillColor = ~pal2(TYPE), 
             opacity = 1, fillOpacity = 1, color = "black", weight = 0.75, 
             popup = ~label, group = "Individual Locations") %>% 
  addCircles(data = cfc_locations[cfc_locations$TYPE == "Soup Kitchen", ], 
             radius = 100, fillColor = ~pal2(TYPE), 
             opacity = 1, fillOpacity = 1, color = "white", weight = 0.75, 
             popup = ~label, group = "Individual Locations") %>%
  addCircles(data = cfc_locations[cfc_locations$TYPE == "Multiple", ], 
             radius = 100, fillColor = ~pal2(TYPE), fillOpacity = 1, stroke = F, 
             popup = ~label, group = "Individual Locations") %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("# of CFC locations per 100k <br>", 
                                      "Community District residents"), 
                       opacity = 1, decreasing = T) %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                       values = cfc_locations$TYPE,
                       title = paste0("CFC Location Type"), 
                       opacity = 1, decreasing = T) %>%
  addCouncilStyle(add_dists = F) %>%
  addLayersControl(
    overlayGroups = c("Individual Locations", "Community District Aggregation"),
    options = layersControlOptions(collapsed = T))

saveWidget(map, file=file.path('visuals', 
                               "number_CFC_locations_Community_District.html"))


################################################################################
# prep for plotting council district
################################################################################

# prep for plotting
d = c(min(council_districts$cfc_count, na.rm=T), 
      max(council_districts$cfc_count, na.rm=T))

pal = councildown::colorBin(
  palette = "cool",
  bins = c(0, 5, 10, 20, 30),
  domain = d,
  na.color = "transparent"
)


################################################################################
# plot static map - # of CFC locations in District
################################################################################

# plot
map = leaflet() %>% 
  addPolygons(data = council_districts, weight = 0, color = ~pal(cfc_count), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = T, 
                  highlight_dists = council_districts$CounDist[council_districts$cfc_count >= 10]) %>%
  addSourceText("Source: NYC Open Data, NYC Human Resources Administration") %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("# of CFC locations in <br>", 
                                      "Council District"), 
                       opacity = 1, decreasing = T) 

mapview::mapshot(map, 
                 file = file.path("visuals", "number_CFC_locations.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)
