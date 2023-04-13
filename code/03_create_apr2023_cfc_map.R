source("code/00_load_dependencies.R")
source("../tokens.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/12/2023
#
# This file creates a map of CFC locations by council district
################################################################################


################################################################################
# read in data
################################################################################

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS")) %>%
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))

community_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))

cd_population = readxl::read_xlsx(file.path("data", "input", 
                                            "nyc_decennialcensusdata_2010_2020_change.xlsx"), 
                                  sheet = "2010, 2020, and Change", skip = 3) %>%
  filter(`CD Type` == "CD") %>%
  select(GeoID, Pop_20) %>%
  rename(cd = GeoID, pop = Pop_20)


################################################################################
# prep for plotting community district
################################################################################

# how many cfc in each district?
community_districts$cfc_count = lengths(st_intersects(community_districts, cfc_geocoded))
community_districts = community_districts %>%
  merge(cd_population, by.x="BoroCD", by.y="cd", all.x=T)%>%
  mutate(cfc_per100k = cfc_count/pop*100000)

# prep for plotting
d = c(min(community_districts$cfc_per100k, na.rm=T), 
      max(community_districts$cfc_per100k, na.rm=T))

pal = colorBin(
  palette = rev(nycc_pal("cool")(100)),
  bins = c(0, 2, 4, 6, 10, 30),
  domain = d,
  na.color = "transparent"
)

pal2 = colorFactor(
  palette = nycc_pal("warm")(3), 
  domain = cfc_locations$TYPE,
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
  addPolygons(data = community_districts, weight = 0, color = ~pal(cfc_per100k), 
              fillOpacity = 1, smoothFactor = 0, 
              popup = ~paste0(round(cfc_per100k, 1), 
                              " CFC locations per 100k"), 
              group = "Community District Aggregation") %>% 
  addCircles(data = cfc_locations, radius = 100, color = ~pal2(TYPE),
             fillOpacity = 0.8, stroke = F, popup = ~label, 
             group = "Individual Locations") %>% 
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


# how many cfc in each district?
council_districts$cfc_count = lengths(st_intersects(council_districts, cfc_geocoded))

# prep for plotting
d = c(min(council_districts$cfc_count, na.rm=T), 
      max(council_districts$cfc_count, na.rm=T))

pal = colorBin(
  palette = rev(nycc_pal("cool")(100)),
  bins = c(0, 5, 10, 20, 30),
  domain = d,
  na.color = "transparent"
)

source_notes_locations$source = "Source: NYC Open Data, NYC Human Resources Administration"


################################################################################
# plot static map - # of CFC locations in District
################################################################################

# plot
map = leaflet() %>% 
  addPolygons(data = council_districts, weight = 0, color = ~pal(cfc_count), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = T, 
                  highlight_dists = council_districts$CounDist[council_districts$cfc_count >= 10]) %>%
  addLabelOnlyMarkers(data = source_notes_locations, 
                      label = ~source, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="15px"))) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("# of CFC locations in <br>", 
                                      "Council District"), 
                       opacity = 1, decreasing = T) 

mapview::mapshot(map, 
                 file = file.path("visuals", "number_CFC_locations.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# create interactive version of CFC map
################################################################################

# prep for mapping
cfc_locations = cfc_locations %>%
  mutate(label = paste0("<strong>Program Name:</strong> ", PROGRAM, "<br>",
                        "<strong>Open Times:</strong> ", DAYS, "<br>",
                        "<strong>Address:</strong> ", address)) 

map = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addCircles(data = cfc_locations, radius = 100, 
              fillOpacity = 0.8, stroke = F, popup = ~label) %>% 
  addCouncilStyle(add_dists = T) 

saveWidget(map, file=file.path('visuals', 
                               "individual CFC locations.html"))
