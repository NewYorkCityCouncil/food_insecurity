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

cfc_locations = readRDS(file.path("data", "output", "cfc_geocoded.RDS"))


################################################################################
# prep for plotting 
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
  addCouncilStyle(add_dists = T) %>%
  addLabelOnlyMarkers(data = source_notes_locations, 
                      label = ~source, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="15px"))) %>%
  addLabelOnlyMarkers(data = councildown:::dists %>% 
                        filter(coun_dist %in% 
                                 council_districts$CounDist[council_districts$cfc_count >= 10]), 
                      lat = ~lab_y, lng = ~lab_x, label = ~coun_dist,
                      labelOptions = labelOptions(permanent = TRUE, noHide = TRUE,
                                                  textOnly = TRUE,
                                                  textsize = 12,
                                                  direction = "center",
                                                  style = list(color = "white",
                                                               'font-family' = "'Open Sans', sans-serif",
                                                               'font-weight' = "bold"))) %>%
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
# plot static map - # of CFC locations in District
################################################################################

# plot
map = leaflet() %>% 
  addCircles(data = cfc_geocoded, radius = 20) %>% 
  addCouncilStyle(add_dists = T) %>%
  addLabelOnlyMarkers(data = source_notes_locations, 
                      label = ~source, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="10px")))

mapview::mapshot(map, 
                 file = file.path("visuals", "individual_CFC_locations.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# create interactive version of CFC map
################################################################################

# prep for mapping
community_districts = community_districts %>%
  mutate(label = paste0("<strong>Community District #</strong>: ", BoroCD, "<br>",
                        "<strong>Population (2020):</strong> ", format(pop, big.mark = ","), "<br>", 
                        "<strong>Number of SNAP recipients (Dec 2021):</strong> ", 
                        format(bc_snap_recipients.x, big.mark = ","), "<br>",
                        "<strong>% of Population receiving SNAP (Dec 2021):</strong> ", round(perc_snap_cur*100, 0), "%<br>",
                        "<strong>% growth in recipients since 2019:</strong> ", 
                        round(((bc_snap_recipients.x-bc_snap_recipients.y)/bc_snap_recipients.y)*100, 0), "%")) 

map = leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(perc_snap_cur), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("% SNAP recipients in <br>", 
                                      "Community District"), 
                       opacity = 1, decreasing = T)

saveWidget(map, file=file.path('visuals', 
                               "percent_individuals_SNAP_benefits.html"))
