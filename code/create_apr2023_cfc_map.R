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

cfc_locations = read_csv(file.path("data", "input", "EFAP_pdf_3_6_23.csv")) %>%
  mutate(address = paste0(DISTADD, ", ", DISTBORO, ", New York, ", DISTZIP))

################################################################################
# munge the cfc locations
################################################################################

register_google(key = google_maps_token)
lat_lon = geocode(cfc_locations$address)
cfc_geocoded = cbind(cfc_locations, lat_lon)

cfc_geocoded$lat[cfc_geocoded$ID == 80419] = 40.704906
cfc_geocoded$lon[cfc_geocoded$ID == 80419] = -73.955944

cfc_geocoded$lat[cfc_geocoded$ID == 85710] = 40.697440
cfc_geocoded$lon[cfc_geocoded$ID == 85710] = -73.789114

# some of them aren't actually in NYC - we're going to manually fix them 
#   if there were more or I had more time I wouldn't but........
out_of_bounds = which(cfc_geocoded$lat > 41| cfc_geocoded$lat < 39 | 
                        cfc_geocoded$lon > -73 | cfc_geocoded$lon < -75)

cfc_geocoded$lat[cfc_geocoded$ID == 81460] = 40.670860
cfc_geocoded$lon[cfc_geocoded$ID == 81460] = -73.938263 

cfc_geocoded$lat[cfc_geocoded$ID == 80373] = 40.692461
cfc_geocoded$lon[cfc_geocoded$ID == 80373] = -73.994368

cfc_geocoded$lat[cfc_geocoded$ID == 85720] = 40.633871
cfc_geocoded$lon[cfc_geocoded$ID == 85720] = -73.964062

cfc_geocoded$lat[cfc_geocoded$ID == 83608] = 40.650028
cfc_geocoded$lon[cfc_geocoded$ID == 83608] = -73.954685

cfc_geocoded$lat[cfc_geocoded$ID == 85702] = 40.807931
cfc_geocoded$lon[cfc_geocoded$ID == 85702] = -73.880980

cfc_geocoded$lat[cfc_geocoded$ID == 85709] = 40.835352
cfc_geocoded$lon[cfc_geocoded$ID == 85709] = -73.862052

cfc_geocoded$lat[cfc_geocoded$ID == 85243] = 40.691430
cfc_geocoded$lon[cfc_geocoded$ID == 85243] = -73.862205

cfc_geocoded$lat[cfc_geocoded$ID == 85332] = 40.682798
cfc_geocoded$lon[cfc_geocoded$ID == 85332] = -73.769656

cfc_geocoded$lat[cfc_geocoded$ID == 83623] = 40.761195
cfc_geocoded$lon[cfc_geocoded$ID == 83623] = -73.869022

cfc_geocoded$lat[cfc_geocoded$ID == 85167] = 40.704565
cfc_geocoded$lon[cfc_geocoded$ID == 85167] = -73.811768

cfc_geocoded$lat[cfc_geocoded$ID == 83296] = 40.707902
cfc_geocoded$lon[cfc_geocoded$ID == 83296] = -73.798884

cfc_geocoded$lat[cfc_geocoded$ID == 85255] = 40.670848
cfc_geocoded$lon[cfc_geocoded$ID == 85255] = -73.769249

cfc_geocoded$lat[cfc_geocoded$ID == 80971] = 40.756786
cfc_geocoded$lon[cfc_geocoded$ID == 80971] = -73.914506

cfc_geocoded$lat[cfc_geocoded$ID == 85380] = 40.636377
cfc_geocoded$lon[cfc_geocoded$ID == 85380] = -74.165004

cfc_geocoded$lat[cfc_geocoded$ID == 85356] = 40.622334
cfc_geocoded$lon[cfc_geocoded$ID == 85356] = -74.080422

cfc_geocoded =  st_as_sf(cfc_geocoded, coords = c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))


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
                                                  style=list('color'="#555555", 'fontSize'="10px"))) %>%
  addLogo(img = "https://drive.google.com/file/d/1emuQNkY_KTgK4WjeubuXhAf1IGdmXjy8/view?usp=sharing", src = "remote")

mapview::mapshot(map, 
                 file = file.path("visuals", "individual_CFC_locations.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# create interactive version of SNAP map
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
