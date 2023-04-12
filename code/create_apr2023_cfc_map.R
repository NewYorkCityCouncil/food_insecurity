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

cfc_geocoded =  st_as_sf(cfc_geocoded, coords = c("lon","lat"),
                         crs = st_crs(4326))

################################################################################
# prep for plotting 
################################################################################

# how many cfc in each district?
districts = aggregate(cfc_geocoded, by=council_districts, FUN=sum)

# prep for plotting
d = c(min(community_districts$perc_snap_cur, na.rm=T), 
      max(community_districts$perc_snap_cur, na.rm=T))

pal = colorBin(
  palette = rev(nycc_pal("cool")(100)),
  bins = 5,
  domain = d,
  na.color = "transparent"
)


labels = community_districts %>%
  st_drop_geometry() %>%
  group_by(boro.x) %>% 
  summarise(bc_snap_recipients.x = sum(bc_snap_recipients.x, na.rm = T)) %>%
  mutate(cur_recipients_label = paste0(boro.x, ": ", format(bc_snap_recipients.x, big.mark=",")), 
         cur_recipients_label = ifelse(is.na(bc_snap_recipients.x), NA, cur_recipients_label)) %>%
  drop_na() 

labels = boro_label_locations %>%
  merge(labels, by.x = "boro", by.y = "boro.x", all = T)

source_notes_locations$source = "SNAP enrollees: data.cityofnewyork.us/d/jye8-w4d7; Population: nyc.gov/site/planning/planning-level/nyc-population/2020-census.page"



################################################################################
# plot static map - % of community district that receives SNAP
################################################################################

# plot
map = leaflet() %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(perc_snap_cur), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLabelOnlyMarkers(data = labels, 
                      label = ~cur_recipients_label, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="20px"))) %>%
  addLabelOnlyMarkers(data = source_notes_locations, 
                      label = ~source, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="10px"))) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("% SNAP recipients in <br>", 
                                      "Community District"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T)

mapview::mapshot(map, 
                 file = file.path("visuals", "percent_individuals_SNAP_benefits_cur.pdf"),
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
