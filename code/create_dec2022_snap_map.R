source("code/00_load_dependencies.R")

########################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/6/2023
#
# This file creates a map of SNAP usage from Human Resources Administration
########################################################################################


################################################################################
# read in data
################################################################################

snap_population = fromJSON("https://data.cityofnewyork.us/resource/jye8-w4d7.json?$query=SELECT%20%60month%60%2C%20%60boro%60%2C%20%60cd%60%2C%20%60bc_snap_recipients%60%2C%20%60bc_snap_households%60%0AWHERE%0A%20%20%60month%60%0A%20%20%20%20BETWEEN%20%222022-04-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20AND%20%222023-04-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp")
snap_population = snap_population %>%
  mutate(cd = gsub("M", "1", cd), 
         cd = gsub("B", "2", cd),
         cd = gsub("K", "3", cd),  
         cd = gsub("Q", "4", cd), 
         cd = gsub("S", "5", cd), 
         bc_snap_recipients = as.numeric(bc_snap_recipients), 
         bc_snap_households = as.numeric(bc_snap_households)) %>%
  filter(month > "2022-12-01")

cd_population = read_csv("https://data.cityofnewyork.us/resource/xi7c-iiu2.csv") %>%
  mutate(borough = gsub("Manhattan", "1", borough), 
         borough = gsub("Bronx", "2", borough),
         borough = gsub("Brooklyn", "3", borough),  
         borough = gsub("Queens", "4", borough), 
         borough = gsub("Staten Island", "5", borough), 
         cd = paste0(borough, str_pad(cd_number, 2, "left", "0"))) %>%
  select(cd, `_2010_population`) %>%
  rename(pop = `_2010_population`)

community_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326)) %>% 
  merge(snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(cd_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  mutate(perc_snap = bc_snap_recipients/pop)


################################################################################
# plot
################################################################################

# prep for plotting
pal = colorNumeric(
  palette = colorRamp(rev(nycc_pal("cool")(12))),
  domain = snap_population$perc_snap,
  na.color = "transparent"
)

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(bc_snap_households), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = community_districts$perc_snap,
                       title = paste0("% of community district  <br>", 
                                      "with SNAP benefits"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T)

mapview::mapshot(map, 
                 file = file.path("visuals", "percent_individuals_SNAP_benefits.png"),
                 remove_controls = c("homeButton", "layersControl"), vwidth = 1000, vheight = 850)


  