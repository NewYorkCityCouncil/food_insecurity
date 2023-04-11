source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/6/2023
#
# This file creates a map of SNAP usage from Human Resources Administration
################################################################################


################################################################################
# read in data
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
  
community_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycd_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326)) %>% 
  merge(current_snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(pre_pandemic_snap_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  merge(cd_population, by.x = "BoroCD", by.y = "cd", all.x = T) %>%
  mutate(perc_snap_cur = bc_snap_recipients.x/pop, 
         perc_snap_pre = bc_snap_recipients.y/pop, 
         borough_letter = substr(boro.x, 1, 1), 
         borough_letter = ifelse(boro.x == "Brooklyn", "K", borough_letter))


################################################################################
# plot pre covid numbers
################################################################################

# prep for plotting
d = c(min(c(community_districts$perc_snap_cur, community_districts$perc_snap_pre), na.rm=T), 
      max(c(community_districts$perc_snap_cur, community_districts$perc_snap_pre), na.rm=T))

pal = colorNumeric(
  palette = colorRamp(rev(nycc_pal("cool")(12))),
  domain = d,
  na.color = "transparent"
)

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(perc_snap_pre), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("% of community district  <br>", 
                                      "with SNAP benefits"), 
                       labFormat = labelFormat(suffix = "%", 
                                                transform = function(x){x*100}),
                       opacity = 1, decreasing = T)

mapview::mapshot(map, 
                 file = file.path("visuals", "percent_individuals_SNAP_benefits_preCOVID.png"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# plot post covid % of SNAP individuals
################################################################################

labels = community_districts %>%
  st_drop_geometry() %>%
  group_by(boro.x) %>% 
  summarise(bc_snap_recipients.x = sum(bc_snap_recipients.x, na.rm = T)) %>%
  mutate(cur_recipients_label = paste0(boro.x, ": ", format(bc_snap_recipients.x, big.mark=",")), 
         cur_recipients_label = ifelse(is.na(bc_snap_recipients.x), NA, cur_recipients_label)) %>%
  drop_na() 

labels = boro_label_locations %>%
  merge(labels, by.x = "boro", by.y = "boro.x", all = T)

map = leaflet() %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(perc_snap_cur), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLabelOnlyMarkers(data = labels, 
                      label = ~cur_recipients_label, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="20px"))) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("% of community district  <br>", 
                                      "with SNAP benefits"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T)

mapview::mapshot(map, 
                 file = file.path("visuals", "percent_individuals_SNAP_benefits_cur.png"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# plot post covid raw # of SNAP individuals
################################################################################

# prep for plotting
d = c(min(c(community_districts$bc_snap_recipients.x, community_districts$bc_snap_recipients.y), na.rm=T), 
      max(c(community_districts$bc_snap_recipients.x, community_districts$bc_snap_recipients.y), na.rm=T))

pal = colorNumeric(
  palette = colorRamp(rev(nycc_pal("cool")(12))),
  domain = d,
  na.color = "transparent"
)
map = leaflet() %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(bc_snap_recipients.x), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLabelOnlyMarkers(data = labels, 
                      label = ~cur_recipients_label, 
                      labelOptions = labelOptions(noHide = T, direction = 'left', textOnly = T, 
                                                  style=list('color'="#555555", 'fontSize'="20px"))) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("# of people in community district  <br>", 
                                      "with SNAP benefits"), 
                       opacity = 1, decreasing = T)

mapview::mapshot(map, 
                 file = file.path("visuals", "num_individuals_SNAP_benefits_cur.png"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# get table output
################################################################################

community_districts %>%
  st_drop_geometry() %>%
  drop_na(pop) %>%
  mutate(perc_snap_cur = paste0(round(perc_snap_cur*100, 0), "%"), 
         bc_snap_recipients.x = format(bc_snap_recipients.x, big.mark=","), 
         pop = format(pop, big.mark=","), 
         BoroCD = paste0(borough_letter, substr(BoroCD, 2, 3))) %>%
  select(BoroCD, pop, bc_snap_recipients.x, perc_snap_cur) %>%
  arrange(BoroCD) %>% 
  rename(`Community District` = BoroCD, 
         `2020 Population` = pop, 
         `Dec 2022 SNAP recipients` = bc_snap_recipients.x, 
         `District SNAP recipients as % of population` = perc_snap_cur) %>%
  gt() %>%
  tab_header(title = "SNAP Recipients by Community District") %>%
  gt_theme_nytimes() %>%
  gtsave("visuals/info_table.pdf")
