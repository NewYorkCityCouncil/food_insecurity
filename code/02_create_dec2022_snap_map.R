source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/13/2023
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
         borough_letter = ifelse(boro.x == "Brooklyn", "K", borough_letter)) %>%
  drop_na(perc_snap_cur)


################################################################################
# prep for plotting 
################################################################################

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

source_notes_locations$source = "Source: NYC Open Data, Department of City Planning"


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
                                                  style=list('color'="#555555", 'fontSize'="15px"))) %>%
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
  mutate(perc_change_snap = round(((bc_snap_recipients.x-bc_snap_recipients.y)/bc_snap_recipients.y)*100, 0), 
         label = paste0("<strong>Community District #</strong>: ", BoroCD, "<br>",
                        "<strong>Population (2020):</strong> ", format(pop, big.mark = ","), "<br>", 
                        "<strong>Number of SNAP recipients (Dec 2021):</strong> ", 
                            format(bc_snap_recipients.x, big.mark = ","), "<br>",
                        "<strong>% of Population receiving SNAP (Dec 2021):</strong> ", round(perc_snap_cur*100, 0), "%<br>",
                        "<strong>% growth in recipients since 2019:</strong> ", 
                        perc_change_snap, "%")) 


d = c(min(community_districts$perc_change_snap, na.rm=T), 
      max(community_districts$perc_change_snap, na.rm=T))

pal2 = colorBin(
  palette = rev(nycc_pal("diverging")(100)),
  bins = c(-45, -30, -20, -5, 5, 20, 30, 45),
  domain = d,
  na.color = "transparent"
)

map = leaflet() %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal2(perc_change_snap), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "Change in SNAP recipients") %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(perc_snap_cur), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% SNAP Dec 2022")  %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = community_districts$perc_snap_cur,
                       title = paste0("% SNAP recipients in <br>", 
                                      "Community District"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T) %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                     values = community_districts$perc_change_snap,
                     title = paste0("% change in SNAP recipients <br>", 
                                    "from Dec 2019 to Dec 2022"), 
                     labFormat = labelFormat(suffix = "%"),
                     opacity = 1, decreasing = T) %>%
  addLayersControl(
    overlayGroups = c("% SNAP Dec 2022", "Change in SNAP recipients"),
    options = layersControlOptions(collapsed = T))

saveWidget(map, file=file.path('visuals', 
                               "percent_individuals_SNAP_benefits.html"))


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
  write.xlsx("visuals/info_table.xlsx")