source("code/00_load_dependencies.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 5/13/2024
#
# This file creates a map of SNAP usage from Human Resources Administration
################################################################################

  
community_districts = read_rds(file.path("data", "output", 
                                         "community_district_data.RDS"))

borough = st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>%
  st_transform(st_crs(4326)) 

################################################################################
# prep for plotting 
################################################################################

# prep for plotting
d = c(min(community_districts$perc_snap_cur, na.rm=T), 
      max(community_districts$perc_snap_cur, na.rm=T))

quantile(community_districts$perc_snap_cur, c(0, .2, .4, .6, .8, 1))*100
pal = colorBin(
  palette = pal_nycc("cool"),
  bins = 4,
  domain = d,
  na.color = "transparent"
)

labels = community_districts %>%
  st_drop_geometry() %>%
  group_by(borough.x) %>% 
  summarise(bc_snap_recipients.x = sum(bc_snap_recipients.x, na.rm = T)) %>%
  mutate(cur_recipients_label = paste0(borough.x, ": ", format(bc_snap_recipients.x, big.mark=",")), 
         cur_recipients_label = ifelse(is.na(bc_snap_recipients.x), NA, cur_recipients_label), 
         borough.x = ifelse(borough.x == "Staten_Island", "Staten Island", borough.x)) %>%
  drop_na() %>%
  as.data.frame()

labels = boro_label_locations %>%
  base::merge(labels, by.x = "boro", by.y = "borough.x", all = T)
  


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
  addSourceText("Source: NYC Open Data, Department of City Planning") %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = d,
                       title = paste0("% SNAP recipients in <br>", 
                                      "Community District, Mar 24"), 
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
                        "<strong>Neighborhoods covered</strong>: ", neighborhoods, "<br>",
                        "<strong>Population (2020):</strong> ", format(pop, big.mark = ","), "<br>", 
                        "<strong>Number of SNAP recipients (Dec 2021):</strong> ", 
                            format(bc_snap_recipients.x, big.mark = ","), "<br>",
                        "<strong>% of Population receiving SNAP (Mar 24):</strong> ", round(perc_snap_cur*100, 0), "%<br>",
                        "<strong>% point change in recipients since Mar 2019:</strong> ", 
                        perc_point_change_snap, "%")) 

d = c(min(community_districts$perc_point_change_snap, na.rm=T), 
      max(community_districts$perc_point_change_snap, na.rm=T))
quantile(community_districts$perc_point_change_snap, c(0, .2, .4, .6, .8, 1))

pal2 = colorBin(
  palette = rev(pal_nycc("diverging")),
  bins = c(-5, -3, -2, -1, 1, 2, 3, 5),
  domain = d,
  na.color = "transparent"
)

map = leaflet() %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal2(perc_point_change_snap), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% point change in SNAP recipients since Mar 2019") %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal(perc_snap_cur), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% SNAP Mar 2024")  %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = community_districts$perc_snap_cur,
                       title = paste0("% SNAP recipients in <br>", 
                                      "Community District"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T, 
                       group = "% SNAP Mar 2024") %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                     values = community_districts$perc_point_change_snap,
                     title = paste0("% point change in SNAP recipients <br>", 
                                    "from Mar 2019 to Mar 2024"), 
                     labFormat = labelFormat(suffix = "%"),
                     opacity = 1, decreasing = T, 
                     group = "% point change in SNAP recipients") %>%
  addLayersControl(
    baseGroups = c("% SNAP Mar 2024", "% point change in SNAP recipients"),
    options = layersControlOptions(collapsed = F, autoZIndex = T))

saveWidget(map, file=file.path('visuals', 
                               "percent_individuals_SNAP_benefits.html"))


################################################################################
# static map
################################################################################

map = leaflet() %>% 
  addPolygons(data = community_districts, weight = 0, color = ~pal2(perc_point_change_snap), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% point change in SNAP recipients since Mar 2019") %>%
  addCouncilStyle(add_dists = T, 
                  highlight_dists = c(44, 45, 47, 48, 24, 20, 50, 24, 27, 28, 15:18, 12, 13)) %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                       values = community_districts$perc_point_change_snap,
                       title = paste0("% point change in SNAP recipients <br>", 
                                      "from Mar 2019 to Mar 2024"), 
                       labFormat = labelFormat(suffix = "%"),
                       opacity = 1, decreasing = T, 
                       group = "% point change in SNAP recipients")

mapview::mapshot(map, 
                 file = file.path("visuals", "percent_change_SNAP_benefits.pdf"),
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
         `Mar 2023 SNAP recipients` = bc_snap_recipients.x, 
         `District SNAP recipients as % of population` = perc_snap_cur) %>%
  write.xlsx("visuals/info_table.xlsx")

