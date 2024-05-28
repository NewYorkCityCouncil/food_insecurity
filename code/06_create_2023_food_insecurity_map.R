source("code/00_load_dependencies.R")
remotes::install_github("walkerke/tidycensus")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 5/21/2024
#
# This file creates a map of food insecurity across the city based on the 
# Mayor's Office of Food Policy data
# https://public.tableau.com/app/profile/claire.reynolds6296/viz/SupplyGap2023/NeighborhoodPrioritization2023
################################################################################


################################################################################
# read in data 
################################################################################

nta_2020 = readRDS(file.path("data", "output", "nta_data.RDS"))

################################################################################
# create interactive version of SNAP map
################################################################################

# prep for plotting ------------------------------------------------------------
d = c(min(nta_2020$food_insecurity_2023, na.rm=T), 
      max(nta_2020$food_insecurity_2023, na.rm=T))

quantile(nta_2020$food_insecurity_2023, c(0, .2, .4, .6, .8, 1), na.rm=T)*100
pal = colorBin(
  palette = pal_nycc("warm"),
  bins = c(.05, .09, .14, .18, .35),
  domain = d,
  na.color = "grey"
)

nta_2020 = nta_2020 %>%
  mutate(label = paste0("<strong>NTA</strong>: ", nta2020, "<br>",
                        "<strong>NTA name</strong>: ", ntaname, "<br>",
                        "<strong>% food insecure in 2023:</strong> ", round(food_insecurity_2023*100, 1), "%<br>", 
                        "<strong>% point change in food insecurity 2018-2023</strong>: ", 
                            round(perc_point_change_food_insecurity*100, 1), "%")) 

d = c(min(nta_2020$perc_point_change_food_insecurity, na.rm=T), 
      max(nta_2020$perc_point_change_food_insecurity, na.rm=T))
quantile(nta_2020$perc_point_change_food_insecurity, c(0, .2, .4, .6, .8, 1), na.rm=T)

pal2 = colorBin(
  palette = rev(pal_nycc("diverging")),
  bins = c(-5, -3, -2, -1, 1, 2, 3, 5),
  domain = d*100,
  na.color = "grey"
)

map = leaflet() %>% 
  addPolygons(data = nta_2020, weight = 0, color = ~pal2(perc_point_change_food_insecurity*100), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% point change in food insecurity since 2018") %>% 
  addPolygons(data = nta_2020, weight = 0, color = ~pal(food_insecurity_2023), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% food insecure 2023") %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = nta_2020$food_insecurity_2023,
                       title = paste0("% food insecure by <br>", 
                                      "Neighborhood Tabulation Area"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T, 
                       group = "% food insecure 2023") %>%
  addLegend_decreasing(position = "topleft", pal = pal2, 
                       values = nta_2020$perc_point_change_food_insecurity,
                       title = paste0("% point change in food insecurity <br>", 
                                      "from 2018 to 2023"), 
                       labFormat = labelFormat(suffix = "%"),
                       opacity = 1, decreasing = T, 
                       group = "% point change in food insecurity since 2018") %>%
  addLayersControl(
    baseGroups = c("% food insecure 2023", "% point change in food insecurity since 2018"),
    options = layersControlOptions(collapsed = F, autoZIndex = T))

saveWidget(map, file=file.path('visuals', 
                               "percent_individuals_food_insecure.html"))

################################################################################
# static map perc individuals
################################################################################

map = leaflet() %>% 
  addPolygons(data = nta_2020, weight = 0, color = ~pal(food_insecurity_2023), 
              fillOpacity = 1, smoothFactor = 0, popup = ~label, 
              group = "% food insecure 2023") %>% 
  addCouncilStyle(add_dists = T, 
                  highlight_dists = c(34, 42, 8:10, 14:18)) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = nta_2020$food_insecurity_2023,
                       title = paste0("% food insecure by <br>", 
                                      "Neighborhood Tabulation Area"), 
                       labFormat = labelFormat(suffix = "%", 
                                               transform = function(x){x*100}),
                       opacity = 1, decreasing = T, 
                       group = "% food insecure 2023")

mapview::mapshot(map, 
                 file = file.path("visuals", "percent_individuals_food_insecure.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)


################################################################################
# static map NUM individuals
################################################################################


quantile(nta_2020$food_insecurity_2023*nta_2020$population, c(0, .2, .4, .6, .8, 1), na.rm=T)
pal = colorBin(
  palette = pal_nycc("warm"),
  bins = c(1000, 3000, 5000, 7000, 9000, 20000),
  domain = d,
  na.color = "grey"
)


map = leaflet() %>% 
  addPolygons(data = nta_2020, weight = 0, color = ~pal(food_insecurity_2023*population), 
              fillOpacity = 1, smoothFactor = 0) %>% 
  addCouncilStyle(add_dists = F) %>%
  addLegend_decreasing(position = "topleft", pal = pal, 
                       values = nta_2020$food_insecurity_2023,
                       title = paste0("# people food insecure by <br>", 
                                      "Neighborhood Tabulation Area"),
                       opacity = 1, decreasing = T) 

mapview::mapshot(map, 
                 file = file.path("visuals", "num_individuals_food_insecure.pdf"),
                 remove_controls = c("homeButton", "layersControl", "zoomControl"), 
                 vwidth = 1000, vheight = 850)

################################################################################
# get top and bottom NTA's
################################################################################

nta_2020 %>% 
  arrange(food_insecurity_2023) %>% 
  st_drop_geometry() %>%
  select(ntaname, food_insecurity_2023) %>% 
  head()

nta_2020 %>% 
  arrange(desc(food_insecurity_2023)) %>% 
  st_drop_geometry() %>%
  select(ntaname, food_insecurity_2023) %>% 
  head()
