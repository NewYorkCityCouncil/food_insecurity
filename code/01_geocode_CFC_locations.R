source("code/00_load_dependencies.R")
source("../tokens.R")

################################################################################
# Created by: Anne Driscoll
# Last edited on: 4/12/2023
#
# Geocode all the CFC locations
################################################################################

################################################################################
# read in data
################################################################################

council_districts = unzip_sf("https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip") %>%
  st_read() %>%
  st_transform(st_crs(4326))

cfc_locations = read_csv(file.path("data", "input", "EFAP_pdf_3_6_23.csv")) %>%
  mutate(DISTBORO = case_when(DISTBORO == "BK" ~ "Brooklyn", 
                              DISTBORO == "BX" ~ "Bronx", 
                              DISTBORO == "NY" ~ "Manhattan", 
                              DISTBORO == "QN" ~ "Queens", 
                              DISTBORO == "SI" ~ "Staten Island", ), 
         address = paste0(DISTADD, ", ", DISTBORO, ", New York, ", DISTZIP))

################################################################################
# munge the cfc locations
################################################################################

register_google(key = google_maps_token)
lat_lon = geocode(cfc_locations$address)
cfc_geocoded = cbind(cfc_locations, lat_lon)

cfc_geocoded$lat[cfc_geocoded$ID == 80419] = 40.704906
cfc_geocoded$lon[cfc_geocoded$ID == 80419] = -73.955944

cfc_geocoded =  st_as_sf(cfc_geocoded, coords = c("lon","lat")) %>% 
  st_set_crs(st_crs(4326))

saveRDS(cfc_geocoded, file.path("data", "output", "cfc_geocoded.RDS"))
