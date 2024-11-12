source("code/00_load_dependencies.R")
source("../tokens.R")
library(data.table)
library(councilverse)

################################################################################
# Created by: Anne Driscoll
# Last edited on: 5/13/2024
#
# Geocode all the CFC locations
################################################################################

################################################################################
# read in data
################################################################################

council_districts = "https://www.nyc.gov/assets/planning/download/zip/data-maps/open-data/nycc_21d.zip" %>%
  unzip_sf() %>%
  st_read() %>%
  st_transform(st_crs(4326))


# color the final dots by type and combine multiples 
cfc_locations = read_csv(file.path("data", "input", "EFAP_pdf_11_4_24.csv")) %>%
  group_by(PROGRAM) %>% # unique locations (combine multiples)
  mutate(n = n()) %>% #count how many programs there are per location
  filter(row_number()==1) %>% # just keep one instance
  mutate(DISTBORO = case_when(DISTBORO == "BK" ~ "Brooklyn", 
                              DISTBORO == "BX" ~ "Bronx", 
                              DISTBORO == "NY" ~ "Manhattan", 
                              DISTBORO == "QN" ~ "Queens", 
                              DISTBORO == "SI" ~ "Staten Island", ), 
         address = paste0(DISTADD, ", ", DISTBORO, ", New York, ", DISTZIP), 
         TYPE = case_when(n == 2 ~ "Multiple", 
                          TYPE == "FP" ~ "Food Pantry", 
                          TYPE == "SK" ~ "Soup Kitchen")) %>% #rename types
  select(-n) %>%
  ungroup()

################################################################################
# munge the cfc locations
################################################################################

geocoded = as.list(rep(NA, nrow(cfc_locations)))
for (i in 1:nrow(cfc_locations)) {
  
  temp = st_read(URLencode(paste0("https://geosearch.planninglabs.nyc/v2/search?text=", 
                                  cfc_locations$address[i], "&size=1")))[1, ]
  
  if (nrow(temp) == 0 | st_is_empty(temp$geometry)) {
    temp = NA
  } else {
    temp = temp %>%
      select(label, neighbourhood, source, confidence, match_type, postalcode)
  }
  
  geocoded[[i]] = temp
}

# figure out which didn't return anything in geocoding
not_empty = sapply(geocoded, nrow)
not_empty[sapply(not_empty, is.null)] = 0
not_empty = unlist(not_empty) == 1
print(sum(!not_empty)) # there are 0 locations that didn't geocode!!!!!

# combine the ones that did return a geocoding
combined = cbind(cfc_locations[not_empty, ], rbindlist(geocoded[not_empty]))

saveRDS(combined, file.path("data", "output", "cfc_geocoded_2024.RDS"))
