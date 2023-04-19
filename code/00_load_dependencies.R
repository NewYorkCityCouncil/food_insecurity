## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("gt", "gtExtras", "mapview", "councildown", "leaflet", 
                      "sf", "tidyverse", "janitor", "jsonlite", "openxlsx", 
                      "htmlwidgets", "ggmap")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages)


# creating points that we will use to add labels in the map 
#     these are just well placed locations for text on the map
geo = st_sfc(st_point(c(-73.9927626, 40.7916781)), 
             st_point(c(-73.757623, 40.631025)), 
             st_point(c(-74.105017, 40.658491)), 
             st_point(c(-73.948387, 40.555264)), 
             st_point(c(-73.930587, 40.898989)))

boro_label_locations = st_sf(boro = c("Manhattan", "Queens", "Staten Island", 
                                      "Brooklyn", "Bronx"), 
                             geometry = geo) %>% 
  st_set_crs(st_crs(4326))


