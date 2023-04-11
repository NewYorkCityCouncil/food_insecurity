## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("gt", "gtExtras", "mapview", "councildown", "leaflet", 
                      "sf", "tidyverse", "janitor", "jsonlite", "openxlsx", 
                      "htmlwidgets")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# function to unzip shapefiles
unzip_sf <- function(zip_url) {
  temp <- tempfile()
  temp2 <- tempfile()
  #download the zip folder from the internet save to 'temp' 
  download.file(zip_url, temp)
  #unzip the contents in 'temp' and save unzipped content in 'temp2'
  unzip(zipfile = temp, exdir = temp2)
  #if returns "character(0), then .shp may be nested within the folder
  your_SHP_file <- ifelse(!identical(list.files(temp2, pattern = ".shp$",full.names=TRUE), character(0)), 
                          list.files(temp2, pattern = ".shp$",full.names=TRUE), 
                          list.files(list.files(temp2, full.names=TRUE), pattern = ".shp$", full.names = TRUE))
  unlist(temp)
  unlist(temp2)
  return(your_SHP_file)
}

# remove created variables for packages
rm(list.of.packages,new.packages)


geometry = st_sfc(st_point(c(-74.0037626, 40.7916781)), 
                  st_point(c(-73.757623, 40.631025)), 
                  st_point(c(-74.105017, 40.658491)), 
                  st_point(c(-73.948387, 40.551264)), 
                  st_point(c(-73.944587, 40.898989)))

boro_label_locations = st_sf(boro = c("Manhattan", "Queens", "Staten Island", 
                                      "Brooklyn", "Bronx"), 
                             geometry = geometry) %>% 
  st_set_crs(st_crs(4326))

