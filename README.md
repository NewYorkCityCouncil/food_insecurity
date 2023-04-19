## Oversight: Food Insecurity in New York City 

An associated webpage for this analysis can be found [on the council website](https://council.nyc.gov/data/emergency-food-in-nyc/): 

### Data sources

Community District population is sourced from the [Department of City Planning](https://www.nyc.gov/site/planning/planning-level/nyc-population/2020-census.page)

current efap locations - [EFAP_pdf_3_6_23.csv](https://github.com/NewYorkCityCouncil/efap/blob/master/data/processed/EFAP_pdf_3_6_23.csv)

geocoded efap location - [efap.geojson](https://github.com/NewYorkCityCouncil/efap/blob/master/data/processed/efap.geojson)

### Summary & Intention

On April 19, 2023, the Committee on General Welfare, chaired by Deputy Speaker Diana Ayala, the Committee on Aging, chaired by Council Member Crystal Hudson, and the Subcommittee on Senior Centers and Food Insecurity, chaired by Council Member Darlene Mealy, will conduct an oversight hearing to examine food insecurity in New York City. Representatives from the Human Resources Administration (HRA), the Department for the Aging (NYC Aging), anti-hunger advocates, emergency food providers, and other interested parties were invited to testify.  

SNAP, formerly known as the Food Stamp Program, is the cornerstone of the nation’s safety net and nutrition assistance programs, providing assistance to millions of eligible low-income people. Benefit levels for SNAP are based on criteria including, but not limited to, household size and income levels. Prior to the pandemic, SNAP households received an average of $240 a month. From April 2020 through February 2023, SNAP benefits increased temporarily due to COVID-19 legislation. However, upon the expiration of emergency allotments in 2023, it is estimated that SNAP benefits will decrease to $182 per month per person, or $6.00 per person per day.

HRA’s Community Food Connection (CFC), formerly the Emergency Food Assistance Program (EFAP), administers funding and coordinates the distribution of shelf-stable food to more than 500 food pantries and community kitchens citywide. Reports from the end of 2022 show that food pantries had served 7,315,960 individuals and community kitchens had served 795,751 meals in New York City. CFC also provides administrative support to cover utilities, equipment, office supplies, and personnel to a food provider (food pantries and community kitchens) with the aim to improve the nutritional status of low-income New Yorkers.

### Main Takeaways

* About 1 in every 5 people were enrolled in SNAP in NYC.  All areas experienced an in utilization per capita, expect for the FiDi & Midtown. However, areas with lower enrollment pre-pandemic had greater increases, especially in Queens and Staten Island. 
* Across NYC, there are 559 emergency food providers, enrolled in HRA’s Community Food Connection (CFC). Community board districts with more CFCs tend to also have a higher % of SNAP recipients per capita. There are some exceptions


### Scripts + Replication 

* **00_load_dependencies.R** - reads in the necessary dependencies and defines some data to be used throughout the code 
* **01_geocode_CFC_locations.R** - reads in the addresses of the Community Food Connections locations and geocodes them using the Google API. The CSV that this file reads in is already a processed product created through a [past project](https://github.com/NewYorkCityCouncil/efap/blob/master/scripts/efap_pdf.R) from the NYC Council data team. The code for this input is linked above and draws from the "currently active" CFC pdf.  
* **02_process_CD_areas.R** - creates the metrics we will map in later files, mostly at the community district level but some at council district level. Pulls in SNAP numbers for Dec 2019 and Dec 2022, and combines names, population, and shapefiles at the community district level. Ouputs `community_district_data.RDS` and `council_district_data.RDS` that are used for mapping. 
* **03_create_apr2023_cfc_map.R** - creates an interactive map for the website of the CFC locations, as well as a council district static pdf map of the count of CFC locations by district. 
* **04_create_dec2022_snap_map.R** - creates a static map of % of each community district that receives SNAP, an interactive map showing SNAP coverage and change, and a table summary output in xlsx format. 


