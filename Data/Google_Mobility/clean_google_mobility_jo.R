#'---------------------------------------------------------------------------------------------------------------
# This code downloads and clean Google Mobility data
#'---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse", "sf","magrittr")
sapply(my_packages, require, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

#-------------------------------------------- ----------------------------------------------------------------

################################
#'## Download latest
################################

download.file("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=911a386b6c9c230f",
              "Data/Google_mobility/google_mobility_latest.csv")

gm <- read.csv("Data/Google_mobility/google_mobility_latest.csv")

glimpse(gm)

gm %<>%
  as_tibble() %>%
  filter(country_region=="United States") %>%
  filter(!is.na(sub_region_2)) %>%
  filter(sub_region_2!="") %>%
  select(-country_region_code,-country_region) %>%
  mutate(FIPS = as.character(census_fips_code)) %>%
  mutate(FIPS = ifelse(nchar(FIPS)==4, paste0("0",FIPS),FIPS)) %>%
  select(-census_fips_code) %>%
  mutate(Date = as.Date(date))

gm_cleaned <- gm %>%
  select(-date) %>%
  rename(State_name = sub_region_1, County_name = sub_region_2) %>%
  select(-iso_3166_2_code)

check <- distinct(gm_cleaned,FIPS,Date)

gm_cleaned <- gm_cleaned %>%
  rename_all(~str_remove(., "_percent_change_from_baseline")) 

glimpse(gm_cleaned)
# Note lots of missing data

gm_cleaned %<>%
  rename(gm_retail_and_recreation = retail_and_recreation,
         gm_grocery_and_pharmacy = grocery_and_pharmacy,
         gm_parks = parks,
         gm_transit_stations = transit_stations,
         gm_workplaces = workplaces,
         gm_residential = residential)


#### Saving

saveRDS(gm_cleaned, "Data/Google_Mobility/Google_Mobility_cleaned.rds")