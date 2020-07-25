
#-------------------------------------------------------------------------------------------------------------------------
#---------- This code loads and cleans all US census tracts to get sf object -- first need to get census API key
#-------------------------------------------------------------------------------------------------------------------------

rm(list = ls()) 
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","sf","rgdal","foreign","tictoc","tidycensus","tigris","units")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

options(tigris_use_cache = TRUE) #To load API census key

#--------------------------------------------  ----------------------------------------------------------------
fips <- fips_codes  %>% 
  as_tibble() %>%
  filter(! (state_code %in% c("60","66","69","74","78")) )
#american Samoa, Guam, 69=Mariana Islands, 74=Outlying area under US sovereignty, 78 = virgin islands


fips <- as.numeric(unique(fips$state_code))[-1] #removing first one



#----- Get geometries and tract medincome in past 12-months inflation adjusted, pooled ACS
sf <- get_acs(geography = "tract", 
                            state = 1,
                            variables = c("B19326_001"),   
                            geometry = T)

for (i in fips) {
  
  tract_sf <- get_acs(geography = "tract", 
                      state = i,
                      variables = c("B19326_001"),    
                      geometry = T)
  
  sf <- rbind(sf,tract_sf)
  message(paste0(i, " done!"))
  message(nrow(sf))
}


#************************************************


#------ cleaning
census_tracts.sf <- sf %>%
  separate(NAME, sep = ",", into = c("Census_tract", "County","State"), remove = T) %>%
  mutate(State = str_trim(State),
         County = str_trim(County),
         Census_tract = str_trim(Census_tract)) %>%
  mutate(County_name = County,
         State_name = State) 

glimpse(census_tracts.sf)



test <- census_tracts.sf %>% filter(is.na(State) | is.na(County) | is.na(Census_tract))
if(nrow(test)==0) rm(test) else stop("problem!!")



message("Some weird empty geometries!")
check <- census_tracts.sf %>% filter(is.na(st_dimension(.))) #removing 219 tracts .... need to check why these do not appear with tidy census

#-------- Generate variables

census_tracts.sf %<>%
  dplyr::rename(Median_income_tract = B19326_001)

summary(census_tracts.sf)



###--------------- Quintiles --------------------

quantiles_tracts <- quantile(census_tracts.sf$Median_income_tract,seq(0,1,.2), na.rm = T)
quantiles_labels <- labels(quantiles_tracts)

for (i in 1:(length(quantiles_tracts)-1)) {
  quantiles_labels[i] = paste0(quantiles_labels[i],"_",quantiles_labels[i+1]) #get classes right
}
quantiles_labels <- quantiles_labels[-6] #remove last one

census_tracts.sf %<>%
  mutate(Income_quintile_group = cut(Median_income_tract, breaks = quantiles_tracts, labels = quantiles_labels ))

###### SAVE
saveRDS(census_tracts.sf,"Data/map_census_tracts_full_us.rds")

