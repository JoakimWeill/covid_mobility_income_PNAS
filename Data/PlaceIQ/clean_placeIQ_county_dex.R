#---------------------------------------------------------------------------------------------------------------
#---------- This code loads and clean the county-level placeIQ data
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) 
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","sf","tigris","lubridate")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map
select<-dplyr::select

#---------------------------------------------------------------------------------------------------------------
datain <- "Data/PlaceIQ/COVIDExposureIndices-master"
dat <- fread(file.path(datain,"/dex_data/county_dex.csv"))

glimpse(dat)

dat %<>%
  as_tibble %>%
  rename(FIPS = county,
         Date = date,
         Device_exposure = dex,
         Num_devices = num_devices,
         Device_exposure_a = dex_a,
         Num_devices_a = num_devices_a) %>%
  mutate(FIPS = as.character(FIPS))


dat %<>%
  mutate(FIPS = if_else(nchar(FIPS)==4,paste0("0",FIPS),FIPS) ) %>%
  mutate(County_FIPS = FIPS, 
         Date = as.Date(Date))

glimpse(dat)
table(dat$FIPS)
table(dat$Date)
summary(dat)

saveRDS(dat, "Data/PlaceIQ/placeIQ_county_device_exposure.rds")
