#---------------------------------------------------------------------------------------------------------------
#---------- This code loads NYT cases and mortality data
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) 
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggrepel","sf","tidycensus","tigris")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

first_day <- "2020/01/20"
last_day <- "2020/05/24"
#---------------------------------------------------------------------------------------------------------------

nyt <- read_csv("Data/cases_mortality_NYT/covid-19-data-master/us-counties.csv")

glimpse(nyt)

# Clean the dataset and create instantaneous variables
nyt %<>%
  rename(Date = date, FIPS = fips, Cum_cases = cases, Cum_deaths = deaths) %>%
  select(-county) %>%
  mutate(state = str_to_title(state)) %>%
  arrange(Date) %>%
  group_by(FIPS) %>%
  mutate(lag_cases = dplyr::lag(Cum_cases)) %>%
  mutate(lag_deaths = dplyr::lag(Cum_deaths)) %>%
  mutate(lag_cases = if_else(is.na(lag_cases),0,lag_cases), #first entry when first case is confirmed
         lag_deaths = if_else(is.na(lag_deaths),0,lag_deaths)) %>% #so imputing 0 before
  mutate(Cases = Cum_cases - lag_cases,
         Deaths = Cum_deaths - lag_deaths) %>%
  select(-lag_deaths,-lag_cases) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(FIPS) %>%
  add_count() %>%
  filter(!is.na(FIPS))

min(nyt$Date)
max(nyt$Date)

length(unique(nyt$FIPS))

message("nyt data does not keep track of counties with 0 cases,we add them from the us.sf map")

#-------- Create NYT panel
all_days <- tibble(FIPS = unique(nyt$FIPS)[1], #first one just to join
                   "Date" = seq(as.Date(first_day), to = as.Date(last_day), by = "day"))

#---- Get correct county name
us_all <- readRDS("Data/map_US_tracts/map_census_tracts_full_us.rds")
us_all %<>%
  st_drop_geometry() %>%
  as_tibble() %>%
  select(FIPS,County_name,State_name)

complete_panel <- us_all  %>%
  left_join(nyt,by = "FIPS") 

check <- complete_panel %>%
  mutate(check = ifelse(State_name ==state,1,0)) %>%
  summarise(mean = mean(check, na.rm=T))

if (check==1) { 
  rm(check) 
  complete_panel %<>% 
    select(-state)
  message("all good")
} else {
  stop("problem")
}


glimpse(complete_panel)

complete_panel %<>%
  full_join(all_days, by = c("FIPS","Date")) %>%
  complete(Date, nesting(FIPS,County_name,State_name)) %>%
  filter(!is.na(Date) & !is.na(FIPS)) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(FIPS,State_name,County_name) %>%
  add_count() %>%
  ungroup()

table(complete_panel$n)

as.numeric(as.Date(last_day)-as.Date(first_day))+1

complete_panel %<>%
  select(-n) %>%
  replace_na(list(Cum_cases = 0, Cum_deaths = 0, Cases = 0, Deaths = 0))

glimpse(complete_panel)
table(complete_panel$Date)
length(unique(complete_panel$FIPS))
length(unique(nyt$FIPS))
summary(complete_panel)

saveRDS(complete_panel, "Data/clean_cases_mortality_NYT/cases_mortality_nyt.rds")
