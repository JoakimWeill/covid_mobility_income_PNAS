
#---------------------------------------------------------------------------------------------------------------
#---- This code generates Fig1 in the PNAS article -- data has been redacted to comply with Safegraph's policy
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) 
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","cowplot","sf","tictoc","lubridate")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

options(stringsAsFactors = FALSE) ## Do not load strings as factors



#-------------------- Load Safegraph data collapsed at the Quintile level

# This dataset has been removed to comply with data access restrictions;
# Access to the Safegraph data is free for COVID-related research and can be requested here: 
# https://www.safegraph.com/covid-19-data-consortium
sg <- readRDS("processed_data_PNAS/safeGraph_SD_cbg_income_quintile.rds")

sg <- sg %>%
  mutate("Median Distance Traveled (km)" = SD_distHome/1000,
         "Completely at Home (%)" = 100*SD_pctHome) %>%
  mutate(Date = as.Date(date)) %>%
  select(-date)


slong <- sg %>%
  pivot_longer(cols = c(-Income_quintile_group,-Date), names_to = "Variable") %>%
  filter(!is.na(Income_quintile_group)) %>%
  mutate(Day = weekdays(Date)) %>%
  filter(!Day %in% c("Saturday","Sunday"))


slong_filtered <- slong %>%
  filter(Variable %in% c("Completely at Home (%)","Median Distance Traveled (km)") ) %>%
  filter(Date != "2020-01-01") 

#------------------------ Google and PlaceIQ data ------------------------

df <- readRDS("processed_data_PNAS/df_raw_graph.rds")

glimpse(df)

message("needed for the plot...")
df %<>%
  rename(Income_quintile_group = Quantile_group)

#Compute mean by day and income quintile
av <- df %>%
  mutate(Income_quintile_group = as.character(Income_quintile_group)) %>%
  filter(!is.na(Income_quintile_group)) %>%
  group_by(Date,Income_quintile_group) %>%
  summarise("Device Exposure (number)" = mean(Device_exposure_a, na.rm = T),
     Mean_retail_rec = mean(gm_retail_and_recreation, na.rm = T)
  ) %>%
  mutate(Mean_retail_rec = ifelse(Mean_retail_rec=="NaN",NA,Mean_retail_rec)) %>%
  mutate(Mean_retail_rec = as.numeric(Mean_retail_rec)) %>%
  rename("Retail and Recreation (rel. baseline)" = Mean_retail_rec) %>%
  pivot_longer(cols = contains(c("Device","Retail")), names_to = "Variable") %>%
  group_by(Variable,Income_quintile_group) %>%
  filter(!is.na(value)) %>%
  mutate(Day = weekdays(Date)) %>%
  filter(!Day %in% c("Saturday","Sunday")) 

glimpse(av)
glimpse(slong_filtered)

append_mob <- bind_rows(slong_filtered,av) %>% 
  mutate(Income_quintile_group = str_replace(Income_quintile_group, "_", "-") %>% 
           str_remove("%")) %>%
  filter(Date != "2020-02-25") %>% #Post on SafeGraph slack declared data for this date was inflated
  filter(Date <= "2020-04-21")

p1 <- ggplot(append_mob) +
  geom_line(data = append_mob %>% filter(Income_quintile_group %in% c("0-20%","80-100%")) ,
            aes(x=Date, y=value, color = Income_quintile_group), size = 0.6)+
  geom_line(data = append_mob %>% filter(!(Income_quintile_group %in% c("0-20%","80-100%")) ) ,
            aes(x=Date, y=value, color = Income_quintile_group), size = 0.2)+
  scale_color_viridis_d(option = "D", begin = 0, end = 1)+
  facet_wrap(~Variable, scales = "free_y")+
  labs(color = "Income quintile:")+
  theme(legend.key.width = unit(2, "line")) +
  theme(axis.title.y=element_blank()) #+

p1

## PNAS formatting
pnas_sizes <- c(8.7, 11.4, 17.8)


main_size <- 8
labs_size <- 7
axis_size <- 6
leg_size <- 6
font_pnas <- "Helvetica"
theme_pnas <-   theme(
  text=element_text(family=font_pnas),
  axis.text.x=element_text(size=axis_size, margin=margin(t=1)),
  axis.text.y=element_text(size=axis_size, margin=margin(r=2)),
  axis.title=element_text(size=labs_size, family=font_pnas),
  axis.title.x=element_text(size=labs_size, family=font_pnas, face="plain"), 
  axis.title.y=element_text(size=labs_size, family=font_pnas, face="plain"), 
  axis.title.y.right=element_text(size=labs_size, angle=90, family=font_pnas, face="plain"),
  strip.text=element_text(hjust=.5, size=leg_size, face="plain", family=font_pnas),
  plot.margin=margin(rep(2, 4)),
  legend.title=element_text(size=leg_size),
  legend.text=element_text(size=leg_size, margin=margin()),
  legend.position="bottom",
  legend.box = "horizontal",
  legend.box.spacing = unit(0.1, 'cm'),
  legend.key = element_rect(fill = NA),
  legend.key.size = unit(0.2, 'cm'),  
  panel.background = element_rect(fill = "white"),
  panel.grid=element_line(color="#cccccc", size=0.2),
  panel.grid.major=element_line(color="#cccccc", size=0.2),
  panel.grid.minor = element_blank())


################### Calendar time #############################

fig_calend_time_up <- p1 + theme_pnas+
  theme(legend.key = element_rect(fill = NA),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height =  unit(0.2, "cm"),
        axis.title.y = element_blank()) +
  labs(color = "Income quintile:") +
  guides(color = guide_legend(override.aes = list(size=2)))

fig_calend_time_up

# Saving

ggsave(fig_calend_time_up, filename = "Output/Fig1/PNAS_graph_rawSeries_1Col.pdf",
       width = 8.7, height = 8.7, units = "cm")


# Slight change to the fond used

Sys.setenv(R_GSCMD = "/usr/bin/gs")
tibble(full_path=list.files("Output/Fig2", pattern = "\\.pdf$", full.names = TRUE)) %$% 
  walk(full_path, extrafont::embed_fonts)
