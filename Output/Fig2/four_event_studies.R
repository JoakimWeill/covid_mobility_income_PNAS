#---------------------------------------------------------------------------------------------------------------
#---------- This is the original code used to produce Fig2
# Due to data access restrictions, the safegraph data was removed from the dataset
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) 
getwd()

my_packages <- c("tidyverse","magrittr","modeest","lfe","stargazer", "broom", "future", "furrr", "Formula","haven")
sapply(my_packages, require, character.only = TRUE)
sapply(my_packages, function(x) packageVersion(x) %>%  as.character)

map<-purrr::map 
select<-dplyr::select

source("Output/general_options.R")
options(scipen=999)

#------------------------------------------------------------------------------------------------------------

df <- readRDS("processed_data_PNAS/df_event_study.rds")


#Outcome variables
list_var <- c("Completely_home_log","Median_distance_traveled_log", # https://www.safegraph.com/covid-19-data-consortium for data access (free for COVID-19 research)
              "Device_exposure_log","gm_retail_and_recreation")

list_names <- c("Completely at Home (log)","Median Distance Traveled (log)",
                "Device Exposure (log)","Retail and Recreation (rel. baseline)")

## df of formulas // most useful for robustness checks
list_var_df <- tibble(list_var=list_var,
                      list_names = list_names)


var_regs <- names(df)
var_regs <- var_regs[str_detect(var_regs,paste0("State_ED_b|","State_ED_f|","State_ED_0")) ]

#Set baseline (remove the dummy)
var_regs <- var_regs[!str_detect(var_regs,"_b1$")]

var_regs_interacted <- paste0(var_regs,":Quantile_group")

var_regs_final <- paste(var_regs_interacted,
                        sep = "", 
                        collapse =  " + ")

# Add cumulative number of cases in the model 
var_regs_final <- paste0(var_regs_final, " + Cum_cases") 


#--------- Put in formula
formula_felm <- Formula::as.Formula(
  sprintf( paste0("y ~", var_regs_final," | Date + FIPS | 0 | FIPS") )
)

# Set up in a tibble to run multiple formulas for robustness checks
formus <- tibble(formula = c(formula_felm),
                 formu_num = 1:1)


#--------------------------- RUN ALL ---------------------------

if(interactive()) future::plan(sequential) else future::plan(multiprocess)
options(mc.cores=8)

out <- expand_grid(list_var_df, formus) %>% 
  mutate(formula = map2_chr(formula, list_var, ~ as.character(.x) %>% 
                              str_replace("^y", .y)),
         reg = furrr::future_map(formula, ~felm(as.Formula(.x), data=df) ))


# Put in tidy format
out %<>%
  mutate(coefs = map(reg, ~tidy(.)))

#Rename outcome variable, remove potential controls
out %<>%
  mutate(coef2 = map2(coefs, list_names, ~mutate(.x, Dep_var = .y))) %>%
  mutate(coef2 = map(coef2, ~filter(.x, !(term%in% c("Cum_cases", "Cum_cases_before_ED")))) ) 


all_coefs <- bind_rows(out$coef2)


#---- Add an empty row for the baseline

all_coefs %<>%
  mutate(add_index = as.integer(rownames(all_coefs)) ) %>%
  mutate(add_index = if_else(str_detect(term,"State_ED_0"),
                             add_index,as.integer(0)) )  

adding_rows <- unique(all_coefs$add_index)[-1] #remove 0 

for (i in 1:length(adding_rows)) {
  all_coefs %<>%
    add_row(term = "", 
            estimate = 0,
            std.error = 0,
            statistic = 0,
            p.value = 0,
            Dep_var = "",
            add_index = 0,
            .before = adding_rows[i])
  adding_rows = adding_rows +1 #need to move the indices
}

#--------- Rename the groups 

all_coefs %<>%
  mutate(Dep_var = ifelse(Dep_var == "", lead(Dep_var),Dep_var)) %>%
  mutate(term = ifelse(term == "", lead(term),term)) %>%
  mutate(Regressor = case_when(str_detect(term, "State_ED") ~ "State_ED",
                               TRUE ~ term)) %>%
  mutate(Quantile = case_when(str_detect(term, "0%_20%") ~ "0%_20%",
                              str_detect(term, "20%_40%") ~ "20%_40%",
                              str_detect(term, "40%_60%") ~ "40%_60%",
                              str_detect(term, "60%_80%") ~ "60%_80%",
                              str_detect(term, "80%_100%") ~ "80%_100%",
                              TRUE ~ "baseline"))

#------Adding Year index

all_coefs %<>%
  group_by(Dep_var,Quantile) %>%
  mutate(Year = seq(-21,21))


glimpse(all_coefs) #check

cleaned_coefs <- all_coefs %>% 
  ungroup() %>% 
  filter(Quantile!="baseline") %>% 
  mutate(Quantile = str_replace(Quantile, "_", "-") %>% 
           str_remove("%")) %>%
  filter(Year!=-21&Year!=21)

pl_4Event <-  ggplot() + #color=Lambda in AES
  geom_point(data = cleaned_coefs %>% filter(Quantile %in% c("0-20%","80-100%")),
             aes(x=Year,y=estimate,color =Quantile),  size = 0.4,alpha=1) + # show.legend = TRUE
  geom_line(data = cleaned_coefs %>% filter(Quantile %in% c("0-20%","80-100%")),
            aes(x=Year,y=estimate,color =Quantile), 
            show.legend = FALSE, size = 0.4, alpha = 1)+
  geom_errorbar(data = cleaned_coefs %>% filter(Quantile %in% c("0-20%","80-100%")),
                aes(x=Year, ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error,color = Quantile),
                width=.03, position=position_dodge(0.05), show.legend = FALSE) +
  geom_point(data = cleaned_coefs %>% filter(!(Quantile %in% c("0-20%","80-100%")) ),
             aes(x=Year,y=estimate,color =Quantile),  size = 0.2,alpha=1) + # show.legend = TRUE
  geom_line(data = cleaned_coefs %>% filter(!(Quantile %in% c("0-20%","80-100%")) ),
            aes(x=Year,y=estimate,color =Quantile), 
            show.legend = FALSE, size = 0.1, alpha = 0.7)+
  geom_errorbar(data = cleaned_coefs %>% filter(!(Quantile %in% c("0-20%","80-100%")) ),
                aes(x=Year, ymin=estimate-1.645*std.error, ymax=estimate+1.645*std.error,color = Quantile),
                width=.03, position=position_dodge(0.05), show.legend = FALSE, size = .2) +
  geom_hline(yintercept=0) +
  facet_wrap(~Dep_var, scales = "free_y")+
  xlab("Day relative to declaration") + 
  ylab("Event-study coefficients") +
  scale_color_viridis_d(option = "D", begin = 0, end = 1)+
  theme_jo +
  theme(panel.grid.major = element_line(colour="gray75", size = (.75)), 
        panel.grid.minor = element_blank(),
        legend.position="bottom",
        legend.box = "horizontal")+
  guides(fill = guide_legend(nrow = 1)) +
  labs(color = "Income quintile:")
pl_4Event


### PNAS Formatting

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



fig_events_4_up_line <- pl_4Event + theme_pnas +
  guides(color = guide_legend(override.aes = list(size=2, shape = 15), nrow=1)) +
  geom_vline(xintercept = -1, linetype=2, color = "darkgrey")

fig_events_4_up_line

ggsave(fig_events_4_up_line, filename = "Output/Fig2/four_events_facet_PNAS_1col_line.pdf",
       width = 8.7, height = 8.7, units = "cm")
# Slight change to the font used

Sys.setenv(R_GSCMD = "/usr/bin/gs")
tibble(full_path=list.files("Output/Fig2", pattern = "\\.pdf$", full.names = TRUE)) %$% 
  walk(full_path, extrafont::embed_fonts)



