## Exploring Population Patterns in YYJ

## get R packages
library(glue) #glue strings
library(cansim) #get StatsCan data
library(dplyr) #data munging
library(ggplot2) #plotting
library(scales) #number commas
library(here) #easier paths
library(curl) #pull data from web
library(readxl) #import xls
library(tidyr) #reshape data frame
library(readr) #import csv
library(sf) #mapping
library(bcmaps) #get bc maps
library(gganimate) #animate plots (package available from GitHub, https://github.com/thomasp85/gganimate)
library(patchwork) #side by side plots (package available from GitHub, https://github.com/thomasp85/patchwork)

path <- here("R", "bc-sd61-popn")

theme_plots <- theme(axis.text = element_text(size = 8),
                     plot.title = element_text(size = 12),
                     plot.subtitle = element_text(size = 10),
                     axis.title = element_blank())


##############################
## YYJ Population Estimates ##
##############################

## Get Victoria Census Metropolitan Census Area (British Columbia) Population Estimates [Table: 17-10-0078-01 (formerly CANSIM  051-0056) 'Annual demographic estimates by census metropolitan area, age and sex, based on the Standard Geographical Classification'] from Statistics Canada.
## Statistics Canada Data is released under the Statistics Canada Open Licence Agreement 
## https://www.statcan.gc.ca/eng/reference/licence).

#temp code
if (!exists("bc_popn")) load(glue(path,"/data/temp.RData"))

# #get data using cansim package
# bc_popn <- get_cansim(1710007801) %>% 
#   filter(GEO == "Victoria, British Columbia",
#          Sex == "Both sexes") %>% 
#   select(year = REF_DATE, age_group = `Age group`, population_estimate = VALUE) %>% 
#   mutate(year = as.integer(year))
# 
# save(bc_popn, file = glue(path, "/data/temp.RData"))

#plot of yyj population estimates
yyj_plot <- bc_popn %>% 
  filter(age_group == "All ages") %>% 
  ggplot(aes(x = year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f") +
  scale_x_continuous(limits = c(2001, 2018),
                     breaks = seq(2002, 2017, 3),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(320000, 380000),
                     breaks = seq(320000, 380000, 5000),
                     expand = c(0,0), labels = comma) +
  labs(title = "Population Estimates for Victoria CMA",
       subtitle = "Include all ages",
       caption = "Data sourced from Statistics Canada") +
  theme_minimal() +
  theme_plots
plot(yyj_plot)

#kid age groups
kid_ages <- c("0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years")

#plot of yyj kid population estimates
yyj_kids_plot <- bc_popn %>% 
  filter(age_group %in% kid_ages) %>% 
  group_by(year) %>% 
  summarise(population_estimate = sum(population_estimate)) %>% 
  ggplot(aes(x = year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f") +
  scale_x_continuous(limits = c(2001, 2018),
                     breaks = seq(2002, 2017, 3),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(64000, 70000),
                     breaks = seq(64000, 70000, 500),
                     expand = c(0,0), labels = comma) +
  labs(title = "Kid Population Estimates for Victoria CMA",
       subtitle = "Kids include ages 0 to 19 years",
       caption = "Data sourced from Statistics Canada") +
  theme_minimal() +
  theme_plots
plot(yyj_kids_plot)

#plot the victoria cma plots
yyj_plot + yyj_kids_plot


####################################
## Esquimalt Population Estimates ##
####################################

## Get Esquimalt District Municipality population estimates from the BCStats Population Estimates webpage:
## https://www2.gov.bc.ca/gov/content?id=36D1A7A4BEE248598281824C13CB65B6 
## BCStats data is released under the B.C. Crown Copyright Licence Agreement (https://www2.gov.bc.ca/gov/content/home/copyright).

crd <- c("Central Saanich", "Colwood", "Esquimalt", "Highlands",
         "Langford", "Metchosin", "North Saanich", "Oak Bay", "Saanich",
         "Sidney", "Sooke", "Victoria", "View Royal")

#get CRD population estimates 2001-2011
bcstats_mun_2001_2011 <- "http://www.bcstats.gov.bc.ca/Files/0379a32f-cec8-438d-83e0-6724b2a2a272/BCDevelopmentRegionRegionalDistrictandMuncipalPopulationEstimates2001-2011.xls"

get_mun_2001_2011 <- curl_download(bcstats_mun_2001_2011,
                                 destfile = glue(path, "/data/bcstats_mun_2001_2011.xls"))

crd_2001_2011 <- read_xls(get_mun_2001_2011, skip = 3) %>%  
  filter(Name %in% crd) %>% 
  select(-`2011`, -Type, -SGC)

#get CRD population estimates 2011-2017 (why 2 files?) and merge with first file
bcstats_mun_2011_2017 <- "http://www.bcstats.gov.bc.ca/Files/285cd56c-9be1-4c5e-a153-3deeffa2ac94/BCDevelopmentRegionRegionalDistrictandMuncipalPopulationEstimates2011-2015.xls"

get_mun_2011_2017 <- curl_download(bcstats_mun_2011_2017,
                                 destfile = glue(path, "/data//bcstats_mun_2011_2017.xls"))

crd_2001_2017 <- read_xls(get_mun_2011_2017, range = ("A3:J48")) %>% 
  filter(Name %in% crd) %>% 
  bind_cols(crd_2001_2011) %>% 
  select(-`2011`, -`Area Type`, -SGC, -Name1) %>% 
  gather(key =  year, value = population_estimate, -Name) %>% 
  mutate(population_estimate = as.numeric(population_estimate),
         year = as.integer(year)) %>% 
  arrange(year)

#plot of Township of Esquimalt population estimates
esquimalt_plot <- crd_2001_2017 %>% 
  filter(Name == "Esquimalt") %>%
  ggplot(aes(x = year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f") +
  labs(title = "Population Estimates for Esquimalt",
       subtitle = "Includes all ages",
       caption = "Data sourced from BCStats") +
  scale_x_continuous(limits = c(2001, 2018),
                     breaks = seq(2002, 2017, 3),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(16400, 17600),
                     breaks = seq(16400, 17600, 100),
                     expand = c(0,0), labels = comma) +
  theme_minimal() +
  theme_plots
plot(esquimalt_plot)

#calculate CRD population change for 2015 to 2017
crd_change <- crd_2001_2017 %>% 
  filter(year %in% c(2015, 2017)) %>%
  group_by(Name) %>% 
  mutate(popchange = population_estimate-lag(population_estimate)) %>% 
  mutate(percchange = round((popchange/lag(population_estimate) * 100), digits = 0)) %>% 
  filter(year == 2017) %>% 
  select(-year) 

#map CRD population change for 2015 to 2017
crd_change_spatial <- municipalities() %>% 
  filter(ADMIN_AREA_ABBREVIATION %in% crd) %>% 
  mutate(Name = ADMIN_AREA_ABBREVIATION) %>% 
  left_join(crd_change) %>% 
  ggplot() +
  geom_sf(aes(fill = percchange)) +
  coord_sf(datum = NA) +
  labs(title = "CRD Percent Population Change 2015-2017",
       subtitle = "Includes all ages",
       caption = "Data sourced from BCStats") +
  scale_fill_gradient(name = "Percent\nChange", low = "#bcbddc", high = "#132B43") +
  theme_minimal() +
  theme_plots
plot(crd_change_spatial)



###############################
## SD61 Population Estimates ##
###############################

## Get School District 61 Population Estimates & Projections from BCStats manual online tool: https://www.bcstats.gov.bc.ca/apps/PopulationProjections.aspx. See the /data/README.md file for manual download details.
## BCStats data is released under the B.C. Crown Copyright Licence Agreement (https://www2.gov.bc.ca/gov/content/home/copyright).

# get SD61 population estimates & projections
sd61_kids <- read_csv(glue(path, "/data/Population_Projections.csv")) %>% 
  select(Year, "<1", "1-4", "5-9", "10-14", "15-19") %>% 
  gather(key = age_group, value = population_estimate, -Year) %>% 
  filter(Year < 2038) %>% 
  mutate(age_group = factor(age_group, ordered = TRUE,
                            levels = c("<1", "1-4", "5-9", "10-14", "15-19")),
         data_type = case_when(Year < 2018 ~ "estimated",
                               TRUE ~ "projected"))

#plot of total sd61 kid population estimates
sd61_kids_total_plot <- sd61_kids %>% 
  group_by(Year, data_type) %>% 
  summarise(population_estimate = sum(population_estimate))  %>%
  mutate(age_group = "Total") %>% #need this for the animated version
  ggplot(aes(x = Year, y = population_estimate, colour = data_type, group = 1)) + 
  geom_point(size = 2) +
  geom_line(size = 1) +
  labs(title = "Kid Population Estimates & Projections for School District 61 (Greater Victoria)",
       subtitle = "Kids include ages 0 to 19 years",
       caption = "Data sourced from BCStats") +
  scale_y_continuous(limits = c(35000, 43000), breaks = seq(35000, 43000, 500), expand = c(0,0), labels = comma) +
  scale_x_continuous(limits = c(1984, 2040), breaks = seq(1986, 2038, 5), expand = c(0,0)) +
  scale_colour_manual(name = "", values = c("#54278f", "#9e9ac8")) +
  theme_minimal() +
  theme_plots +
  theme(legend.position = c(.2,.2),
        legend.text = element_text(size = 12))
plot(sd61_kids_total_plot)

#animated version
p <- sd61_kids_total_plot +
  transition_reveal(age_group, Year)

#save to gif
anim_save(glue(path, "/data/sd61_kid_popn.gif"),
          animate(p, width = 700, height = 600))

#facet plot of kid population estimates in SD61 by age group
sd61_kids_facet_plot <- sd61_kids %>% 
  filter(age_group != "<1") %>% 
  ggplot() +
  geom_line(aes(x = Year, y = population_estimate, linetype = data_type),
            size = 1, colour = "#54278f") +
  facet_wrap(~ age_group) +
  labs(title = "Kid Population Estimates by Age Group for SD61",
       subtitle = "Kids include ages 1 to 19 years",
       caption = "Data sourced from BCStats") +
  scale_y_continuous(breaks = seq(6000, 13000, 1000), expand = c(0,0), labels = comma) +
  scale_x_continuous(limits = c(1986, 2040), breaks = seq(1990, 2038, 4), expand = c(0,0)) +
  scale_linetype_discrete(name = "") +
  theme_bw() +
  theme_plots +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) 
plot(sd61_kids_facet_plot)

#animated version
m <- sd61_kids_facet_plot +
    transition_reveal(age_group, Year)

#save to gif
anim_save(glue(path, "/data/sd61_popn_facet.gif"),
          animate(m, width = 700, height = 600))

#animated plot of kid population estimates in SD61 by age group
sd61_kids_line_plot <- sd61_kids %>% 
  filter(age_group != "<1") %>% 
  ggplot(aes(Year, population_estimate, group = age_group, colour = data_type)) +
  geom_line(size = 1) +
  geom_segment(aes(xend = 2037 , yend = population_estimate), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 2037.1 , label = age_group), hjust = 0, colour = "#54278f") + 
  coord_cartesian(clip = 'off') + 
  labs(title = "Kid Population Estimates & Projections by Age Group for SD61",
       # subtitle = "Kids include ages 1 to 19 years",
       caption = "Data sourced from BCStats") +
  theme(plot.margin = margin(5.5, 0, 5.5, 5.5)) +
  scale_y_continuous(breaks = seq(6000, 13000, 1000), expand = c(0,0), labels = comma) +
  scale_x_continuous(limits = c(1984, 2040), breaks = seq(1986, 2038, 5), expand = c(0,0)) +
  scale_colour_manual(name = "", values = c("#54278f", "#9e9ac8")) +
  scale_fill_manual(name = "", values = c("#54278f", "#9e9ac8")) +
  theme_minimal() +
  theme_plots +
  theme(legend.position = "top",
        legend.text = element_text(size = 12))
plot(sd61_kids_line_plot)

#animated version
d <- sd61_kids_line_plot +
  transition_reveal(age_group, Year)

#save to gif
anim_save(glue(path, "/sd61_kid_popn_ages.gif"),
          animate(d, width = 700, height = 600))
