## Exploring Population Patterns in YYJ

## get packages
library(cansim) #get StatsCan data
library(dplyr) #data munging
library(ggplot2) #plotting
library(scales) #number commas
library(here) #easier paths
library(curl) #pull data from web
library(readxl) #import xls
library(tidyr) #reshaor data frame
library(readr) #import csv
library(sf)
library(bcmaps)

## Get Victoria Census Metropolitan Census Area (British Columbia) Population Estimates [Table: 17-10-0078-01 (formerly CANSIM  051-0056) 'Annual demographic estimates by census metropolitan area, age and sex, based on the Standard Geographical Classification'] from Statistics Canada.
## Statistics Canada Data is released under the Statistics Canada Open Licence Agreement 
## https://www.statcan.gc.ca/eng/reference/licence).

#get data using cansim package
bc_popn <- get_cansim(1710007801) %>% 
  filter(GEO == "Victoria, British Columbia",
         Sex == "Both sexes") %>% 
  select(year = REF_DATE, age_group = `Age group`, population_estimate = VALUE) 

## YYJ population estimates

#plot of yyj population estimates
yyj_plot <- bc_popn %>% 
  filter(age_group == "All ages") %>% 
  ggplot(aes(x = year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f") +
  labs(title = "Population Estimates for Victoria Census Metropolitan Area (British Columbia)",
       subtitle = "The Victoria Census Metropolitan Area is the CRD minus the Gulf Islands",
       caption = "Data sourced from Statistics Canada (Table: 17-10-0078-01)",
       y = "", x = "") +
  scale_y_continuous(limits = c(320000, 380000), breaks = seq(320000, 380000, 5000), expand = c(0,0), labels = comma) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())
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
  labs(title = "Population Estimates for Kids in Victoria Census Metropolitan Area (British Columbia)",
       subtitle = "Kids include ages 0 to 19 years",
       caption = "Data sourced from Statistics Canada (Table: 17-10-0078-01)",
       y = "", x = "") +
  scale_y_continuous(limits = c(64000, 70000), breaks = seq(64000, 70000, 500), expand = c(0,0), labels = comma) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())
plot(yyj_kids_plot)



## Get Esquimalt District Municipality population estimates from the BCStats Population Estimates webpage:
## https://www2.gov.bc.ca/gov/content?id=36D1A7A4BEE248598281824C13CB65B6 
## BCStats data is released under the B.C. Crown Copyright Licence Agreement (https://www2.gov.bc.ca/gov/content/home/copyright).

crd <- c("Central Saanich", "Colwood", "Esquimalt", "Highlands",
         "Langford", "Metchosin", "North Saanich", "Oak Bay", "Saanich",
         "Sidney", "Sooke", "Victoria", "View Royal")

# get CRD population estimates 2001-2011
bcstats_mun_2001_2011 <- "http://www.bcstats.gov.bc.ca/Files/0379a32f-cec8-438d-83e0-6724b2a2a272/BCDevelopmentRegionRegionalDistrictandMuncipalPopulationEstimates2001-2011.xls"

get_mun_2001_2011 <- curl_download(bcstats_mun_2001_2011,
                                 destfile = here("R", "bc-sd61-popn", "data",
                                                 "/bcstats_mun_2001_2011.xls"))

crd_2001_2011 <- read_xls(get_mun_2001_2011, skip = 3) %>%  
  filter(Name %in% crd) %>% 
  select(-`2011`, -Type, -SGC)

# get CRD population estimates 2011-2017 (why 2 files?) and merge with first file
bcstats_mun_2011_2017 <- "http://www.bcstats.gov.bc.ca/Files/285cd56c-9be1-4c5e-a153-3deeffa2ac94/BCDevelopmentRegionRegionalDistrictandMuncipalPopulationEstimates2011-2015.xls"

get_mun_2011_2017 <- curl_download(bcstats_mun_2011_2017,
                                 destfile = here("R", "bc-sd61-popn", "data",
                                                 "/bcstats_mun_2011_2017.xls"))

crd_2001_2017 <- read_xls(get_mun_2011_2017, range = ("A3:J48")) %>% 
  filter(Name %in% crd) %>% 
  bind_cols(crd_2001_2011) %>% 
  select(-`2011`, -`Area Type`, -SGC, -Name1) %>% 
  gather(key =  year, value = population_estimate, -Name) %>% 
  mutate(population_estimate = as.numeric(population_estimate)) %>% 
  arrange(year)

## Calculate and Map CRD population change for 2015 to 2017
crd_change <- crd_2001_2017 %>% 
  filter(year %in% c(2015, 2017)) %>%
  group_by(Name) %>% 
  mutate(popchange = population_estimate-lag(population_estimate)) %>% 
  mutate(percchange = round((popchange/lag(population_estimate) * 100), digits = 0)) %>% 
  filter(year == 2017) %>% 
  select(-year) 

crd_change_spatial <- municipalities() %>% 
  filter(ADMIN_AREA_ABBREVIATION %in% crd) %>% 
  mutate(Name = ADMIN_AREA_ABBREVIATION) %>% 
  left_join(crd_change) %>% 
  ggplot() +
  geom_sf(data = crd_spatial, aes(fill = percchange)) +
  coord_sf(datum=NA) +
  labs(title = "Percent Population Change 2014-2017 in the Capital Regional District",
       subtitle = "Includes all ages",
       caption = "Data sourced from BC Stats Population Estimates webpage (13 October 2018)",
       y = "", x = "") +
  scale_fill_gradient(name = "Percent\nChange", low = "#bcbddc", high = "#132B43") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())
plot(crd_change_spatial)


#plot of Township of Esquimalt population estimates
esquimalt_plot <- crd_2001_2017 %>% 
  filter(Name == "Esquimalt") %>%
  ggplot(aes(x = year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f") +
  labs(title = "Population Estimates for the Township of Esquimalt, British Columbia",
       subtitle = "Includes all ages",
       caption = "Data sourced from BC Stats Population Estimates webpage (13 October 2018)",
       y = "", x = "") +
  scale_y_continuous(limits = c(16400, 17600), breaks = seq(16400, 17600, 100), expand = c(0,0), labels = comma) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())
plot(esquimalt_plot)
  


## Get School District 61 Population Estimates & Projections from BCStats manual online tool: https://www.bcstats.gov.bc.ca/apps/PopulationProjections.aspx. See the /data/README.md file for manual download details.
## BCStats data is released under the B.C. Crown Copyright Licence Agreement (https://www2.gov.bc.ca/gov/content/home/copyright).

# get SD61 population estimates & projections
sd61_kids <- read_csv("R/bc-sd61-popn/data/Population_Projections.csv") %>% 
  select(Year, "<1", "1-4", "5-9", "10-14", "15-19") %>% 
  gather(key = age_group, value = population_estimate, -Year) %>% 
  filter(Year < 2028) %>% 
  mutate(age_group = factor(age_group, ordered = TRUE, levels = c("<1", "1-4", "5-9", "10-14", "15-19")))

#plot of total sd61 kid population estimates
sd61_kids_total <- sd61_kids %>% 
  group_by(Year) %>% 
  summarise(population_estimate = sum(population_estimate)) %>% 
  mutate(age_group = "Total")

sd61_kids_total_2001_2017 <-  sd61_kids_total %>% 
  filter(Year < 2018)

sd61_kids_plot <- ggplot(sd61_kids_total, aes(x = Year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f", linetype = "dotted") +
  geom_line(data = sd61_kids_total_2001_2017, aes(x = Year, y = population_estimate, group = 1),size = 1, colour = "#54278f") +
  labs(title = "Population Estimates & Projections for Kids in School District 61 (Greater Victoria)",
       subtitle = "Kids include ages 0 to 19 years",
       caption = "Data sourced from BC Stats Population Projections webpage (13 October 2018)",
       y = "", x = "") +
  scale_y_continuous(limits = c(35000, 43000), breaks = seq(35000, 43000, 500), expand = c(0,0), labels = comma) +
  scale_x_continuous(limits = c(1984, 2030), breaks = seq(1986, 2028, 2), expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.minor = element_blank())
plot(sd61_kids_plot)

#facet plot of kid population estimates in SD61 by age group
sd61_2001_2017 <- sd61_kids %>% 
  filter(Year < 2018)

sd61_projections <- sd61_kids %>%
  filter(Year > 2016, Year < 2029)

sd61_kids_facet_plot <- ggplot() +
  geom_line(data = sd61_2001_2017, aes(x = Year, y = population_estimate,
                                 group = age_group, colour = age_group), size = 1, colour = "#54278f") +
  geom_line(data = sd61_projections, aes(x = Year, y = population_estimate,
                                  group = age_group, colour = age_group), linetype = "dotted", size = 1, colour = "#54278f") +
  facet_wrap(~ age_group) +
  scale_colour_manual(name="", values = sd61_colr_palette) +
  labs(title = "Population Estimates for Kids by Age Group in School District 61 (Greater Victoria)",
       subtitle = "Kids include ages 0 to 19 years",
       caption = "Data sourced from BC Stats Population Projections webpage (13 October 2018)",
       y = "", x = "") +
  scale_y_continuous(expand = c(0,0), labels = comma) +
  scale_x_continuous(limits = c(1986, 2027), breaks = seq(1990, 2028, 4), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16))
plot(sd61_kids_facet_plot)
