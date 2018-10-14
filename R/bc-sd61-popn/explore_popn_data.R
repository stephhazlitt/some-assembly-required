## Exploring Population Patterns in YYJ

## get packages
library(cansim) #get StatsCan data
library(dplyr) #data munging
library(ggplot2) #plotting
library(RColorBrewer) #colour palette
library(here)
library(curl) #pull data from web
library(readxl) #data import
library(tidyr)

## Get Victoria, British Columbia Population Estimates [Table: 17-10-0078-01 (formerly CANSIM  051-0056) 'Annual demographic estimates by census metropolitan area, age and sex, based on the Standard Geographical Classification'] from Statistics Canada
## Data is released under the Statistics Canada Open Licence Agreement 
## https://www.statcan.gc.ca/eng/reference/licence)

#get data using cansim package
bc_popn <- get_cansim(1710007801) %>% 
  filter(GEO == "Victoria, British Columbia",
         Sex == "Both sexes") %>% 
  select(year = REF_DATE, age_group = `Age group`, population_estimate = VALUE) 

#kid age groups
kid_ages <- c("0 to 4 years", "5 to 9 years", "10 to 14 years")

#plot colours
colr_palette <-  brewer.pal(4,"Purples")[2:4]

#plot of kid popn in yyj
yyj_kids_plot <- bc_popn %>% 
  filter(age_group %in% kid_ages) %>% 
  ggplot(aes(x = year, y = population_estimate,
             group = age_group, colour = age_group)) + 
  geom_line(size = 1) +
  scale_colour_manual(name="", values = colr_palette) +
  labs(title = "Population Estimates for Kids in Victoria, British Columbia",
       caption = "Data sourced from Statistics Canada (Table: 17-10-0078-01)",
       y = "", x = "") +
  scale_y_continuous(limits = c(13000, 19000), breaks = seq(13000, 19000, 1000), expand = c(0,0), labels = comma) +
  theme_minimal() +
  theme(legend.position = c(.8,.7),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())
plot(yyj_kids_plot)


# grownup_ages <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 to 89 years", "90 years and over")

# grownups <- bc_pop %>% 
#   filter(age_group %in% grownup_ages)

## Get Esquimalt District Municipality population estimates from BCStats
## Data is provided online 
## https://www2.gov.bc.ca/gov/content?id=36D1A7A4BEE248598281824C13CB65B6 
## released under the BC Crown Copyright Licence Agreement (https://www2.gov.bc.ca/gov/content/home/copyright)


# get Esquimalt population numbers
bcstats_mun_2001_2011 <- "http://www.bcstats.gov.bc.ca/Files/0379a32f-cec8-438d-83e0-6724b2a2a272/BCDevelopmentRegionRegionalDistrictandMuncipalPopulationEstimates2001-2011.xls"

bcstats_mun_2011_2017 <- "http://www.bcstats.gov.bc.ca/Files/285cd56c-9be1-4c5e-a153-3deeffa2ac94/BCDevelopmentRegionRegionalDistrictandMuncipalPopulationEstimates2011-2015.xls"

mun_2001_11 <- curl_download(bcstats_mun_2001_2011, destfile = here("R", "bc-sd61-popn", "data", "/bcstats_mun_2001_2011.xls"))
mun_2011_17 <- curl_download(bcstats_mun_2011_2017, destfile = here("R", "bc-sd61-popn", "data", "/bcstats_mun_2011_2017.xls"))

esquimalt_2001_11 <- read_xls(mun_2001_11, skip = 3) %>% 
  filter(SGC == "17040") %>%
  select(-Type) %>% 
  gather(key =  year, value = population_estimate,
         -SGC, -Name) %>% 
  mutate(population_estimate = as.numeric(population_estimate))

esquimalt_popn <- read_xls(mun_2011_17, range = ("A3:J48")) %>% 
  filter(SGC == "17040") %>%
  select(-`Area Type`, -`2011`) %>% 
  gather(key =  year, value = population_estimate,
         -SGC, -Name) %>% 
  bind_rows(esquimalt_2001_11) %>% 
  arrange(year)

#plot of esquimalt population estimates
esquimalt_plot <- ggplot(esquimalt_popn, aes(x = year, y = population_estimate, group = 1)) + 
  geom_line(size = 1, colour = "#54278f") +
  labs(title = "Population Estimates for the Township of Esquimalt, British Columbia",
       caption = "Data sourced from BC Stats Population Estimates webpage (13 October 2018)",
       y = "", x = "") +
  scale_y_continuous(limits = c(16400, 17600), breaks = seq(16400, 17600, 100), expand = c(0,0), labels = comma) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16),
        panel.grid.major.x = element_blank())
plot(esquimalt_plot)
  