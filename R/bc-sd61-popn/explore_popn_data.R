library(cansim) #get StatsCan data
library(dplyr) #data munging
library(ggplot2) #plotting
library(gghighlight)

## Get Victoria, British Columbia Population Estimates [Table: 17-10-0078-01 (formerly CANSIM  051-0056)] from Statistics Canada
## Data is released under the Statistics Canada Open Licence Agreement 
## https://www.statcan.gc.ca/eng/reference/licence)


bc_pop <- get_cansim(1710007801) %>% 
  filter(GEO == "Victoria, British Columbia",
         Sex == "Both sexes") %>% 
  select(year = REF_DATE, age_group = `Age group`, population_estimate = VALUE)

grownup_ages <- c("15 to 19 years", "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 to 89 years", "90 years and over")

kid_ages <- c("0 to 4 years", "5 to 9 years", "10 to 14 years")

kids <- bc_pop %>% 
  filter(age_group %in% kid_ages)

grownups <- bc_pop %>% 
  filter(age_group %in% grownup_ages)

popn_plot <- ggplot() + 
  geom_line(data = kids, aes(x = year, y = population_estimate,
                             group = age_group, colour = age_group)) +
  #geom_line(data = grownups, aes(x = year, y = population_estimate, group = age_group)) +        
  theme_minimal()
plot(popn_plot)

popn_plot <- ggplot() + 
  geom_line(data = grownups, aes(x = year, y = population_estimate, group = age_group)) + 
  facet_wrap(~ age_group) +
  theme_minimal()
plot(popn_plot)
