library(weathercan)
library(dplyr)
library(ggplot2)

## find the weather station id with weathercan
stn <- stations_search(name = "Esquimalt", interval = "hour") %>% 
  select(station_id) %>% 
  pull()

## get the data for stn with weathercan
data <- weather_dl(station_ids = stn)
# save(data, file = "R/fence/tmp/stn52_data.RData")
# load("R/fence/tmp/stn52_data.RData")

## coord_polar plot of wind direction for wind speeds >50km/h
wind_circle <- data %>% 
  select(station_id, station_name, date, wind_spd, wind_dir) %>% 
  filter(wind_spd > 50) %>%
  mutate(wind_dir_deg = wind_dir*10,
         event = case_when(date == "2019-02-09" ~ "Feb 9th 2019",
                           TRUE ~ "1994 to 2019")) %>% 
  ggplot(aes(x = wind_dir_deg, y = wind_spd, group = event, colour = event)) +
  geom_point(alpha = .7, size = 3, position = "jitter") +
  coord_polar() +
  theme_minimal() +
  scale_colour_manual(name = NULL, values = c("Feb 9th 2019" = "red",
                                              "1994 to 2019" = "#3182bd")) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(360, 90, 180, 270),
                     labels = c("North\n(360)", "East\n(90)", "South\n(180)", "West\n(270)")) +
  scale_y_continuous(limits = c(0, 70)) +
  labs(x = "wind direction (degrees true)",
       y = "wind speed (km/h)",
       title = "Wind Direction for Average Hourly Wind Speeds Above 50km/h\nRecorded at Esquimalt Harbour (1994-2019)",
       caption = "Data sourced from Environment Canada using the\nweathercan R package on February 20th 2019") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.position = c(.99,.9))
wind_circle

## save png for sharing
ggsave(filename = "R/fence/tmp/wind_coord_plot.png",
       plot = wind_circle)

## coord facet plot all wind speeds
wind_circle_facet <- data %>% 
  select(station_id, station_name, year, date, wind_spd, wind_dir) %>% 
  filter(wind_spd != 0) %>%
  mutate(wind_dir_deg = wind_dir*10,
         event = case_when(date == "2019-02-09" ~ "Feb 9th 2019",
                           TRUE ~ "1994 to 2019"),
         wind_spd_cat = cut(wind_spd, breaks = c(0,25,50,75),
                            labels = c("1-25 km/h winds",
                                       "26-50 km/h winds",
                                       "51-75 km/h winds"))) %>% 
  ggplot(aes(x = wind_dir_deg, y = wind_spd, group = event, colour = event)) +
  geom_point(alpha = .7, size = 2, position = "jitter") +
  coord_polar() +
  theme_minimal() +
  facet_wrap(~wind_spd_cat) +
  scale_colour_manual(name = NULL, values = c("Feb 9th 2019" = "red",
                                              "1994 to 2019" = "#3182bd")) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = c(360, 90, 180, 270),
                     labels = c("North\n(360)", "East\n(90)", "South\n(180)", "West\n(270)")) +
  scale_y_continuous(limits = c(0, 70)) +
  labs(x = "wind direction (degrees true)",
       y = "wind speed (km/h)",
       title = "Wind Direction for Average Hourly Wind Speeds\nRecorded at Esquimalt Harbour (1994-2019)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 5),
        strip.text = element_text(size = 7, face = "bold"),
        legend.text = element_text(size = 6))
wind_circle_facet

## save png for sharing
ggsave(filename = "R/fence/tmp/wind_coord_facet_plot.png",
       plot = wind_circle_facet)
