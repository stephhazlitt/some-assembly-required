
<!-- 
This file is licensed with the Creative Commons Attribution 4.0 International License.
-->

## Weather with `weathercan` 🇨🇦

So. This happened.

<img src = "images/fence.jpg" width = "500"></img>

😱

``` r
# stns <- stations %>% 
#   filter(prov == "BC") %>% 
#   filter(str_detect(station_name, "\\ESQUIMALT")) %>% 
#   filter(end == max(end, na.rm = TRUE) & interval == "hour")
  
stn <- stations_search(name = "Esquimalt", interval = "hour") %>% 
  select(station_id) %>% 
  pull()
```

``` rawdata

#data <- weather_dl(stn)
```

``` r
# data %>% 
#   select(station_id, station_name, date, wind_spd) %>% 
#   ggplot(aes(x = date, y = wind_spd)) +
#   geom_point(colour = "#3182bd", alpha = .7) +
#   theme_minimal() +
#   labs(x = NULL, y = "wind speed (km/h)") +
# #  geom_hline(yintercept = 50, linetype = 2) +
#   scale_y_continuous(limits = c(0, 70),
#                      breaks = seq(0, 70, 10),
#                      expand = c(0, 0))
# 
# since2017 <- data %>% 
#   select(station_id, station_name, date, wind_spd) %>% 
#   filter(date > "2017-04-01") %>% 
#   ggplot(aes(x = time, y = wind_spd)) +
#   geom_point(colour = "#3182bd", alpha = .7) +
#   theme_minimal() +
#   labs(x = NULL, y = "wind speed (km/h)") +
#   geom_hline(yintercept = 50, linetype = 2) +
#   scale_y_continuous(limits = c(0, 60),
#                      breaks = seq(0, 60, 10),
#                                   expand = c(0, 0))
# 
# wind2019 <- data %>% 
#   select(station_id, station_name, date, wind_spd) %>% 
#   filter(date > "2018-12-10") %>% 
#   ggplot(aes(x = time, y = wind_spd)) +
#   geom_point(colour = "#3182bd", alpha = .7) +
#   theme_minimal() +
#   labs(x = NULL, y = "wind speed (km/h)") +
#   geom_hline(yintercept = 50, linetype = 2) +
#   scale_y_continuous(limits = c(0, 60),
#                      breaks = seq(0, 60, 10),
#                      expand = c(0, 0))
# 
# windFeb19 <- data %>% 
#   select(station_id, station_name, date, time, wind_spd) %>% 
#   filter(time >="2019-02-08 14:00:00" & date < "2019-02-10 00:00:00") %>% 
#   ggplot(aes(x = time, y = wind_spd)) +
#   geom_point(colour = "#3182bd", alpha = .7) +
#   theme_minimal() +
#   labs(x = NULL, y = "wind speed (km/h)") +
#   geom_hline(yintercept = 50, linetype = 2) +
#   scale_y_continuous(limits = c(0, 60),
#                      breaks = seq(0, 60, 10),
#                      expand = c(0, 0))
```

  - Learn more about the amazing `weathercan` package
    [here](http://ropensci.github.io/weathercan/)
  - The R source code for this post can be reviewed
    [here](https://github.com/stephhazlitt/some-assembly-required/blob/master/R/fence/fence.Rmd)
  - `weathercan` is part of the [rOpenSci
    project](https://ropensci.org/)—they are transforming science
    through open data and software 💯

2019-02-23
