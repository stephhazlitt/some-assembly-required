
<!-- 
This file is licensed with the Creative Commons Attribution 4.0 International License.
-->

## When You Miss the Target 🎯

``` r
wind_dir <- data %>% 
  select(station_id, station_name, date, wind_spd, wind_dir) %>% 
  filter(wind_spd != 0) %>% 
  mutate(wind_spd_cat = cut(wind_spd, breaks = c(0,25,50,75),
                            labels = c("1-25 km/h Winds",
                                       "26-50 km/h Winds",
                                       "51-75 km/h Winds")))
wind_dir
```

    ## # A tibble: 210,898 x 6
    ##    station_id station_name      date       wind_spd wind_dir wind_spd_cat  
    ##         <dbl> <chr>             <date>        <dbl>    <dbl> <fct>         
    ##  1         52 ESQUIMALT HARBOUR 1994-02-01       11       36 1-25 km/h Win…
    ##  2         52 ESQUIMALT HARBOUR 1994-02-01       11       35 1-25 km/h Win…
    ##  3         52 ESQUIMALT HARBOUR 1994-02-01       15       35 1-25 km/h Win…
    ##  4         52 ESQUIMALT HARBOUR 1994-02-01        9       36 1-25 km/h Win…
    ##  5         52 ESQUIMALT HARBOUR 1994-02-01        7       36 1-25 km/h Win…
    ##  6         52 ESQUIMALT HARBOUR 1994-02-01        9       36 1-25 km/h Win…
    ##  7         52 ESQUIMALT HARBOUR 1994-02-01       11       36 1-25 km/h Win…
    ##  8         52 ESQUIMALT HARBOUR 1994-02-01        7       36 1-25 km/h Win…
    ##  9         52 ESQUIMALT HARBOUR 1994-02-01        9       35 1-25 km/h Win…
    ## 10         52 ESQUIMALT HARBOUR 1994-02-01        9       36 1-25 km/h Win…
    ## # … with 210,888 more rows

``` r
wind_plot <- wind_dir %>% 
  filter(wind_dir != 0) %>% 
  mutate(wind_dir_deg = wind_dir*10) %>% 
  ggplot(aes(x = date, y = wind_dir_deg)) +
  geom_point(colour = "#3182bd", alpha = .7) +
  facet_wrap(~wind_spd_cat) +
  theme_minimal() +
  labs(x = NULL, y = "wind direction (degrees true)",
       title = "Wind Direction & Average Hourly Wind Speed Measured\nat Esquimalt Harbour (1994-2019)",
       caption = "*Data sourced from Environment Canada using the weathercan R package") +
  scale_y_continuous(limits = c(-5, 365),
                     breaks = seq(0, 360, 60),
                     expand = c(0, 0)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        plot.margin = unit(c(4,35,4,4),"mm"))
wind_plot
```

![](wind_direction_files/figure-gfm/wind_plot-1.png)<!-- -->

``` r
wind_plot_img <- image_read("tmp/wind_dir_plot.png")
fence_img <- image_read("images/fence.jpg")

final_img <- image_composite(image_scale(wind_plot_img, "x800"),
                             image_scale(fence_img, "x100"),
                             offset = "+970+575")
final_img <- image_draw(final_img)
rect(930, 560, 1110, 725, border = "red", lty = "dashed", lwd = 3)
final_img
```

![](wind_direction_files/figure-gfm/final_plot-1.png)<!-- -->

2019-02-26
