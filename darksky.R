# load packages ----------------------------------------------------------------
library(darksky)
library(tidyverse)
library(lubridate)

# set location and time and locale ---------------------------------------------
RDU <- list(lat = 35.8801, lon = -78.7880, time = "2019-05-15T00:00:00-04:00")

# get forecast -----------------------------------------------------------------
rdu_forecast <- get_forecast_for(RDU$lat, RDU$lon, RDU$time, exclude = "daily,currently")

# plot avg apparent temperature throughout day ---------------------------------
us_blue <- "#002664"

rdu_forecast$hourly %>% 
  mutate(
    hour = hour(time) - 5, # because package converts to local time
    time_of_day = cut(hour, breaks = 4, labels = c("night", "morning", "afternoon", "evening"))
  ) %>%
  group_by(time_of_day) %>%
  summarize(mean_temp = mean(apparentTemperature)) %>%
  ggplot(aes(x = time_of_day, y = mean_temp)) +
    geom_line(group = 1, color = us_blue)  + 
    geom_point(color = us_blue) +
    labs(x = "", y = "Mean temperature (F)", 
         title = "Apparent temperature",
         subtitle = "May 15, 2019 - Durham, NC") +
    scale_y_continuous(limits = c(40, 80),
                       sec.axis = sec_axis(trans = ~(. - 32) * (5/9), 
                                           name = "Mean temperature (C)"))
