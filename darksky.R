# load packages ----------------------------------------------------------------
library(darksky)
library(tidyverse)
library(lubridate)

# set location and time and locale ---------------------------------------------
EDI <- list(lat = 55.9508, lon = -3.3615,  time = "2019-05-15T00:00:00+01:00")

# get forecast -----------------------------------------------------------------
edi_forecast <- get_forecast_for(EDI$lat, EDI$lon, EDI$time, exclude = "daily,currently")

# plot avg apparent temperature throughout day ---------------------------------
scot_blue <- "#0165BF"

edi_forecast$hourly %>% 
  mutate(
    hour = hour(time),
    time_of_day = cut(hour, breaks = 4, labels = c("night", "morning", "afternoon", "evening"))
  ) %>%
  group_by(time_of_day) %>%
  summarise(mean_temp = mean(apparentTemperature)) %>%
  ggplot(aes(x = time_of_day, y = mean_temp)) +
    geom_line(group = 1, colour = scot_blue) + 
    geom_point(colour = scot_blue) +
    labs(x = "", y = "Mean temperature (F)", 
         title = "Apparent temperature",
         subtitle = "15 May 2019 - Edinburgh, UK") +
    scale_y_continuous(limits = c(40, 80),
                       sec.axis = sec_axis(trans = ~(. - 32) * (5/9), 
                                           name = "Mean temperature (C)"))
