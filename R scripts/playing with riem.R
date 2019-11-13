library(tidyverse)
library(riem)
library(USAboundaries)

states <- us_states() %>%
  filter(jurisdiction_type == "state") %>%
  select(name) %>%
  mutate(name = paste0(name," ASOS"))

networks <- left_join(states, riem_networks())

stations <- tibble()

for (nw in networks$code) {
  temp_data <- riem_stations(nw) %>%
    mutate(code = nw) %>%
    rename(station = name)
  
  stations <- bind_rows(stations, temp_data)
  
}

stations2 <- left_join(stations, networks) %>%
  mutate(state = str_remove(name," ASOS")) %>%
  select(-3, -4, -6) %>%
  rename(station_name = station,
         station = id)

climate <- tibble()
  
for (station in stations$id) {
  
  temp_data <- riem_measures(station, date_start = "2018-11-12", date_end = "2019-11-12") %>%
  filter(is.na(tmpf) == FALSE) %>%
    mutate(station = station)
  
  climate <- bind_rows(climate, temp_data)
}

climate2 <- left_join(climate, stations2)

Maine <- climate2 %>%
  select(valid, lon, lat, tmpf, state) %>%
  mutate(date = lubridate::date(valid)) %>%
  filter(state == "Maine")

maine <- Maine %>%
  group_by(date) %>%
  summarise(temp = median(tmpf))

ggplot(maine) +
  geom_point(aes(x = date, y = temp), color = "skyblue") +
  geom_line(aes(x = date, y = temp), color = "skyblue") +
  theme_classic()
