library(sf)
library(tidyverse)
library(maps)
library(rvest)
library(USAboundaries)

counties <- read_delim("National_Counties.txt", delim = "|")

counties1 <- counties %>%
  filter(FEATURE_CLASS == "Populated Place")

states <- read_csv("salaries_by_city.csv") %>%
  group_by(State_Abbr) %>%
  summarise()

counties1 <- left_join(states, counties1, by = c("State_Abbr" = "STATE_ALPHA"))

salaries <- read_csv("salaries_by_city.csv")

counties2 <- counties1 %>%
  select(1, 3, 6) %>%
  rename(City = FEATURE_NAME, County = COUNTY_NAME)

final <- inner_join(salaries, counties2) %>%
  distinct()


county_salaries <- final %>%
  group_by(County, State_Abbr, State) %>%
  summarise(salary = mean(Salary)) %>%
  separate(County, into = c("County", "bleh"), sep = "\\(CA\\)") %>%
  separate(County, into = c("County", "bleg"), sep = "\\(city\\)") %>%
  select(1, 4:6)


# url <- "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"
# 
# data <- read_html(url) %>%
#   html_nodes("table") %>%
#   html_table(head = T, fill = T)
# 
# table <- data[[2]] %>%
#   separate(`County or equivalent`, into = c("County", "Bleh"), sep = " County")

map_counties <- us_counties() %>%
  rename(County = name, State = state_name)

final2 <- left_join(county_salaries, map_counties)

ggplot() +
  geom_sf(final2, mapping = aes(fill = salary))
