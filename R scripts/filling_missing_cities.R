library(tidyverse)

test <- final %>%
  group_by(State) %>%
  summarise(count = n(), cities = n()/366)

missing_state <- final %>%
  filter(State == "Nebraska") %>%
  group_by(City) %>%
  summarise(count = n())

missing_cities <- final %>%
  filter(City %in% c("Oshkosh", "Ragan")) 

title1 <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>%
  html_nodes(".h1_title") %>%
  html_text() %>%
  as.character()

state <- title1

data <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>%
  html_nodes("table.daily_climate_table") %>%
  html_table(head = T, fill = TRUE)

table <- data[[1]] %>%
  mutate(State = state[1])

dataset <- bind_rows(dataset, table)
