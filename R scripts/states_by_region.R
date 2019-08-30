library(tidyverse)
library(rvest)

URL <- "https://www.nationsonline.org/oneworld/US-states-by-area.htm"

data <- read_html(URL) %>%
  html_nodes("#statelist") %>%
  html_table(head = T)

states_by_region <- data[[1]]

write.csv(states_by_region, "C:/Users/ebj800/Documents/Teacher Salaries Project/states_by_region.csv")
