library(tidyverse)
library(rvest)
library(RSelenium)

URL <- "https://www.usclimatedata.com/climate/"

dataset <- data.frame()

setwd("~/Teacher Salaries Project")

salaries <- read_csv("salaries_by_city.csv")

states <- salaries %>%
  group_by(State) %>%
  summarise() %>%
  slice(28:50)

rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]


for(s in states$State) {
  
  tryCatch({
    
    url <- paste0(URL, s, "/united-states/")
  remDr$navigate(url)
    
  Sys.sleep(2)
  
  temp_data <- salaries %>%
    filter(State == s)
  
  webElem <- remDr$findElement(using = "link text", s)
  webElem$clickElement()
  }, error = function(e){print("Error in Loop 1")})
  

for(i in temp_data$City) {
  
  tryCatch({
  webElem <- remDr$findElement(using = "link text", i)
  webElem$clickElement()
  
  print(i)
  
months <- seq(41, 338, by = 27)

webElem <- remDr$findElement(using = "link text", "Daily")
webElem$clickElement()
  
for (m in months) {
  
  tryCatch({
  webElem <- remDr$findElement(using = "class", "dropdown")
  webElem$clickElement()
  
  remDr$mouseMoveToLocation(y = m, webElement = webElem)
  remDr$click()
  
  Sys.sleep(2)

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
  }, error = function(e){print("Error in Loop 3")})
}

remDr$goBack()
Sys.sleep(2)

  }, error = function(e){remDr$navigate(url)
    Sys.sleep(2)})
  
}
  
  
}

final <- dataset %>%
  separate(State, into = c("City", "State"), sep = " - ") %>%
  separate(City, into = c("bleh", "City"), sep = "Climate ") %>%
  separate(Day, into = c("Day", "Month"), sep = " ") %>%
  mutate(Day = as.numeric(Day)) %>%
  select(1:8, 10, 11) %>%
  distinct()

write_csv(final, "climate_data2.csv")

test <- salaries %>%
  separate(State, c("State1", "State2"), sep = " ")

