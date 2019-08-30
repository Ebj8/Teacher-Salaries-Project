library(rvest)
library(tidyverse)
library(lubridate)

URL <- "https://www.usclimatedata.com/climate/alabama/united-states/3"


dataset <- data.frame()

for (i in c(170:189)) {
  print(paste0("getting data for page: 3", i))
  
  url <- paste0(URL, i)
  
  title1 <- read_html(url) %>%
    html_nodes(".h1_title") %>%
    html_text() %>%
    as.character()
  
  state <- title1
  
  data <- read_html(url) %>%
    html_nodes("table") %>%
    html_table(head = T)
  
  table1 <- data[[1]] %>%
    rename(stat = 1) %>%
    mutate_if(is.numeric, as.character)
  
  
  table2 <- data[[2]] %>%
    select(2:7) %>%
    mutate_if(is.numeric, as.character)
  
  final_table <- bind_cols(table1, table2) %>%
    mutate(State = state)
  
  dataset <- bind_rows(dataset, final_table)
  
  # Sys.sleep(3)
  
}

url <- "https://www.usclimatedata.com/climate/maryland/united-states/1872"

title1 <- read_html(url) %>%
  html_nodes(".h1_title") %>%
  html_text() %>%
  as.character()

state <- title1

data <- read_html(url) %>%
  html_nodes("table") %>%
  html_table(head = T)

table1 <- data[[1]] %>%
  rename(stat = 1) %>%
  mutate_if(is.numeric, as.character)


table2 <- data[[2]] %>%
  select(2:7) %>%
  mutate_if(is.numeric, as.character)

final_table <- bind_cols(table1, table2) %>%
  mutate(State = state)

dataset <- bind_rows(dataset, final_table)

for (i in c(191:195)) {
  print(paste0("getting data for page: 3", i))
  
  url <- paste0(URL, i)
  
  title1 <- read_html(url) %>%
    html_nodes(".h1_title") %>%
    html_text() %>%
    as.character()
  
  state <- title1
  
  data <- read_html(url) %>%
    html_nodes("table") %>%
    html_table(head = T)
  
  table1 <- data[[1]] %>%
    rename(stat = 1) %>%
    mutate_if(is.numeric, as.character)
  
  
  table2 <- data[[2]] %>%
    select(2:7) %>%
    mutate_if(is.numeric, as.character)
  
  final_table <- bind_cols(table1, table2) %>%
    mutate(State = state)
  
  dataset <- bind_rows(dataset, final_table)
  
  # Sys.sleep(3)
  
}

url <- "https://www.usclimatedata.com/climate/montana/united-states/919"

title1 <- read_html(url) %>%
  html_nodes(".h1_title") %>%
  html_text() %>%
  as.character()

state <- title1

data <- read_html(url) %>%
  html_nodes("table") %>%
  html_table(head = T)

table1 <- data[[1]] %>%
  rename(stat = 1) %>%
  mutate_if(is.numeric, as.character)


table2 <- data[[2]] %>%
  select(2:7) %>%
  mutate_if(is.numeric, as.character)

final_table <- bind_cols(table1, table2) %>%
  mutate(State = state)

dataset <- bind_rows(dataset, final_table)

for (i in c(197:220)) {
  print(paste0("getting data for page: 3", i))
  
  url <- paste0(URL, i)
  
  title1 <- read_html(url) %>%
    html_nodes(".h1_title") %>%
    html_text() %>%
    as.character()
  
  state <- title1
  
  data <- read_html(url) %>%
    html_nodes("table") %>%
    html_table(head = T)
  
  table1 <- data[[1]] %>%
    rename(stat = 1) %>%
    mutate_if(is.numeric, as.character)
  
  
  table2 <- data[[2]] %>%
    select(2:7) %>%
    mutate_if(is.numeric, as.character)
  
  final_table <- bind_cols(table1, table2) %>%
    mutate(State = state)
  
  dataset <- bind_rows(dataset, final_table)
  
  # Sys.sleep(3)
  
}

climate_data <- dataset %>%
  separate(State, into = c("State", "City"), sep = " - ") %>%
  separate(State, into = c("trash", "State"), sep = "limate ") %>%
  select(1:13, 15, 16) %>%
  filter(stat != "Average snowfall in :") %>%
  gather(key = "Month", value = "measurement", -stat, -State, -City) %>%
  spread(stat, measurement) %>%
  rename(Avg_Precipitation = `Av. precipitation in :`, 
         Avg_High = `Average high in :`,
         Avg_Low = `Average low in :`,
         Days_with_Precipitation = `Days with precipitation:`,
         Hours_Sunshine = `Hours of sunshine:`) %>%
  mutate_at(vars(c(Avg_Precipitation, Avg_High, Avg_Low,
                   Days_with_Precipitation, Hours_Sunshine)), as.numeric) %>%
  mutate(Month_Order = case_when(
    Month == "Jan" ~ 1,
    Month == "Feb" ~ 2, 
    Month == "Mar" ~ 3,
    Month == "Apr" ~ 4,
    Month == "May" ~ 5,
    Month == "Jun" ~ 6,
    Month == "Jul" ~ 7,
    Month == "Aug" ~ 8,
    Month == "Sep" ~ 9,
    Month == "Oct" ~ 10,
    Month == "Nov" ~ 11,
    Month == "Dec" ~ 12
  ))

write.csv(climate_data, "C:/Users/ebj800/Documents/Teacher Salaries Project/Climate_data.csv")

climate_data_month <- climate_data %>%
  group_by(State, City) %>%
  summarise(max_high = max(Avg_High), min_low = min(Avg_Low), 
            max_precipitation = max(Avg_Precipitation), 
            min_precipitation = min(Avg_Precipitation),
            max_days_with_precipitation = max(Days_with_Precipitation),
            min_days_with_precipitation = min(Days_with_Precipitation),
            max_hours_sunshine = max(Hours_Sunshine),
            min_hours_sunshine = min(Hours_Sunshine))

write.csv(climate_data_month, "C:/Users/ebj800/Documents/Teacher Salaries Project/Climate_data_month.csv")
