library(tidyverse)
library(rvest) # this does the actual scraping of the data
library(RSelenium) # this is for the automated web browser
library(USAboundaries) # this is just for getting a vector of state names

# Getting a list of all 50 US states
states <- us_states() %>%
  filter(jurisdiction_type == "state") %>%
  select(5)


URL <- "https://www.salary.com/research/salary/benchmark/public-school-teacher-salary/"


#initializing an empty dataset to store scraped data in
dataset <- data.frame()

#open the browser
rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

# A loop for navigating to the salary page for all fifty states
for (v in states$stusps){
  
  url <- paste0(URL, v)

# telling the browser to navigate to the url
remDr$navigate(url)

# this little bit of code determines how many pages of salaries will be read through
num_results <- read_html(url) %>%
  html_nodes("#ResultCount") %>%
  html_text() %>%
  as.numeric()

remainder <- num_results %% 10

if (remainder == 0) {
  num_pages <- num_results %/% 10
} else {
  num_pages <- (num_results %/% 10) + 1
}

# this loop reads through all of the pages of salaries
for (i in c(1:num_pages)){
  
  q <- as.character(i)
  
  # this finds the link to click on and then clicks on it
  webElem <- remDr$findElement(using = "link text", q)
  webElem$clickElement()
  
  # this slows down the process slightly so that the computer doesn't crash a server or get ahead of itself
  Sys.sleep(1.5)
  
  #this is where the data gets scraped from that page
  data <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>%
    html_nodes("table") %>%
    html_table(head = T, fill = TRUE)
  
  table <- data[[1]]
  
  #this is where we add the data to the dataset
  dataset <- bind_rows(dataset, table)
}
}

#Close the browser
remDr$close()

# Cleaning the data
final <- dataset %>%
  select(1:2) %>%
  separate(col = Location, into = c("Location", "City"), sep = "Location") %>%
  separate(col = `Avg. Salary`, into = c("bleep", "Salary"), sep = "Avg. Salary") %>%
  mutate(City = str_trim(City, "both"),
         Salary = str_trim(Salary)) %>%
  separate(Salary, c("bleh", "Salary"), "\\$") %>%
  separate(Salary, c("Sal", "ary"), ",") %>%
  mutate(Salary = as.numeric(paste0(Sal, ary)))  %>%
  separate(City, c("City", "State_Abbr"), ", ") %>%
  select(2,3,8)

# write_csv(final, "salary_by_city.csv")
           